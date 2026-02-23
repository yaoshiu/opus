import { createSignal, mergeProps, onCleanup, onMount } from "solid-js";
import { foldKeymap } from "@codemirror/language";
import { defaultKeymap, historyKeymap } from "@codemirror/commands";
import { basicSetup, EditorView } from "codemirror";
import {
  closeBracketsKeymap,
  completionKeymap,
} from "@codemirror/autocomplete";
import { searchKeymap } from "@codemirror/search";
import { keymap } from "@codemirror/view";
import { oneDark } from "@codemirror/theme-one-dark";

const fillHeight = EditorView.theme({
  "&": { height: "100%", width: "100%" },
  ".cm-scroller": { overflow: "auto", "font-family": "inherit" },
});

export function createEditor(doc?: string) {
  const [view, setView] = createSignal<EditorView>();
  function Editor(
  ) {
    let self!: HTMLDivElement;
    onMount(() => {
      setView(
        new EditorView({
          doc,
          parent: self,
          extensions: [
            basicSetup,
            fillHeight,
            oneDark,
            keymap.of([
              ...closeBracketsKeymap,
              ...defaultKeymap,
              ...searchKeymap,
              ...historyKeymap,
              ...foldKeymap,
              ...completionKeymap,
            ]),
          ],
        }),
      );
    });

    onCleanup(() => {
      view()?.destroy();
    });
    return <div class="h-full w-full" ref={self} />;
  }
  return [view, Editor] as const;
}
