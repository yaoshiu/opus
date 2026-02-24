import {
  closeBracketsKeymap,
  completionKeymap,
} from "@codemirror/autocomplete";
import { defaultKeymap, historyKeymap, indentWithTab } from "@codemirror/commands";
import { foldKeymap } from "@codemirror/language";
import { searchKeymap } from "@codemirror/search";
import { oneDark } from "@codemirror/theme-one-dark";
import { keymap } from "@codemirror/view";
import { basicSetup, EditorView } from "codemirror";
import { createSignal, onCleanup, onMount } from "solid-js";

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
              indentWithTab,
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
