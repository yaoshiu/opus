<script lang="ts">
  import { basicSetup, EditorView } from "codemirror";
  import { keymap, type EditorViewConfig } from "@codemirror/view";
  import { oneDark } from "@codemirror/theme-one-dark";
  import {
    defaultKeymap,
    historyKeymap,
    indentWithTab,
  } from "@codemirror/commands";
  import { foldKeymap } from "@codemirror/language";
  import { searchKeymap } from "@codemirror/search";
  import { closeBracketsKeymap } from "@codemirror/autocomplete";

  const fillHeight = EditorView.theme({
    "&": { height: "100%", width: "100%" },
    ".cm-scroller": { overflow: "auto", "font-family": "inherit" },
  });

  interface Props extends EditorViewConfig {
    view: EditorView | undefined;
  }

  let { view = $bindable(), ...props }: Props = $props();

  function bindView(el: HTMLElement) {
    view = new EditorView({
      parent: el,
      extensions: [
        basicSetup,
        fillHeight,
        oneDark,
        keymap.of([
          indentWithTab,
          ...defaultKeymap,
          ...historyKeymap,
          ...foldKeymap,
          ...searchKeymap,
          ...closeBracketsKeymap,
        ]),
      ],
      ...props,
    });
    return () => view?.destroy();
  }
</script>

<div class="h-full w-full" {@attach bindView}></div>
