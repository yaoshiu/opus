import { onCleanup, onMount } from "solid-js";
import {
  type ITerminalInitOnlyOptions,
  type ITerminalOptions,
  Terminal as XTerm,
} from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { WebglAddon } from "@xterm/addon-webgl";
import "@xterm/xterm/css/xterm.css";
import _ from "lodash";

export function createTerminal(
  init?: ITerminalInitOnlyOptions & ITerminalOptions,
) {
  const term = new XTerm(init);
  const fitAddon = new FitAddon();
  term.loadAddon(fitAddon);

  function Terminal() {
    let self!: HTMLDivElement;

    onMount(() => {
      term.open(self);
      const webglAddon = new WebglAddon();
      term.loadAddon(webglAddon);

      webglAddon.onContextLoss(() => {
        webglAddon.dispose();
      });

      fitAddon.fit();

      const handleResize = () => fitAddon.fit();
      globalThis.addEventListener("resize", handleResize);

      onCleanup(() => {
        term.dispose();
        globalThis.removeEventListener("resize", handleResize);
        self.replaceChildren();
      });
    });

    return <div class="h-full w-full" ref={self} />;
  }
  return [term, Terminal] as const;
}
