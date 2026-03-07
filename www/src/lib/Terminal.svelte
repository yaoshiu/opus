<script lang="ts">
  import {
    type ITerminalOptions,
    type ITerminalInitOnlyOptions,
    Terminal,
  } from "@xterm/xterm";
  import "@xterm/xterm/css/xterm.css";
  import { FitAddon } from "@xterm/addon-fit";
  import { WebglAddon } from "@xterm/addon-webgl";

  interface Props {
    term: Terminal | undefined;
    options?: ITerminalOptions & ITerminalInitOnlyOptions;
  }

  let { term = $bindable(), options }: Props = $props();

  function bindTerm(el: HTMLElement) {
    const instance = new Terminal(options);
    const fitAddon = new FitAddon();
    instance.loadAddon(fitAddon);
    instance.open(el);

    try {
      const webglAddon = new WebglAddon();
      instance.loadAddon(webglAddon);
    } catch (e) {
      console.error("WebGL failed", e);
    }

    fitAddon.fit();
    term = instance;

    const handleResize = () => fitAddon.fit();
    globalThis.addEventListener("resize", handleResize);

    return () => {
      globalThis.removeEventListener("resize", handleResize);
      instance.dispose();
      term = undefined;
    };
  }
</script>

<div class="h-full w-full" {@attach bindTerm}></div>
