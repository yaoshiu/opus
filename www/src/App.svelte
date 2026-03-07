<script lang="ts">
  import { Circle, Play, Plus, Save, X } from "lucide-svelte";

  import Editor from "./lib/Editor.svelte";
  import Terminal from "./lib/Terminal.svelte";
  import { type FileTree, Opus } from "./lib/opus";
  import Readline from "./lib/readline";
  import wasmUrl from "/opus.wasm?url";
  import type { EditorView } from "codemirror";
  import type { Terminal as XTerm } from "@xterm/xterm";
  import { onDestroy } from "svelte";

  const ARGS = ["opus.wasm"];
  const PROMPT = "opus> ";
  const INITIALIZED = "Opus initialized!";
  const REPL = "<repl>";
  const DEFAULTPATH = "untitled.op";
  const CODES = [
    {
      path: "hello.op",
      content: `($begin
  ($define! $mod ($import mod.op))
  ($define! a "Goodbye, world!")
  ; You can try '($mod a)' in the REPL without running 'mod.op' to see if a is visible in '$mod'
  (display! (($mod b)))
)`,
    },
    {
      path: "mod.op",
      content: `($begin
  ($define! a "Hello, world!")
  ($define! b ($lambda () a))
  ($export b))`,
    },
  ];

  interface Filetab {
    path: string;
    content: string;
  }

  function toFileTree(path: string, content: string | null): FileTree {
    return path
      .split("/")
      .reduceRight<
        FileTree | string | null
      >((acc, key) => ({ [key]: acc }), content) as FileTree;
  }

  let term: XTerm | undefined;
  let tabs = $state<Filetab[]>(CODES);
  let tabIdx = $state(0);
  let editingIdx = $state<number | null>(null);
  let editingPath = $state("");
  let current = $derived(tabs[tabIdx]);
  let views = new Map<string, EditorView | undefined>(
    CODES.map(({ path }) => [path, undefined]),
  );

  let opus: Opus | undefined;

  function onStdout(line: string) {
    term?.writeln(line);
  }

  function onStderr(line: string) {
    term?.writeln(line);
  }

  async function handleLine(
    line: string | null,
    source = REPL,
    ignoreRes = false,
  ) {
    if (!opus) {
      opus = await Opus.init({ url: wasmUrl, args: ARGS, onStdout, onStderr });
      for (const { path, content } of tabs) {
        opus.addContent(toFileTree(path, content));
      }
      term?.writeln(INITIALIZED);
    }

    if (line === null) {
      return true;
    }

    const res = await opus.eval(line, source);
    switch (res.type) {
      case "success": {
        const val = await res.expr.render(true);
        await res.expr.dispose();
        if (ignoreRes) {
          return true;
        }
        term?.writeln(val.replace(/\r?\n/g, "\r\n"));
        return true;
      }
      case "error":
        term?.writeln(res.message.replace(/\r?\n/g, "\r\n"));
        return true;
      case "incomplete":
        return false;
    }
  }

  const readline = new Readline(PROMPT, handleLine);

  function saveCode() {
    let { path, content } = current;
    const view = views.get(path);
    const code = view?.state.doc.toString();
    if (code !== undefined) {
      content = code;
      opus?.addContent(toFileTree(path, content));
    }
  }

  async function runCode() {
    const { content } = current;
    term?.write("\r\n");
    await handleLine(content, "", true);
    readline.clear();
    readline.redraw();
  }

  function deleteTab(idx: number) {
    const tab = tabs[idx];
    opus?.deleteContent(toFileTree(tab.path, null));
    const currentIdx = tabIdx;
    let newIdx = currentIdx;
    if (idx < currentIdx) {
      newIdx = currentIdx - 1;
    } else if (idx === currentIdx) {
      if (idx >= tabs.length - 1) {
        newIdx = Math.max(0, idx - 1);
      }
    }

    tabs.splice(idx, 1);
    tabIdx = newIdx;
  }

  function addTab() {
    const tab: Filetab = {
      path: "",
      content: "",
    };
    const newIdx = tabs.length;

    tabs.push(tab);
    tabIdx = newIdx;
    editingIdx = newIdx;
    editingPath = "";
  }

  function renameTab() {
    const final = editingPath || DEFAULTPATH;
    const idx = editingIdx;
    if (idx !== null) {
      tabs[idx].path = final;
      editingIdx = null;
      editingPath = "";
    }
  }

  onDestroy(() => opus?.dispose());
</script>

<div class="h-screen w-screen bg-zinc-900 text-white/75">
  <div
    class="h-12 px-4 w-full flex justify-between items-center bg-zinc-900 border border-white/5"
  >
    <div class="font-[Orbitron] font-bold text-lg">Opus</div>
    <div class="flex gap-4">
      <svelte:boundary>
        {@const common = {
          type: "button" as const,
          class:
            "hover:text-white transition-colors flex text-sm items-center gap-1 rounded-md",
        }}
        <button {...common} onclick={saveCode}><Save size={12} /> Save</button>
        <button {...common} onclick={runCode}><Play size={12} /> Run</button>
      </svelte:boundary>
    </div>
  </div>
  <div
    class="h-full w-full grid md:grid-cols-2 md:grid-rows-1 grid-rows-2 overflow-hidden font-mono"
  >
    <div class="h-full min-w-0 overflow-hidden border-white/5">
      <div class="h-8 flex bg-zinc-900 shadow-md border-y-white/5">
        {#each tabs as { path }, idx (path)}
          <button
            type="button"
            class={[
              idx === tabIdx && "text-white",
              "px-2 relative border-x border-white/5 transition-colors group grid grid-cols-[1fr_auto_1fr] gap-1 h-full items-center text-xs",
            ]}
            onclick={() => {
              tabIdx = idx;
            }}
          >
            <Circle
              size={8}
              fill={idx === tabIdx ? "currentColor" : undefined}
            />
            {#if editingIdx === idx}
              <input
                type="text"
                {@attach (el) => el.focus()}
                bind:value={editingPath}
                onblur={renameTab}
                onkeydown={(e) => {
                  if (e.key === "Enter") {
                    renameTab();
                  } else if (e.key === "Escape") {
                    editingIdx = null;
                  }
                }}
                placeholder={DEFAULTPATH}
                onclick={(e) => e.stopPropagation()}
                class="border-none outline-none w-20"
              />
            {:else}
              <span
                role="button"
                tabindex="0"
                ondblclick={() => {
                  editingIdx = idx;
                  editingPath = path;
                }}
              >
                {path}
              </span>
            {/if}
            <X
              size={12}
              class="group-hover:visible md:invisible transition-all justify-self-end"
              onclick={(e) => {
                e.stopPropagation();
                deleteTab(idx);
              }}
            />
          </button>
        {/each}
        <button type="button" class="px-2 flex items-center hover:text-white">
          <Plus size={12} onclick={addTab} />
        </button>
      </div>
      {#each tabs as { path, content }, idx}
        <div class={[idx !== tabIdx && "hidden", "h-full"]}>
          <Editor
            bind:view={() => views.get(path), (view) => views.set(path, view)}
            doc={content}
          />
        </div>
      {/each}
    </div>
    <div class="min-w-0 overflow-hidden p-2">
      <Terminal
        bind:term={
          () => term,
          (new_term) => {
            term = new_term;
            term?.loadAddon(readline);
          }
        }
      />
    </div>
  </div>
</div>
