import { createTerminal } from "./components/Terminal.tsx";
import { type FileTree, Opus } from "./opus.ts";
import {
  batch,
  createMemo,
  createSignal,
  For,
  onCleanup,
  onMount,
  type Signal,
} from "solid-js";
import wasmUrl from "/opus.wasm?url";
import Readline from "./readline.ts";
import { createEditor } from "./components/Editor.tsx";
import { Play, Plus, Save, X } from "lucide-solid";
import color from "tailwindcss/colors";

const ARGS = ["opus.wasm"];
const PROMPT = "opus> ";
const INITIALIZED = "Opus initialized!";
const REPL = "<repl>";
const CODE = [
  `($begin
  ($define! $mod ($import mod.op))
  ; You can try '($mod a)' in the REPL without running 'mod.op' to see if a is visible in '$mod'
  (display! (($mod b)))
)`,
  `($begin
  ($define! a "Hello, world!")
  ($define! b ($lambda () a))
  ($export b))`,
];

interface Filetab {
  path: string;
  editor: ReturnType<typeof createEditor>;
  doc: Signal<string>;
}

function toFileTree(path: string, content: string | null): FileTree {
  return path.split("/").reduceRight<FileTree | string | null>(
    (acc, key) => ({ [key]: acc }),
    content,
  ) as FileTree;
}

function App() {
  const [term, Terminal] = createTerminal({
    fontFamily: "Source Code Pro",
    theme: { background: color.zinc[900] },
  });
  const [tabs, setTabs] = createSignal<Filetab[]>([{
    path: "hello.op",
    editor: createEditor(CODE[0]),
    doc: createSignal(CODE[0]),
  }, {
    path: "mod.op",
    editor: createEditor(CODE[1]),
    doc: createSignal(CODE[1]),
  }]);
  const [tabIdx, setTabIdx] = createSignal(0);
  const current = createMemo(() => tabs()[tabIdx()]);

  let opus: Opus;

  async function onLine(
    line: string | null,
    source = REPL,
    ignoreRes: boolean = false,
  ) {
    if (!opus) {
      opus = await Opus.init({
        url: wasmUrl,
        args: ARGS,
        onStdout,
        onStderr,
      });
      for (const tab of tabs()) {
        opus.addContent(toFileTree(tab.path, tab.doc[0]()));
      }
      term.writeln(INITIALIZED);
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
        term.writeln(val.replace(/\r?\n/g, "\r\n"));
        return true;
      }
      case "error":
        term.writeln(res.message.replace(/\r?\n/g, "\r\n"));
        return true;
      case "incomplete":
        return false;
    }
  }

  const readline = new Readline(PROMPT, onLine);

  onMount(() => {
    term.loadAddon(readline);
  });

  function onStdout(line: string) {
    term.writeln(line);
  }

  function onStderr(line: string) {
    term.writeln(line);
  }

  const saveCode = () => {
    const { path, doc: [_, setDoc], editor: [view] } = current();
    const doc = view()?.state.doc.toString();
    if (doc) {
      setDoc(doc);
      opus?.addContent(toFileTree(path, doc));
    }
  };

  async function runCode() {
    const doc = current().doc[0]();
    term.write("\r\n");
    await onLine(doc, "", true);
    readline.clear();
    readline.redraw();
  }

  function deleteTab(idx: number) {
    const tab = tabs()[idx];
    opus.addContent(toFileTree(tab.path, null));
    const currentIdx = tabIdx();
    let newIdx = currentIdx;
    if (idx < currentIdx) {
      newIdx = currentIdx - 1;
    } else if (idx === currentIdx) {
      if (idx >= tabs().length - 1) {
        newIdx = Math.max(0, idx - 1);
      }
    }
    batch(() => {
      setTabs(tabs().filter((_, i) => i !== idx));
      setTabIdx(newIdx);
    });
  }

  onCleanup(() => {
    opus?.dispose();
  });

  return (
    <div class="h-screen w-screen bg-zinc-900 text-white/85">
      <div class="h-12 px-4 w-full flex justify-between items-center bg-zinc-900 border border-white/5">
        <div class="font-[Orbitron] font-bold text-lg">
          Opus
        </div>
        <div class="flex gap-4">
          <For
            each={[{
              onClick: saveCode,
              children: (
                <>
                  <Save size={12} />
                  Save
                </>
              ),
            }, {
              onClick: runCode,
              children: (
                <>
                  <Play size={12} /> Run
                </>
              ),
            }]}
          >
            {({ onClick, children }) => (
              <button
                type="button"
                class="hover:text-white flex text-sm items-center gap-1 rounded-md"
                onClick={onClick}
              >
                {children}
              </button>
            )}
          </For>
        </div>
      </div>
      <div class="h-full w-full grid grid-cols-2 overflow-hidden font-mono">
        <div class="h-full min-w-0 overflow-hidden border-white/5">
          <div class="h-8 flex bg-zinc-900 shadow-md border-y-white/5">
            <For each={tabs()}>
              {(tab, idx) => (
                <button
                  type="button"
                  class="px-2 relative border-x border-white/5 group grid grid-cols-[1fr_auto_1fr] gap-1 h-full items-center text-xs"
                  classList={{ "text-white": idx() === tabIdx() }}
                  onClick={() => {
                    setTabIdx(idx());
                  }}
                >
                  <div />
                  {tab.path}{" "}
                  <X
                    size={8}
                    class="group-hover:visible invisible transition-all justify-self-end"
                    onClick={(e) => {
                      e.stopPropagation();
                      deleteTab(idx());
                    }}
                  />
                </button>
              )}
            </For>
            <button
              type="button"
              class="px-2 flex items-center hover:text-white"
            >
              <Plus size={12} />
            </button>
          </div>
          <For each={tabs()}>
            {({ editor: [_, Editor] }, idx) => (
              <div class="h-full" classList={{ hidden: idx() !== tabIdx() }}>
                <Editor />
              </div>
            )}
          </For>
        </div>
        <div class="h-full min-w-0 overflow-hidden p-2">
          <Terminal />
        </div>
      </div>
    </div>
  );
}

export default App;
