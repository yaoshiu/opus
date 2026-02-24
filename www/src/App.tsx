import { Circle, Play, Plus, Save, X } from "lucide-solid";
import {
  batch,
  createMemo,
  createSignal,
  For,
  onCleanup,
  onMount,
  type ParentProps,
  Show,
  type Signal,
} from "solid-js";
import color from "tailwindcss/colors";
import { createEditor } from "./components/Editor.tsx";
import { createTerminal } from "./components/Terminal.tsx";
import { type FileTree, Opus } from "./opus.ts";
import Readline from "./readline.ts";
import wasmUrl from "/opus.wasm?url";

const ARGS = ["opus.wasm"];
const PROMPT = "opus> ";
const INITIALIZED = "Opus initialized!";
const REPL = "<repl>";
const DEFAULTPATH = "untitled.op";
const CODE = [{
  path: "hello.op",
  content: `($begin
  ($define! $mod ($import mod.op))
  ($define! a "Goodbye, world!")
  ; You can try '($mod a)' in the REPL without running 'mod.op' to see if a is visible in '$mod'
  (display! (($mod b)))
)`,
}, {
  path: "mod.op",
  content: `($begin
  ($define! a "Hello, world!")
  ($define! b ($lambda () a))
  ($export b))`,
}];

interface Filetab {
  path: Signal<string>;
  editor: ReturnType<typeof createEditor>;
  content: Signal<string>;
}

function toFileTree(path: string, content: string | null): FileTree {
  return path.split("/").reduceRight<FileTree | string | null>(
    (acc, key) => ({ [key]: acc }),
    content,
  ) as FileTree;
}

function ToolButton(
  { onClick, children }: ParentProps<{ onClick: () => void }>,
) {
  return (
    <button
      type="button"
      class="hover:text-white transition-colors flex text-sm items-center gap-1 rounded-md"
      onClick={onClick}
    >
      {children}
    </button>
  );
}

function App() {
  const [term, Terminal] = createTerminal({
    fontFamily: "Source Code Pro",
    theme: { background: color.zinc[900] },
  });
  const [tabs, setTabs] = createSignal<Filetab[]>(
    CODE.map(({ path, content }) => ({
      path: createSignal(path),
      editor: createEditor(content),
      content: createSignal(content),
    })),
  );
  const [tabIdx, setTabIdx] = createSignal(0);
  const [editingIdx, setEditingIdx] = createSignal<number | null>(null);
  const [editingPath, setEditingPath] = createSignal<string>("");
  const current = createMemo(() => tabs()[tabIdx()]);

  let opus: Opus | undefined;

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
        opus.addContent(toFileTree(tab.path[0](), tab.content[0]()));
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

  function onStdout(line: string) {
    term.writeln(line);
  }

  function onStderr(line: string) {
    term.writeln(line);
  }

  const saveCode = () => {
    const { path: [path], content: [, setContent], editor: [view] } = current();
    const content = view()?.state.doc.toString();
    if (content !== undefined) {
      setContent(content);
      opus?.addContent(toFileTree(path(), content));
    }
  };

  async function runCode() {
    const content = current().content[0]();
    term.write("\r\n");
    await onLine(content, "", true);
    readline.clear();
    readline.redraw();
  }

  function deleteTab(idx: number) {
    const tab = tabs()[idx];
    opus?.deleteContent(toFileTree(tab.path[0](), null));
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

  function addTab() {
    const tab: Filetab = {
      path: createSignal(""),
      editor: createEditor(),
      content: createSignal(""),
    };
    const newIdx = tabs().length;

    batch(() => {
      setTabs((tabs) => [...tabs, tab]);
      setTabIdx(newIdx);
      setEditingIdx(newIdx);
      setEditingPath(DEFAULTPATH);
    });
  }

  function renameTab() {
    const final = editingPath() || DEFAULTPATH;
    const idx = editingIdx();
    if (idx !== null) {
      batch(() => {
        tabs().at(idx)?.path[1]?.(final);
        setEditingIdx(null);
        setEditingPath("");
      });
    }
  }

  onMount(() => {
    term.loadAddon(readline);
  });

  onCleanup(() => {
    opus?.dispose();
  });

  return (
    <div class="h-screen w-screen bg-zinc-900 text-white/75">
      <div class="h-12 px-4 w-full flex justify-between items-center bg-zinc-900 border border-white/5">
        <div class="font-[Orbitron] font-bold text-lg">
          Opus
        </div>
        <div class="flex gap-4">
          <ToolButton onClick={saveCode}>
            <Save size={12} />Save
          </ToolButton>
          <ToolButton onClick={runCode}>
            <Play size={12} />Run
          </ToolButton>
        </div>
      </div>
      <div class="h-full w-full grid md:grid-cols-2 md:grid-rows-1 grid-rows-2 overflow-hidden font-mono">
        <div class="h-full min-w-0 overflow-hidden border-white/5">
          <div class="h-8 flex bg-zinc-900 shadow-md border-y-white/5">
            <For each={tabs()}>
              {({ path: [path] }, idx) => (
                <button
                  type="button"
                  class="px-2 relative border-x border-white/5 transition-colors group grid grid-cols-[1fr_auto_1fr] gap-1 h-full items-center text-xs"
                  classList={{ "text-white": idx() === tabIdx() }}
                  onClick={() => {
                    setTabIdx(idx());
                  }}
                >
                  <Circle
                    size={8}
                    fill={idx() === tabIdx() ? "currentColor" : undefined}
                  />
                  <Show
                    when={editingIdx() === idx()}
                    fallback={
                      <span
                        onDblClick={() => {
                          setEditingIdx(idx());
                          setEditingPath(path());
                        }}
                      >
                        {path()}
                      </span>
                    }
                  >
                    <input
                      type="text"
                      ref={(e) => {
                        setTimeout(() => e.focus(), 0);
                      }}
                      value={editingPath()}
                      onInput={(e) => setEditingPath(e.target.value)}
                      onBlur={renameTab}
                      onKeyDown={(e) => {
                        if (e.key === "Enter") {
                          renameTab();
                        } else if (e.key === "Escape") {
                          setEditingIdx(null);
                        }
                      }}
                      placeholder={DEFAULTPATH}
                      onClick={(e) => e.stopPropagation()}
                      class="border-none outline-none w-20"
                    />
                  </Show>
                  <X
                    size={12}
                    class="group-hover:visible md:invisible transition-all justify-self-end"
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
              <Plus size={12} onClick={addTab} />
            </button>
          </div>
          <For each={tabs()}>
            {({ editor: [, Editor] }, idx) => (
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
