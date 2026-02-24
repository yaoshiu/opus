import {
  ConsoleStdout,
  Directory,
  File,
  Inode,
  OpenFile,
  PreopenDirectory,
  WASI,
} from "@bjorn3/browser_wasi_shim";
import opus_exports from "./opus_exports.js";

declare const __phantom: unique symbol;
type Ptr<A> = number & {
  readonly [__phantom]: A;
};
type Env = "Env";
type SExpr = "SExpr";

interface OpusWasmExports {
  memory: WebAssembly.Memory;

  _initialize: () => unknown;
  hs_init(arg0: number, arg1: number): Promise<void>;

  init_env(): Promise<Ptr<Env>>;
  hs_free<T>(ptr: Ptr<T>): Promise<void>;
  eval(
    env: Ptr<Env>,
    input: string,
    source: string,
    onSucc: (val: Ptr<SExpr>) => void,
    onIncomp: () => void,
    onErr: (msg: string) => void,
  ): Promise<void>;
  render(val: Ptr<SExpr>, quote: boolean): Promise<string>;
}

type OpusWasm = WebAssembly.Instance & { exports: OpusWasmExports };

interface Fat<T> {
  ptr: Ptr<T>;
  freed: boolean;
}

// [NOTE]: It's safe to render/free a SExpr even after freeing the env,
// as long as the Haskell heap is still alive
export class BoxedSExpr {
  private reg: FinalizationRegistry<Fat<SExpr>>;
  private exports: OpusWasmExports;
  private fat: Fat<SExpr>;
  private token = {};

  constructor(
    ptr: Ptr<SExpr>,
    exports: OpusWasmExports,
    reg: FinalizationRegistry<Fat<SExpr>>,
  ) {
    this.exports = exports;
    this.fat = { ptr: ptr, freed: false };
    this.reg = reg;
    this.reg.register(this, this.fat, this.token);
  }

  render(quote: boolean) {
    if (this.fat.freed) {
      throw new Error("BoxedExpr has been disposed!");
    }
    return this.exports.render(this.fat.ptr, quote);
  }

  async dispose() {
    if (this.fat.freed) {
      console.warn("SExpr double free");
      return;
    }
    this.fat.freed = true;
    this.reg.unregister(this.token);
    await this.exports.hs_free(this.fat.ptr);
  }

  async [Symbol.asyncDispose]() {
    await this.dispose();
  }
}

export type FileTree = { [name: string]: FileTree | string | null };

function buildInode(value: FileTree | string | null): Inode | null {
  if (typeof value === "string") {
    return new File(new TextEncoder().encode(value));
  } else if (value === null) {
    return null;
  } else {
    const contents = new Map<string, Inode>();
    for (const [name, child] of Object.entries(value)) {
      const inode = buildInode(child);
      if (inode) {
        contents.set(name, inode);
      } else {
        contents.delete(name);
      }
    }
    return new Directory(contents);
  }
}

function deleteInode(parent: Map<string, Inode>, tree: FileTree) {
  for (const [name, subtree] of Object.entries(tree)) {
    if (typeof subtree === "string") {
      parent.delete(name);
    } else {
      const child = parent.get(name);
      if (
        child && "contents" in child && child.contents instanceof Map && subtree
      ) {
        deleteInode(child.contents, subtree);
      }
    }
  }
}

export interface OpusConfig {
  url: string;
  args: string[];
  onStdout: (line: string) => void;
  onStderr: (line: string) => void;
  initRoot?: FileTree;
  initEnv?: string[];
}

export type EvalResult =
  | { type: "success"; expr: BoxedSExpr }
  | { type: "incomplete" }
  | { type: "error"; message: string };

export class Opus {
  private exports: OpusWasmExports;
  private env: Fat<Env>;
  private contents;
  private reg: FinalizationRegistry<Fat<unknown>>;
  private token = {};

  static async init({
    url,
    args,
    onStdout,
    onStderr,
    initRoot = {},
    initEnv = [],
  }: OpusConfig) {
    const contents = new Map<string, Inode>();
    for (const [name, child] of Object.entries(initRoot)) {
      const inode = buildInode(child);
      if (inode) {
        contents.set(name, inode);
      }
    }
    const fds = [
      new OpenFile(new File([])),
      ConsoleStdout.lineBuffered(onStdout),
      ConsoleStdout.lineBuffered(onStderr),
      new PreopenDirectory("/", contents),
    ];
    const wasi = new WASI(args, initEnv, fds);

    const response = await fetch(url);
    const wasmModule = await WebAssembly.compileStreaming(response);

    const __exports = {};
    const instance = (await WebAssembly.instantiate(wasmModule, {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: opus_exports(__exports),
    })) as OpusWasm;
    Object.assign(__exports, instance.exports);

    wasi.initialize(instance);

    await instance.exports.hs_init(0, 0);

    const env = await instance.exports.init_env();

    // [NOTE]: contents is shared by reference with the PreopenDirectory
    // WASI reads it lazily
    return new Opus(instance.exports, env, contents);
  }
  private constructor(
    exports: OpusWasmExports,
    env: Ptr<Env>,
    contents: Map<string, Inode>,
  ) {
    this.exports = exports;
    this.env = { ptr: env, freed: false };
    this.contents = contents;
    this.reg = new FinalizationRegistry((fat) => {
      if (fat.freed) {
        console.warn("Env double free!");
        return;
      }
      fat.freed = true;
      // [NOTE]: Fire and forget
      void exports.hs_free(fat.ptr);
    });
    this.reg.register(this, this.env, this.token);
  }

  eval(input: string, source: string): Promise<EvalResult> {
    return new Promise((resolve, reject) => {
      if (this.env.freed) {
        reject("Env disposed!");
        return;
      }
      let called = false;
      const once = <F extends (...args: never[]) => unknown>(func: F) => {
        return (...args: Parameters<F>) => {
          if (called) {
            return;
          }
          called = true;
          return func(...args);
        };
      };
      this.exports.eval(
        this.env.ptr,
        input,
        source,
        once((ptr: Ptr<SExpr>) =>
          resolve({
            type: "success",
            expr: new BoxedSExpr(ptr, this.exports, this.reg),
          })
        ),
        once(() => resolve({ type: "incomplete" })),
        once((msg: string) => resolve({ type: "error", message: msg })),
      ).catch(once(reject));
    });
  }

  // [NOTE]: Hot Update
  addContent(tree: FileTree) {
    for (const [name, value] of Object.entries(tree)) {
      const inode = buildInode(value);
      if (inode) {
        this.contents.set(name, inode);
      }
    }
  }

  deleteContent(tree: FileTree) {
    deleteInode(this.contents, tree);
  }

  async dispose() {
    if (this.env.freed) {
      console.warn("Env double free!");
      return;
    }
    this.reg.unregister(this.token);
    const env = this.env;
    this.env.freed = true;
    await this.exports.hs_free(env.ptr);
  }

  async [Symbol.asyncDispose]() {
    await this.dispose();
  }
}
