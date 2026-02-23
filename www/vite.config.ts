import console from "node:console";
import { join } from "node:path";
import { defineConfig, type Plugin } from "vite";
import solid from "vite-plugin-solid";
import process from "node:process";
import { existsSync, mkdirSync } from "node:fs";
import { execSync } from "node:child_process";
import tailwindcss from "@tailwindcss/vite";

const buildHaskellWasm = () => {
  console.log("\x1b[36m[Haskell WASM] Building...\x1b[0m");
  try {
    const publicDir = join(process.cwd(), "public");
    if (!existsSync(publicDir)) {
      mkdirSync(publicDir);
    }

    const rootDir = join(process.cwd(), "..");
    const cabal = "wasm32-wasi-cabal";
    const ghc = "wasm32-wasi-ghc";
    const cmd = `cd ${rootDir} && ${cabal} build wasm && ` +
      `cp "$(${cabal} list-bin opus-wasm)" www/public/opus.wasm && ` +
      `"$(${ghc} --print-libdir)/post-link.mjs" ` +
      "-i www/public/opus.wasm -o www/src/opus_exports.js";

    execSync(cmd, { stdio: "inherit", shell: "/bin/bash" });
    console.log("\x1b[32m[Haskell WASM] Build successful!\x1b[0m");
  } catch (err) {
    console.error("\x1b[31m[Haskell WASM] Build failed:\x1b[0m", err);
  }
};

function haskellWasmPlugin(): Plugin {
  let isBuilding = false;

  return {
    name: "haskell-wasm-plugin",

    configureServer(server) {
      const rootDir = join(process.cwd(), "..");
      const globs = [
        "core/**/*.hs",
        "wasm/**/*.hs",
        "core/*.cabal",
        "wasm/*.cabal",
        "core/data/*.op",
      ];
      server.watcher.add(globs.map((glob) => join(rootDir, glob)));
    },

    buildStart() {
      if (isBuilding) return;
      isBuilding = true;
      buildHaskellWasm();
      isBuilding = false;
    },

    handleHotUpdate({ file, server }) {
      const isHaskell = file.endsWith(".hs");
      const isCabal = file.endsWith(".cabal");
      const isPrelude = file.endsWith(".op");

      if (isHaskell || isCabal || isPrelude) {
        if (isBuilding) return;
        isBuilding = true;
        buildHaskellWasm();
        isBuilding = false;
        server.ws.send({ type: "full-reload" });
      }
    },
  };
}

export default defineConfig({
  plugins: [haskellWasmPlugin(), tailwindcss(), solid()],
});
