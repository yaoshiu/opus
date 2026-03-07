import { defineConfig, type Plugin } from 'vite'
import { svelte } from '@sveltejs/vite-plugin-svelte'
import console from "node:console";
import { join } from "node:path";
import process from "node:process";
import { execSync } from "node:child_process";
import { existsSync, mkdirSync } from "node:fs";
import tailwindcss from "@tailwindcss/vite";

function buildHaskellWasm() {
  console.log("\x1b[36m[Haskell WASM] Building...\x1b[0m");
  try {
    const publicDir = join(process.cwd(), "public");
    if (!existsSync(publicDir)) {
      mkdirSync(publicDir);
    }

    const libDir = join(process.cwd(), "src", "lib");
    if (!existsSync(libDir)) {
      mkdirSync(libDir);
    }

    const rootDir = join(process.cwd(), "..");
    const cabal = "wasm32-wasi-cabal";
    const ghc = "wasm32-wasi-ghc";
    const cmd = `cd ${rootDir} && ${cabal} build wasm && ` +
      `cp "$(${cabal} list-bin opus-wasm)" ${publicDir}/opus.wasm && ` +
      `"$(${ghc} --print-libdir)/post-link.mjs" ` +
      `-i ${publicDir}/opus.wasm -o ${libDir}/opus_exports.js`;

    execSync(cmd, { stdio: "inherit", shell: "/bin/bash" });
    console.log("\x1b[32m[Haskell WASM] Build successful!\x1b[0m");
  } catch (err) {
    console.error("\x1b[31m[Haskell WASM] Build failed:\x1b[0m", err);
  }
}


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


// https://vite.dev/config/
export default defineConfig({
  plugins: [haskellWasmPlugin(), tailwindcss(), svelte()],
  base: "./",
})
