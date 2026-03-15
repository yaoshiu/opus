<div align="center">
  <h1>Opus 🎼</h1>
  <p><b>A minimal, statically-scoped Lisp dialect based on f-expressions.</b></p>

  [![Try Web REPL](https://img.shields.io/badge/Playground-Live_Web_REPL-2ea44f?style=for-the-badge)](https://yaoshiu.github.io/opus)
  [![Build Status](https://img.shields.io/badge/build-passing-brightgreen?style=flat-square)]()
  [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg?style=flat-square)](https://github.com/yaoshiu/opus/blob/master/LICENSE)
</div>

Opus is a minimal, statically-scoped Lisp dialect based on the semantics of f-expressions (the Kernel language). It is implemented in Haskell, utilizes Continuation-Passing Style (CPS) for control flow, and compiles to WebAssembly.

## Abstract & Philosophy

The primary design goal of Opus is to minimize the language's trusted computing base (Special Forms) while maximizing its expressive power through orthogonal abstraction.

Unlike traditional Lisps that separate functions (applicatives) and macros, Opus unifies them into a single primitive: the **operative**  (`$vau`). Furthermore, environments are treated as first-class executable values. In Opus, the traditional `eval` function is conceptually replaced by applying an environment to an Abstract Syntax Tree (AST).

This paradigm shifts the responsibility of language features-such as hygienic modules, sandboxing, and control flow-from compiler/interpreter-level implementation to user-land code.

---

## Core Semantics & Emergent Properties

The heart of Opus is **operative** (`$vau`). Unlike traditional functions, an operative receives its arguments **unevaluated** and has explicit access to the calling environment.

This allows us to derive standard Lisp features from first principles. For instance, a `$lambda` is simply a "wrapped operative" that evaluates its arguments before execution:

```scheme
($define! $lambda
  ($vau (params body) env
    (env (@ wrap (@ $vau '_ params body)))))
    ;; The @ here is simply an alias for `list`
```

*Note: In Opus, environments are themselves operatives that act as evaluators.*

This example highlights several crucial distinctions between Opus and common programming languages:

- **Macros** are just operatives that call the evaluator at the end of their body.
- **Functions** are simply "wrapped operatives" by the `wrap` pirmitive, which forces argument evaluation prior to execution.
- Operatives can be placed in an "AST" to be evaluated by an environment/evaluator.

Because Opus relies purely on environments and operatives, several complex features emerge naturally from its minimal ruleset.

### First-class Environments as Modules

By treating an environment as an operative that evaluates ASTs within its own scope, a hygienic module export system can be implemented entirely in user-space (see `prelude.op`):

```scheme
($define! $export
  ($vau exports private
    ($let ((public (child raw-root!)))
      ($begin
        (map ($lambda (sym)
                      (public
                        (@ $define! sym (private sym))))
             exports)
        (unwrap public)))))
```

### Capability-Based Sandboxing

Opus has no hardcoded special forms. Variable binding (`$define!`) is simply an operative bound in the global root. Consequently, creating an mathematically safe, read-only sandbox is achieved by simply redefining or removing the binding capability within a child environment:

```scheme
($define! $sandbox (unwrap (child root)))
($sandbox ($define! $define! ()))
($sandbox ($define! raw-root! ()))
($sandbox ($define! root ()))
;; now the sandbox is readonly
```

### Control Flow & Continuations

Since Opus is implemented using a CPS-based interpreter (`ContT`), it natively supports Proper Tail calls and First-class Continuations.

Exalmple of capturing and invoking a continuation in the REPL:

```
opus> ($define! a ($let ((k (call/cc ($lambda (k) k)))) ($begin (displayln! "hey!") k)))
hey!
()
opus> (a a)
hey!
()
opus> (a 1)
hey!
()
opus> a
1
```

## Build Instructions

### Native Build (Linux / macOS)

```sh
git clone https://github.com/yaoshiu/opus.git
cd opus
cabal build cli
cabal run cli
```

### Nix

```sh
git clone https://github.com/yaoshiu/opus.git
cd opus
nix run .
```

### WebAssembly Build

Ensure you have the wasm32-wasi-ghc toolchain configured.

```sh
nix develop # flake.nix provides the wasm32-wasi-ghc-meta
wasm32-wasi-cabal build wasm
```

## References

- [The Kernel Programming Language](https://web.cs.wpi.edu/~jshutt/kernel.html) (John Shutt's PhD Thesis)
