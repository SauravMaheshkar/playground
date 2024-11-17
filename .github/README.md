# [![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org) Nix Flake templates
* [`templates/c`](../templates/c/flake.nix): Basic devShell with [gdb](https://sourceware.org/gdb/)
* [`templates/haskell`](../templates/haskell/flake.nix): Basic devShell with [cabal](https://www.haskell.org/cabal/), [ghc](https://www.haskell.org/ghc/) and [haskell-language-server](https://wiki.haskell.org/Haskell-language-server)
* [`templates/prolog`](../templates/prolog/flake.nix): Basic devShell with [swiProlog](https://www.swi-prolog.org/)
* [`templates/python`](../templates/python/flake.nix): Basic devShell with py3.10, [uv](https://github.com/astral-sh/uv), [mypy](https://github.com/python/mypy) and [ruff](https://github.com/astral-sh/ruff)
* [`templates/rust`](../templates/rust/flake.nix): Basic devShell with [rustToolchain](https://rust-lang.github.io/rustup/concepts/toolchains.html) and [rust-analyser](https://rust-analyzer.github.io/)

# Data Structures / Algorithms
Implementations of various leetcode problems in (C/C++/Haskell/Prolog/Python/Rust) with automated testing using CI/CD. (atm I'm working on the [Neetcode Blind 75](https://neetcode.io/practice))

<details>
<summary>Leetcode Tests CI/CD Status</summary>
<br>

[![Leetcode C/C++ Tests](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-c-tests.yml/badge.svg)](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-c-tests.yml) [![Leetcode Prolog Tests](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-prolog-tests.yml/badge.svg)](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-prolog-tests.yml) [![Leetcode Python Tests](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-python-tests.yml/badge.svg)](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-python-tests.yml) [![Leetcode Rust Tests](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-rust-tests.yml/badge.svg)](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-rust-tests.yml) [![Leetcode Haskell Tests](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-haskell-tests.yml/badge.svg)](https://github.com/SauravMaheshkar/playground/actions/workflows/leetcode-haskell-tests.yml)
</details>

# `arena.c` [![memalloc lib checks](https://github.com/SauravMaheshkar/playground/actions/workflows/memalloc.yml/badge.svg)](https://github.com/SauravMaheshkar/playground/actions/workflows/memalloc.yml)
C library for Arena based memory management, for example usage refer to `src/memalloc/arena/example.c`
