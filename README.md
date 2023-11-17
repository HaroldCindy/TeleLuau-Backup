# TeleLuau

A fork of [Luau](https://github.com/Roblox/luau/) exploring serializable micro-VMs for
[mobile agent](https://en.wikipedia.org/wiki/Mobile_agent) entities in virtual worlds.
It uses a modified [Eris](https://github.com/fnuecke/eris) (renamed Ares) to serialize agent execution state.

Its intended use is scripting for mixed-author environments with stateful, semi-autonomous objects that can seamlessly
roam across server instances.

This is a work-in-progress, and includes a lot of changes to support transparently serializable execution state
that are unlikely to be upstreamable, but might be a helpful reference. See `microvm_notes.md` for rough WIP notes.

See [the current diff between Luau and Teleluau here](https://github.com/HaroldCindy/teleluau/compare/master...teleluau?expand=1)

The changes to Luau proper are pretty minimal and mainly involve adding hooks to support VM state serialization.

* Basic VM state serialization is complete. A yielded thread can be serialized, along with its global environment
  without unnecessary duplication of protos from the "base" system image.
* Many cheap "forks" of a base script may be spawned inside a VM, each with their own isolated state
* Iterators are stable across `deserialize(serialize(vm_state))` trips, regardless of hash bucketing changes
* Luau's JIT can be used mostly as-is, and serializing state while inside a JITed function is fully supported.
* Per-script Memory limits are implemented through Luau's alloc hook + memcat system.
* * This could do with some tuning to make GC more likely as a fork's soft memory limit approaches
* Pre-emptive scheduling is not yet implemented, an extensible scheduler interface is planned.

# Contributing

<<<<<<< HEAD
Sure! PRs are welcome, as are discussions and emails. Truth be told, I'm using this to learn Lua and VM design by
targeting an obscure usecase, so if something seems odd to you it's probably because I don't know what I'm doing!
=======
`luau-analyze` is a command-line type checker and linter; given a set of input files, it produces errors/warnings according to the file configuration, which can be customized by using `--!` comments in the files or [`.luaurc`](https://rfcs.luau-lang.org/config-luaurc) files. For details please refer to [type checking]( https://luau-lang.org/typecheck) and [linting](https://luau-lang.org/lint) documentation.

# Installation

You can install and run Luau by downloading the compiled binaries from [a recent release](https://github.com/luau-lang/luau/releases); note that `luau` and `luau-analyze` binaries from the archives will need to be added to PATH or copied to a directory like `/usr/local/bin` on Linux/macOS.

Alternatively, you can use one of the packaged distributions (note that these are not maintained by Luau development team):

- macOS: [Install Homebrew](https://docs.brew.sh/Installation) and run `brew install luau`
- Arch Linux: From the AUR (Arch Linux User Repository), install one of these packages via a AUR helper or manually (by cloning their repo and using ``makepkg``): [luau](https://aur.archlinux.org/packages/luau) (manual build), [luau-git](https://aur.archlinux.org/packages/luau-git) (manual build by cloning this repo), or [luau-bin](https://aur.archlinux.org/packages/luau-bin) (pre-built binaries from releases)
- Alpine Linux: [Enable community repositories](https://wiki.alpinelinux.org/w/index.php?title=Enable_Community_Repository) and run `apk add luau`
- Gentoo Linux: Luau is [officially packaged by Gentoo](https://packages.gentoo.org/packages/dev-lang/luau) and can be installed using `emerge dev-lang/luau`. You may have to unmask the package first before installing it (which can be done by including the `--autounmask=y` option in the `emerge` command).

After installing, you will want to validate the installation was successful by running the test case [here](https://luau-lang.org/getting-started).
>>>>>>> upstream/master

## Building

On all platforms, you can use CMake to run the following commands to build Luau binaries from source:

```sh
mkdir cmake && cd cmake
cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo
cmake --build . --target Luau.Repl.CLI --config RelWithDebInfo
cmake --build . --target Luau.Analyze.CLI --config RelWithDebInfo
```

Alternatively, on Linux/macOS you can use `make`:

```sh
make config=release luau luau-analyze
```

To integrate Luau into your CMake application projects as a library, at the minimum you'll need to depend on `Luau.Compiler` and `Luau.VM` projects. From there you need to create a new Luau state (using Lua 5.x API such as `lua_newstate`), compile source to bytecode and load it into the VM like this:

```cpp
// needs lua.h and luacode.h
size_t bytecodeSize = 0;
char* bytecode = luau_compile(source, strlen(source), NULL, &bytecodeSize);
int result = luau_load(L, chunkname, bytecode, bytecodeSize, 0);
free(bytecode);

if (result == 0)
    return 1; /* return chunk main function */
```

For more details about the use of host API you currently need to consult [Lua 5.x API](https://www.lua.org/manual/5.1/manual.html#3). Luau closely tracks that API but has a few deviations, such as the need to compile source separately (which is important to be able to deploy VM without a compiler), or lack of `__gc` support (use `lua_newuserdatadtor` instead).

To gain advantage of many performance improvements it's highly recommended to use `safeenv` feature, which sandboxes individual scripts' global tables from each other as well as protects builtin libraries from monkey-patching. For this to work you need to call `luaL_sandbox` for the global state and `luaL_sandboxthread` for each new script's execution thread.

# Testing

Luau has an internal test suite; in CMake builds it is split into two targets, `Luau.UnitTest` (for bytecode compiler and type checker/linter tests) and `Luau.Conformance` (for VM tests). The unit tests are written in C++, whereas the conformance tests are largely written in Luau (see `tests/conformance`).

Makefile builds combine both into a single target and can be ran via `make test`.

# Dependencies

Luau uses C++ as its implementation language. The runtime requires C++11, whereas the compiler and analysis components require C++17. It should build without issues using Microsoft Visual Studio 2017 or later, or gcc-7 or clang-7 or later.

Other than the STL/CRT, Luau library components don't have external dependencies. The test suite depends on [doctest](https://github.com/onqtam/doctest) testing framework, and the REPL command-line depends on [isocline](https://github.com/daanx/isocline).

# License

Luau implementation is distributed under the terms of [MIT License](https://github.com/luau-lang/luau/blob/master/LICENSE.txt). It is based on Lua 5.x implementation that is MIT licensed as well.

When Luau is integrated into external projects, we ask to honor the license agreement and include Luau attribution into the user-facing product documentation. The attribution using [Luau logo](https://github.com/luau-lang/site/blob/master/logo.svg) is also encouraged.
