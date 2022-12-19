# TeleLuau

A fork of [Luau](https://github.com/Roblox/luau/) exploring serializable micro-VMs for 
[mobile agent](https://en.wikipedia.org/wiki/Mobile_agent) entities in virtual worlds.
It uses a modified [Eris](https://github.com/fnuecke/eris) (renamed Ares) to serialize agent execution state.

Its intended use is scripting for mixed-author environments with stateful, semi-autonomous objects that can seamlessly
roam across server instances.

This is a work-in-progress, and includes a lot of changes to support transparently serializable execution state
that are unlikely to be upstreamable, but might be a helpful reference. See `microvm_notes.md` for rough WIP notes.

# Status

See [the current diff between Luau and Teleluau here](https://github.com/HaroldCindy/teleluau/compare/master...teleluau?expand=1)

The changes to Luau proper are pretty minimal and mainly involve adding hooks to support VM state serialization.

* Basic VM state serialization is complete. A yielded thread can be serialized, along with its global environment
  without unnecessary duplication of protos from the "base" system image.
* Many cheap "forks" of a base script may be spawned inside a VM, each with their own isolated state
* Iterators are stable across `deserialize(serialize(vm_state))` trips, regardless of hash bucketing changes
* Luau's JIT can be used mostly as-is, and serializing state while inside a JITed function is fully supported.
* Pre-emptive scheduling is not yet implemented, an extensible scheduler interface is planned.
* Per-script Memory limits are similarly unimplemented, but will be implemented through Luau's alloc hook + memcat
  system.

# Contributing

Sure! PRs are welcome, as are discussions and emails. Truth be told, I'm using this to learn Lua and VM design by
targeting an obscure usecase, so if something seems odd to you it's probably because I don't know what I'm doing!

# License

Luau implementation is distributed under the terms of [MIT License](https://github.com/Roblox/luau/blob/master/LICENSE.txt). It is based on Lua 5.x implementation that is MIT licensed as well.

When Luau is integrated into external projects, we ask to honor the license agreement and include Luau attribution into the user-facing product documentation. The attribution using [Luau logo](https://github.com/Roblox/luau/blob/master/docs/logo.svg) is also encouraged.
