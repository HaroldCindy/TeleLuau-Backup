# TeleLuau

A fork of [Luau](https://github.com/Roblox/luau/) exploring serializable micro-VMs for 
[mobile agent](https://en.wikipedia.org/wiki/Mobile_agent) entities in virtual worlds.
It uses a modified [Eris](https://github.com/fnuecke/eris) to serialize agent execution state.

This is a work-in-progress, and includes a lot of changes that are unlikely to be upstreamable to support transparently
serializable execution state, but might be a helpful reference. See `microvm_notes.md` for rough WIP notes.

# License

Luau implementation is distributed under the terms of [MIT License](https://github.com/Roblox/luau/blob/master/LICENSE.txt). It is based on Lua 5.x implementation that is MIT licensed as well.

When Luau is integrated into external projects, we ask to honor the license agreement and include Luau attribution into the user-facing product documentation. The attribution using [Luau logo](https://github.com/Roblox/luau/blob/master/docs/logo.svg) is also encouraged.
