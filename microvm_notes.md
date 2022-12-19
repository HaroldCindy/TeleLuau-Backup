# Serializable execution state = yes

## Unsolved Problems

### Bytecode sharing

Recompiling the same script each time it's loaded into a VM sucks. Copying the bytecode (and potentially native asm)
for each `Proto` also sucks if you have a lot of the same scripts, wastes tons of memory. Luau doesn't really modify
the bytecode other than for the inline lookup caches, which isn't a blocker for sharing.

#### Idea

Have a "base" thread that only exists to load compiled scripts into, it stays alive as long as any of its "forks"
are alive. It owns the memory for the proto bytecode (and potentially x64 asm.) Once the script is compiled and loaded
into the VM, the state gets serialized through Ares, with placeholders for the bytecode and asm fields in the Protos.

When a new instance of a script needs to be spun up, it gets the initial state for its VM populated with the serialized
state from the "base" VM, creating its own `Proto`s, but pointing its bytecode and asm pointers to those in the "base"
VM. The created `Proto`s would have to be marked as having unowned code so as to not `free()` the shared data if the
script state gets wound up. Maybe we can just use the `Proto`s directly so long as the threads belong to the same
`global_State`?

Basically we want to create something like a "permanent" generation (even though this obviously isn't a generational
collector.) Does any of the code care about "ownership" of `Proto`s or constants referenced by them? The only problem
I can think of is that dupclosure would no longer be able to use the "constant" closures on the Proto because there
would be an env mismatch between the "base" VM and the fork.

# Pre-emptive scheduling = yes

Forcing yields in VM interrupt callbacks is great.

## Unsolved Problems

### Some Lua features builtins make it hard to limit abuse

`gmatch`, `gsub`, etc can go quadratic due to backtracking, can't interrupt because they're in C land.

https://github.com/minetest-mods/mesecons/issues/255

#### Idea

Why the heck can't we interrupt a C call anyway? Can't we just write the state to the stack and push a continuation
before yielding? I guess that would be annoying to keep in sync with upstream. Best bet would be to write pure Lua
implementations things like that since they're easier to interrupt. We don't really care that they can go quadratic,
we care that they're _annoying to interrupt_ when they do.

https://notabug.org/pgimeno/patlua/raw/master/pat.lua maybe?

### How to distinguish forced interrupt yields from `coroutine.yield()`

The distinction is important when reloading VM state with Ares, we don't want to restart yielded coroutines
that weren't stopped by us.

#### Idea

New thread state that we could serialize? Look for places where `LUA_BREAK` is currently referenced

### "Gas" implementation

How do we know when to yield?

#### Idea

`gettimeofday()` delta at interrupt points is a crap metric but I guess it's ok as a first pass. Doesn't require
bytecode introspection. Fastcalls could have some finite cost.


# Serialized execution states from untrusted sources (maybe?)

Luau is meant to be safe enough that you can accept script source (but not bytecode!) from an arbitrary source and
be relatively safe, barring resource exhaustion attacks which can be handled with VM hooks. Luau's security
model _requires_ recompiling script source to bytecode, we have to be extra careful about how we handle serialized
execution state if we want to be able to accept it from untrusted parties, but Eris' thread execution state
representation assumes that you can just use the bytecode of a serialized `Proto` without recompiling. That introduces
a number of forward-compatibility issues that we now need to care about.

Naturally, if you only load execution states from trusted sources, none of this really matters.

## Unsolved Problems

Remember that serialized thread state includes the instruction pointer, the current state of the stack and the
current state of the locals. The bytecode you compile must be exactly the same as the bytecode that belonged to the
serialized execution state. Any changes to bytecode offsets mean that the serialized state will no longer work.

### Instruction pointers change on recompilation

If you use a newer compiler that decides a branch can be eliminated, the instruction pointer (which is an offset from
the start of the function) may become wrong. If the compiler decides it's now able to inline a function it wasn't able
to before, your instruction pointer may also become wrong. Luau's bytecode compiler having an optimization pass that can
improve across releases is now a problem for you!

#### Idea

A field could be added to protos that included a map of all legal yield points (func call, back edge of
loop, etc,) and only serialize the "relative" PC so it could be remapped to the new bytecode.

### Stack effect / callinfo differences
Even if you deal with the bytecode offset problem, expected stack effects and register allocation in your
newly-compiled code must also be the same as the original, or the serialized stack will be invalid.

There would also need to be some sort of validation for the stack and callinfo array. Technically it's possible
to represent totally invalid callinfo arrays that may mess up VM state if not validated. Luau's analysis tools should
give you the tools to figure out what the state of the local stack frame _should_ whatever point in a given proto,
but I doubt it can infer if a callinfo array itself is sensible or not due to dynamic dispatch. Would require some
thought.

#### Idea

Write a lot of validation tools, I guess. For the bytecode compatibility issue, I think you'd have to be pretty
conservative about optimizations or include context about what optimizations were used to compile the script originally.
Those optimizations would have to be forward compatible and behave in the same way in future versions.

For callinfo validation, I think it's ok if the callinfo array isn't sensible so long as it can be verified to not
be totally wrong (i.e. stack the wrong size for the function, mismatched types in the local stack, wrong number of
upvalues.)

Non-trivial problem, but i guess it could work if you were careful.

# Random

* to prevent name collisions in newer versions of the VM that add globals, serialized state should include
  a list of keys on the global table. any that now exist but didn't exist before, and aren't present on the
  proxy table should be `nil`ed on the proxy table so they aren't visible. This is important for scripts that
  rely on global vars being readable without ever having first set them (nasty, but legal.)
* readonly state for tables needs serializing.
* anything for thread deserialization that needs to be special? sandboxing state? safeenv state?
* How to deal with "library" closures on global implemented in Lua (gmatch, etc?) is setfenv() a problem?
* c closures should always have `GL->env` as their env, unless you're doing something weird.
* can't use `raw_iter()` if we care about dead keys
* luau does NOT allow yielding in metamethods, this may be an issue for `__call`
* * https://create.roblox.com/docs/scripting/luau/metatables#metamethods
* * https://luau-lang.org/compatibility#lua-52
* * Maybe allow "trivial" metamethods, set "should suspend" flag in interrupt if we detect we're inside multiple
    c calls? kill thread if "extra" budget exceeded after that, maybe apply scheduling penalty if not?
* * Does no yielding inside metamethods imply no breakpoints inside metamethods?
* * `__call` seems to allow `yield`ing just fine
* Tables can only shrink when adding a key because otherwise `nil`ing the current field in `for` loops
  would be illegal.
* upvalue persistence is based on upvalue address, which needs to be made stack-relative.
* Luau is smart and guards against ref loops in __index and friends
* Serializing activememcat / memcat?
* Strings are interned and shared across threads within the same VM. Great for memory, but makes it difficult to
  have effective per-thread resource limitations.
* * Maybe only "global" / "shared" strings like those present in the bytecode can end up in the global strings table,
    then the rest end up in a "local" strings table? I dunno. string equality being done by pointer compare makes things
    hairy. Maybe new threads get their own string table inheriting from the global string table?
* * Ugh, maybe "real" micro-VMs are easiest to orchestrate like this.
* `baseCCalls` consistency needed
* Change `newlstr()` to only check for strings in memcat 0 (base memcat) and the state's current memcat.
* Optionally treat `GCObject`s in memcat 0 as "permanent", not traversing them or freeing them until the VM is destroyed
