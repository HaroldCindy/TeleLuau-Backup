/*
Ares - Heavy-duty persistence for Luau - Based on Eris, based on Pluto
Copyright (c) 2022 by Harold Cindy
Copyright (c) 2013-2015 by Florian Nuecke.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

/* The API still uses the "eris" prefix throughout, and uses the old whitespace
 * style to make diffing against Eris proper easier.
 */

/* Standard library headers. */
#include <cstdio>
#include <cstring>
#include <ostream>
#include <istream>
#include <sstream>
#include <vector>

/* Mark us as part of the Lua core to get access to what we need. */
#define eris_c
#define LUA_CORE

/* Public Lua headers. */
#include "lua.h"
#include "lualib.h"

/* Internal Lua headers. */
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"

/* Eris header. */
#include "ares.h"
#include "lapi.h"
#include "ltable.h"
#include "ludata.h"

/*
** {===========================================================================
** Default settings.
** ============================================================================
*/

/* Note that these are the default settings. They can be changed either from C
 * by calling ares_g|set_setting() or from Lua using ares.settings(). */

/* The metatable key we use to allow customized persistence for tables and
 * userdata. */
// TODO: This really should be based on userdata tags
static const char *const kPersistKey = "__persist";

/* Whether to persist debug information such as line numbers and upvalue and
 * local variable names. */
static const bool kWriteDebugInformation = true;

/* Generate a human readable "path" that is shown together with error messages
 * to indicate where in the object the error occurred. For example:
 * ares.persist({false, bad = setmetatable({}, {__persist = false})})
 * Will produce: main:1: attempt to persist forbidden table (root.bad)
 * This can be used for debugging, but is disabled per default due to the
 * processing and memory overhead this introduces. */
static const bool kGeneratePath = false;

/* The maximum object complexity. This is the number of allowed recursions when
 * persisting or unpersisting an object, for example for nested tables. This is
 * used to avoid segfaults when writing or reading user data. */
static const lua_Unsigned kMaxComplexity = 10000;

/*
** ============================================================================
** Lua internals interfacing.
** ============================================================================
*/

/* Lua internals we use. We define these as macros to make it easier to swap
 * them out, should the need ever arise. For example, the later Pluto versions
 * copied these function to own files (presumably to allow building it as an
 * extra shared library). These should be all functions we use that are not
 * declared in lua.h or lauxlib.h. If there are some still directly in the code
 * they were missed and should be replaced with a macro added here instead. */
/* I'm quite sure we won't ever want to do this, because Eris needs a slightly
 * patched Lua version to be able to persist some of the library functions,
 * anyway: it needs to put the continuation C functions in the perms table. */
/* ldebug.h */
#define eris_ci_func ci_func
/* ldo.h */
#define eris_incr_top incr_top
#define eris_savestack savestack
#define eris_restorestack restorestack
#define eris_reallocstack luaD_reallocstack
/* lfunc.h */
#define eris_newproto luaF_newproto
#define eris_newLclosure luaF_newLclosure
#define eris_newupval luaF_newupval
#define eris_findupval luaF_findupval
/* lgc.h */
#define eris_barrierproto luaC_barrierproto
/* lmem.h */
#define eris_reallocvector(L, b, on, n, e) luaM_reallocarray(L, b, on, n, e, L->activememcat)
/* lobject.h */
#define eris_ttypenv(o) ((o)->tt)
#define eris_clLvalue clLvalue
#define eris_setnilvalue setnilvalue
#define eris_setclLvalue setclLvalue
#define eris_setobj setobj
#define eris_setsvalue2n setsvalue
/* lstate.h */
#define eris_isLua isLua
#define eris_gch gch
#define eris_gco2uv gco2uv
#define eris_obj2gco obj2gco
#define eris_extendCI luaE_extendCI
/* lstring. h */
#define eris_newlstr luaS_newlstr

/* These are required for cross-platform support, since the size of TValue may
 * differ, so the byte offset used by savestack/restorestack in Lua it is not a
 * valid measure. */
#define eris_savestackidx(L, p) ((p) - (L)->stack)
#define eris_restorestackidx(L, n) ((L)->stack + (n))

/*
** ============================================================================
** Language strings for errors.
** ============================================================================
*/

#define ERIS_ERR_CFUNC "attempt to persist a light C function (%p) \"%s\""
#define ERIS_ERR_CFUNC_UPVALS "attempt to persist non-permanent C function "\
                              "with upvals (%p) \"%s\""
#define ERIS_ERR_COMPLEXITY "object too complex"
#define ERIS_ERR_HOOK "cannot persist yielded hooks"
#define ERIS_ERR_METATABLE "bad metatable, not nil or table"
#define ERIS_ERR_NOFUNC "attempt to persist unknown function type"
#define ERIS_ERR_READ "could not read data"
#define ERIS_ERR_SPER_FUNC "%s did not return a function"
#define ERIS_ERR_SPER_LOAD "bad unpersist function (%s expected, returned %s)"
#define ERIS_ERR_SPER_PROT "attempt to persist forbidden table"
#define ERIS_ERR_SPER_TYPE "%d not nil, boolean, or function"
#define ERIS_ERR_SPER_UFUNC "invalid restore function"
#define ERIS_ERR_SPER_UPERM "bad permanent value (%s expected, got %s)"
#define ERIS_ERR_SPER_UPERMNIL "bad permanent value (no value)"
#define ERIS_ERR_STACKBOUNDS "stack index out of bounds"
#define ERIS_ERR_TABLE "bad table value, got a nil value"
#define ERIS_ERR_THREAD "cannot persist currently running thread"
#define ERIS_ERR_THREADCI "invalid callinfo"
#define ERIS_ERR_THREADCTX "bad C continuation function"
#define ERIS_ERR_THREADERRF "invalid errfunc"
#define ERIS_ERR_THREADPC "saved program counter out of bounds"
#define ERIS_ERR_TRUNC_INT "int value would get truncated"
#define ERIS_ERR_TRUNC_SIZE "size_t value would get truncated"
#define ERIS_ERR_TYPE_FLOAT "unsupported lua_Number type"
#define ERIS_ERR_TYPE_INT "unsupported int type"
#define ERIS_ERR_TYPE_SIZE "unsupported size_t type"
#define ERIS_ERR_TYPEP "trying to persist unknown type %d"
#define ERIS_ERR_TYPEU "trying to unpersist unknown type %d"
#define ERIS_ERR_UCFUNC "bad C closure (C function expected, got %s)"
#define ERIS_ERR_UCFUNCNULL "bad C closure (C function expected, got null)"
#define ERIS_ERR_USERDATA "attempt to literally persist userdata"
#define ERIS_ERR_WRITE "could not write data"
#define ERIS_ERR_REF "invalid reference #%d. this usually means a special "\
                      "persistence callback of a table referenced said table "\
                      "(directly or indirectly via an upvalue)."

/*
** ============================================================================
** Constants, settings, types and forward declarations.
** ============================================================================
*/

/* The "type" we write when we persist a value via a replacement from the
 * permanents table. This is just an arbitrary number, but it must be outside
 * the range Lua uses for its types (> LUA_TOTALTAGS). */
#define ERIS_PERMANENT (LUA_TDEADKEY + 1)
/* The "type" we use to reference something from the (ephemeral) reftable */
#define ERIS_REFERENCE (ERIS_PERMANENT + 1)

/* Avoids having to write the nullptr all the time, plus makes it easier adding
 * a custom error message should you ever decide you want one. */
#define eris_checkstack(L, n) luaL_checkstack(L, n, nullptr)

/* Used for internal consistency checks, for debugging. These are true asserts
 * in the sense that they should never fire, even for bad inputs. */
#if !defined(NDEBUG) || defined(LUAU_ENABLE_ASSERT)
#define eris_assert(c) LUAU_ASSERT(c)
#define eris_ifassert(e) e
#else
#define eris_assert(c) ((void)0)
#define eris_ifassert(e) ((void)0)
#endif

/* State information when persisting an object. */
typedef struct PersistInfo {
  std::ostream *writer;
  void *ud;
  const char *metafield;
  bool writeDebugInfo;
  bool persistingCFunc;
} PersistInfo;

typedef uint8_t lu_byte;

/* State information when unpersisting an object. */
typedef struct UnpersistInfo {
  std::istream * reader;
  size_t sizeof_int;
  size_t sizeof_size_t;
  size_t vector_components;
} UnpersistInfo;

/* Info shared in persist and unpersist. */
typedef struct Info {
  lua_State *L;
  lua_Unsigned level;
  int refcount; /* int because rawseti/rawgeti takes an int. */
  lua_Unsigned maxComplexity;
  bool generatePath;
  bool persisting;
  /* Which one it really is will always be clear from the context. */
  union {
    PersistInfo pi;
    UnpersistInfo upi;
  } u;
} Info;

typedef enum eris_CIKind {
    ERIS_CI_KIND_NONE = 0,
    ERIS_CI_KIND_LUA = 1,
    ERIS_CI_KIND_C = 2,
} eris_CIKind;

/* Type names, used for error messages. */
static const char *const kTypenames[] = {
  "nil", "boolean", "lightuserdata", "number", "string",
  "table", "function", "userdata", "thread", "proto", "upval",
  "deadkey", "permanent"
};

/* Setting names as used in ares.settings / ares_g|set_setting. Also, the
 * addresses of these static variables are used as keys in the registry of Lua
 * states to save the current values of the settings (as light userdata). */
static const char kSettingMetafield[] = "spkey";
static const char kSettingGeneratePath[] = "path";
static const char kSettingWriteDebugInfo[] = "debug";
static const char kSettingMaxComplexity[] = "maxrec";
static const char kForkerPermsTable[] = "forkerpermstable";
static const char kForkerUPermsTable[] = "forkerupermstable";
static const char kForkerBaseThread[] = "forkerbasethread";
static const char kForkerBaseState[] = "forkerbasestate";

/* Header we prefix to persisted data for a quick check when unpersisting. */
static char const kHeader[] = { 'A', 'R', 'E', 'S' };
#define HEADER_LENGTH sizeof(kHeader)

/* Floating point number used to check compatibility of loaded data. */
static const lua_Number kHeaderNumber = (lua_Number)-1.234567890;

/* Stack indices of some internal values/tables, to avoid magic numbers. */
#define PERMIDX 1
#define REFTIDX 2
#define BUFFIDX 3
#define PATHIDX 4

/* Table indices for upvalue tables, keeping track of upvals to open. */
#define UVTOCL 1
#define UVTONU 2
#define UVTVAL 3
#define UVTREF 4

// Stack indices for forkserver threads
#define FS_STATE_IDX 1

/* }======================================================================== */

/*
** {===========================================================================
** Utility functions.
** ============================================================================
*/

/* Temporarily disable GC collections while this object is live, restoring GC
 * parameters when it goes out of scope.
 */
struct ScopedDisableGC {
    explicit ScopedDisableGC(lua_State *L):
            state(L), threshold(L->global->GCthreshold) {
        state->global->GCthreshold = SIZE_MAX;
    };

    ~ScopedDisableGC() {
        state->global->GCthreshold = threshold;
    }

    lua_State *state;
    size_t threshold;
};

/* Pushes an object into the reference table when unpersisting. This creates an
 * entry pointing from the id the object is referenced by to the object. */
static int
registerobject(Info *info) {                          /* perms reftbl ... obj */
  const int reference = ++(info->refcount);
  eris_checkstack(info->L, 1);
  lua_pushvalue(info->L, -1);                     /* perms reftbl ... obj obj */
  lua_rawseti(info->L, REFTIDX, reference);           /* perms reftbl ... obj */
  return reference;
}

/** ======================================================================== */

/* Pushes a TString* onto the stack if it holds a value, nil if it is nullptr. */
static void
pushtstring(lua_State* L, TString *ts) {                               /* ... */
  if (ts) {
    eris_setsvalue2n(L, L->top, ts);
    eris_incr_top(L);                                              /* ... str */
  }
  else {
    lua_pushnil(L);                                                /* ... nil */
  }
}

/* Creates a copy of the string on top of the stack and sets it as the value
 * of the specified TString**. */
static void
copytstring(lua_State* L, TString **ts) {
  if (lua_type(L, -1) == LUA_TNIL) {
    *ts = nullptr;
  } else {
    size_t length;
    const char* value = lua_tolstring(L, -1, &length);
    *ts = eris_newlstr(L, value, length);
  }
}

/** ======================================================================== */

// Get an opaque, stable identifier to an upvalue that will be valid for
// the duration of the persist() call.
static void *eris_getupvalueid_safe(Info *info, int funcindex, int n) {
  void *uv_id = lua_getupvalueid(info->L, funcindex, n);
  // We have to be careful about addresses that are on the stack that
  // persist() is running on. We may need to realloc() our stack as part
  // of serializing, which will invalidate any existing UpValue addresses.
  // Sniff out addresses within our stack, and key them off of a
  // stack-relative address instead.
  if (uv_id >= info->L->stack && uv_id < info->L->top) {
    // Tag the upper byte so we know it's not a real pointer. Luau's
    // iterators also push a `size_t` in `lightuserdata` for the current
    // iter index, so we want to make sure those would never match an
    // upvalue `lightuserdata` key.
    size_t tag_byte = size_t(0xF) << ((sizeof(void*) - size_t(1)) * 8);
    uv_id = (void*)(eris_savestackidx(info->L, (StkId)uv_id) | tag_byte);
  }
  return uv_id;
}

/** ======================================================================== */

// Get a pointer to the "real" globals base table, not the sandboxed proxy table.
// This is important for thread serialization where we want to serialize any
// mutations the thread may have done to its sandboxed globals, but we want to
// preserve the "proxy-ness" of the thread's global table.
static Table *eris_getglobalsbase(Info *info) {
  lua_State *GL = lua_mainthread(info->L);
  Table *gt = GL->gt;
  // Looks like the global thread has sandboxed globals, we have to do a bit
  // of spelunking to get a reference to the base globals table.
  if (gt->metatable && !gt->readonly) {
    eris_assert(gt->metatable->readonly);
    // The real table should live in the metatable's __index
    TString *index_key = eris_newlstr(info->L, "__index", strlen("__index"));
    gt = hvalue(luaH_getstr(gt->metatable, index_key));
  }
  return gt;
}

/** ======================================================================== */

/* Pushes the specified segment to the current path, if we're generating one.
 * This supports formatting strings using Lua's formatting capabilities. */
static void
pushpath(Info *info, const char* fmt, ...) {     /* perms reftbl var path ... */
  if (!info->generatePath) {
    return;
  }
  else {
    va_list argp;
    eris_checkstack(info->L, 1);
    va_start(argp, fmt);
    lua_pushvfstring(info->L, fmt, argp);    /* perms reftbl var path ... str */
    va_end(argp);
    lua_rawseti(info->L, PATHIDX, lua_objlen(info->L, PATHIDX) + 1);
  }                                              /* perms reftbl var path ... */
}

/* Pops the last added segment from the current path if we're generating one. */
static void
poppath(Info *info) {                            /* perms reftbl var path ... */
  if (!info->generatePath) {
    return;
  }
  eris_checkstack(info->L, 1);
  lua_pushnil(info->L);                      /* perms reftbl var path ... nil */
  lua_rawseti(info->L, PATHIDX, lua_objlen(info->L, PATHIDX));
}                                                /* perms reftbl var path ... */

/* Concatenates all current path segments into one string, pushes it and
 * returns it. This is relatively inefficient, but it's for errors only and
 * keeps the stack small, so it's better this way. */
static const char*
path(Info *info) {                               /* perms reftbl var path ... */
  if (!info->generatePath) {
    return "";
  }
  eris_checkstack(info->L, 3);
  lua_pushstring(info->L, "");               /* perms reftbl var path ... str */
  lua_pushnil(info->L);                  /* perms reftbl var path ... str nil */
  while (lua_next(info->L, PATHIDX)) {   /* perms reftbl var path ... str k v */
    lua_insert(info->L, -2);             /* perms reftbl var path ... str v k */
    lua_insert(info->L, -3);             /* perms reftbl var path ... k str v */
    lua_concat(info->L, 2);                /* perms reftbl var path ... k str */
    lua_insert(info->L, -2);               /* perms reftbl var path ... str k */
  }                                          /* perms reftbl var path ... str */
  return lua_tostring(info->L, -1);
}

/* Generates an error message with the appended path, if available. */
static int
eris_error(Info *info, const char *fmt, ...) {                         /* ... */
    va_list argp;
    eris_checkstack(info->L, 5);

    luaL_where(info->L, 1);                                     /* ... where */
    va_start(argp, fmt);
    lua_pushvfstring(info->L, fmt, argp);                    /* ... where str */
    va_end(argp);
    if (info->generatePath) {
      lua_pushstring(info->L, " (");                    /* ... where str " (" */
      path(info);                                 /* ...  where str " (" path */
      lua_pushstring(info->L, ")");            /* ... where str " (" path ")" */
      lua_concat(info->L, 5);                                      /* ... msg */
    }
    else {
      lua_concat(info->L, 2);                                      /* ... msg */
    }
    lua_error(info->L);
    return 0;
}

/** ======================================================================== */

/* Tries to get a setting from the registry. */
static bool
get_setting(lua_State *L, void *key) {                                 /* ... */
  eris_checkstack(L, 1);
  lua_pushlightuserdata(L, key);                                   /* ... key */
  lua_gettable(L, LUA_REGISTRYINDEX);                        /* ... value/nil */
  if (lua_isnil(L, -1)) {                                          /* ... nil */
    lua_pop(L, 1);                                                     /* ... */
    return false;
  }                                                              /* ... value */
  return true;
}

/* Stores a setting in the registry (or removes it if the value is nil). */
static void
set_setting(lua_State *L, void *key) {                           /* ... value */
  eris_checkstack(L, 2);
  lua_pushlightuserdata(L, key);                             /* ... value key */
  lua_insert(L, -2);                                         /* ... key value */
  lua_settable(L, LUA_REGISTRYINDEX);                                  /* ... */
}

/* Used as a callback for luaL_opt to check boolean setting values. */
static bool
checkboolean(lua_State *L, int narg) {                       /* ... bool? ... */
  if (!lua_isboolean(L, narg)) {                                /* ... :( ... */
    luaL_argerror(L, narg, lua_pushfstring(L,
      "boolean expected, got %s", lua_typename(L, lua_type(L, narg))));
    return false;
  }                                                           /* ... bool ... */
  return lua_toboolean(L, narg);
}

/* }======================================================================== */

/*
** {===========================================================================
** Persist and unpersist.
** ============================================================================
*/

/* I have macros and I'm not afraid to use them! These are highly situational
 * and assume an Info* named 'info' is available. */

/* Writes a raw memory block with the specified size. */
#define WRITE_RAW(value, size) { \
  info->u.pi.writer->write((char*)(value), (size)); \
  if (info->u.pi.writer->fail()) \
    eris_error(info, ERIS_ERR_WRITE); } while(0)

/* Writes a single value with the specified type. */
#define WRITE_VALUE(value, type) write_##type(info, value)

/* Writes a typed array with the specified length. */
#define WRITE(value, length, type) { \
    int _i; for (_i = 0; _i < length; ++_i) WRITE_VALUE((value)[_i], type); } while(0)

/** ======================================================================== */

/* Reads a raw block of memory with the specified size. */
#define READ_RAW(value, size) {\
  info->u.upi.reader->read((char *)(value), (size)); \
  if (info->u.upi.reader->fail())        \
    eris_error(info, ERIS_ERR_READ); } while(0)

/* Reads a single value with the specified type. */
#define READ_VALUE(type) read_##type(info)

/* Reads a typed array with the specified length. */
#define READ(value, length, type) { \
    int _i; for (_i = 0; _i < (length); ++_i) (value)[_i] = READ_VALUE(type); }

/** ======================================================================== */

static void
write_uint8_t(Info *info, uint8_t value) {
  WRITE_RAW(&value, sizeof(uint8_t));
}

static void
write_uint16_t(Info *info, uint16_t value) {
  write_uint8_t(info, value);
  write_uint8_t(info, value >> 8);
}

static void
write_uint32_t(Info *info, uint32_t value) {
  write_uint8_t(info, value);
  write_uint8_t(info, value >> 8);
  write_uint8_t(info, value >> 16);
  write_uint8_t(info, value >> 24);
}

static void
write_uint64_t(Info *info, uint64_t value) {
  write_uint8_t(info, value);
  write_uint8_t(info, value >> 8);
  write_uint8_t(info, value >> 16);
  write_uint8_t(info, value >> 24);
  write_uint8_t(info, value >> 32);
  write_uint8_t(info, value >> 40);
  write_uint8_t(info, value >> 48);
  write_uint8_t(info, value >> 56);
}

static void
write_int16_t(Info *info, int16_t value) {
  write_uint16_t(info, (uint16_t)value);
}

static void
write_int32_t(Info *info, int32_t value) {
  write_uint32_t(info, (uint32_t)value);
}

static void
write_int64_t(Info *info, int64_t value) {
  write_uint64_t(info, (uint64_t)value);
}

static void
write_float32(Info *info, float value) {
  uint32_t rep;
  memcpy(&rep, &value, sizeof(float));
  write_uint32_t(info, rep);
}

static void
write_float64(Info *info, double value) {
  uint64_t rep;
  memcpy(&rep, &value, sizeof(double));
  write_uint64_t(info, rep);
}

/* Note regarding the following: any decent compiler should be able
 * to reduce these to just the write call, since sizeof is constant. */

static void
write_int(Info *info, int value) {
  if (sizeof(int) == sizeof(int16_t)) {
    write_int16_t(info, value);
  }
  else if (sizeof(int) == sizeof(int32_t)) {
    write_int32_t(info, value);
  }
  else if (sizeof(int) == sizeof(int64_t)) {
    write_int64_t(info, value);
  }
  else {
    eris_error(info, ERIS_ERR_TYPE_INT);
  }
}

static void
write_size_t(Info *info, size_t value) {
  if (sizeof(size_t) == sizeof(uint16_t)) {
    write_uint16_t(info, value);
  }
  else if (sizeof(size_t) == sizeof(uint32_t)) {
    write_uint32_t(info, value);
  }
  else if (sizeof(size_t) == sizeof(uint64_t)) {
    write_uint64_t(info, value);
  }
  else {
    eris_error(info, ERIS_ERR_TYPE_SIZE);
  }
}

static void
write_lua_Number(Info *info, lua_Number value) {
  if (sizeof(lua_Number) == sizeof(uint32_t)) {
    write_float32(info, value);
  }
  else if (sizeof(lua_Number) == sizeof(uint64_t)) {
    write_float64(info, value);
  }
  else {
    eris_error(info, ERIS_ERR_TYPE_FLOAT);
  }
}

/* Note that Lua only ever uses 32 bits of the Instruction type, so we can
 * assert that there will be no truncation, even if the underlying type has
 * more bits (might be the case on some 64 bit systems). */

static void
write_Instruction(Info *info, Instruction value) {
  if (sizeof(Instruction) == sizeof(uint32_t)) {
    write_uint32_t(info, value);
  }
  else {
    auto pvalue = (uint32_t)value;
    /* Lua only uses 32 bits for its instructions. */
    eris_assert((Instruction)pvalue == value);
    write_uint32_t(info, pvalue);
  }
}

/** ======================================================================== */

// TODO: clean up this nonsense to require fewer individual stream reads
static uint8_t
read_uint8_t(Info *info) {
  uint8_t value;
  READ_RAW(&value, sizeof(uint8_t));
  return value;
}

static uint16_t
read_uint16_t(Info *info) {
  auto value = (uint16_t)read_uint8_t(info);
  value |= (uint16_t)read_uint8_t(info) << 8;
  return value;
}

static uint32_t
read_uint32_t(Info *info) {
  auto value = (uint32_t)read_uint8_t(info);
  value |= (uint32_t)read_uint8_t(info) << 8;
  value |= (uint32_t)read_uint8_t(info) << 16;
  value |= (uint32_t)read_uint8_t(info) << 24;
  return value;
}

static uint64_t
read_uint64_t(Info *info) {
  auto value = (uint64_t)read_uint8_t(info);
  value |= (uint64_t)read_uint8_t(info) << 8;
  value |= (uint64_t)read_uint8_t(info) << 16;
  value |= (uint64_t)read_uint8_t(info) << 24;
  value |= (uint64_t)read_uint8_t(info) << 32;
  value |= (uint64_t)read_uint8_t(info) << 40;
  value |= (uint64_t)read_uint8_t(info) << 48;
  value |= (uint64_t)read_uint8_t(info) << 56;
  return value;
}

static int16_t
read_int16_t(Info *info) {
  return (int16_t)read_uint16_t(info);
}

static int32_t
read_int32_t(Info *info) {
  return (int32_t)read_uint32_t(info);
}

static int64_t
read_int64_t(Info *info) {
  return (int64_t)read_uint64_t(info);
}

static float
read_float32(Info *info) {
  float value;
  uint32_t rep = read_uint32_t(info);
  memcpy(&value, &rep, sizeof(float));
  return value;
}

static double
read_float64(Info *info) {
  double value;
  uint64_t rep = read_uint64_t(info);
  memcpy(&value, &rep, sizeof(double));
  return value;
}

/* Note regarding the following: unlike with writing the sizeof check will be
 * impossible to optimize away, since it depends on the input; however, the
 * truncation check may be optimized away in the case where the read data size
 * equals the native one, so reading data written on the same machine should be
 * reasonably quick. Doing a (rather rudimentary) benchmark this did not have
 * any measurable impact on performance. */

static int
read_int(Info *info) {
  int value;
  if (info->u.upi.sizeof_int == sizeof(int16_t)) {
    int16_t pvalue = read_int16_t(info);
    value = (int)pvalue;
    if ((int32_t)value != pvalue) {
      eris_error(info, ERIS_ERR_TRUNC_INT);
    }
  }
  else if (info->u.upi.sizeof_int == sizeof(int32_t)) {
    int32_t pvalue = read_int32_t(info);
    value = (int)pvalue;
    if ((int32_t)value != pvalue) {
      eris_error(info, ERIS_ERR_TRUNC_INT);
    }
  }
  else if (info->u.upi.sizeof_int == sizeof(int64_t)) {
    int64_t pvalue = read_int64_t(info);
    value = (int)pvalue;
    if ((int64_t)value != pvalue) {
      eris_error(info, ERIS_ERR_TRUNC_INT);
    }
  }
  else {
    eris_error(info, ERIS_ERR_TYPE_INT);
    value = 0; /* not reached */
  }
  return value;
}

static size_t
read_size_t(Info *info) {
  size_t value;
  if (info->u.upi.sizeof_size_t == sizeof(uint16_t)) {
    uint16_t pvalue = read_uint16_t(info);
    value = (size_t)pvalue;
    if ((uint32_t)value != pvalue) {
      eris_error(info, ERIS_ERR_TRUNC_SIZE);
    }
  }
  else if (info->u.upi.sizeof_size_t == sizeof(uint32_t)) {
    uint32_t pvalue = read_uint32_t(info);
    value = (size_t)pvalue;
    if ((uint32_t)value != pvalue) {
      eris_error(info, ERIS_ERR_TRUNC_SIZE);
    }
  }
  else if (info->u.upi.sizeof_size_t == sizeof(uint64_t)) {
    uint64_t pvalue = read_uint64_t(info);
    value = (size_t)pvalue;
    if ((uint64_t)value != pvalue) {
      eris_error(info, ERIS_ERR_TRUNC_SIZE);
    }
  }
  else {
    eris_error(info, ERIS_ERR_TYPE_SIZE);
    value = 0; /* not reached */
  }
  return value;
}

static lua_Number
read_lua_Number(Info *info) {
  if (sizeof(lua_Number) == sizeof(uint32_t)) {
    return read_float32(info);
  }
  else if (sizeof(lua_Number) == sizeof(uint64_t)) {
    return read_float64(info);
  }
  else {
    eris_error(info, ERIS_ERR_TYPE_FLOAT);
    return 0; /* not reached */
  }
}

static Instruction
read_Instruction(Info *info) {
  return (Instruction)read_uint32_t(info);
}

/** ======================================================================== */

/* Forward declarations for recursively called top-level functions. */
static void persist_keyed(Info*, int type);
static void persist(Info*);
static void unpersist(Info*);

/*
** ============================================================================
** Simple types.
** ============================================================================
*/

static void
p_boolean(Info *info) {                                           /* ... bool */
  WRITE_VALUE(lua_toboolean(info->L, -1), uint8_t);
}

static void
u_boolean(Info *info) {                                                /* ... */
  eris_checkstack(info->L, 1);
  lua_pushboolean(info->L, READ_VALUE(uint8_t));                  /* ... bool */

  eris_assert(lua_type(info->L, -1) == LUA_TBOOLEAN);
}

/** ======================================================================== */

static void
p_pointer(Info *info) {                                         /* ... ludata */
  WRITE_VALUE((size_t)lua_touserdata(info->L, -1), size_t);
}

static void
u_pointer(Info *info) {                                                /* ... */
  eris_checkstack(info->L, 1);
  lua_pushlightuserdata(info->L, (void*)READ_VALUE(size_t));    /* ... ludata */

  eris_assert(lua_type(info->L, -1) == LUA_TLIGHTUSERDATA);
}

/** ======================================================================== */

static void
p_number(Info *info) {                                             /* ... num */
  WRITE_VALUE(lua_tonumber(info->L, -1), lua_Number);
}

static void
u_number(Info *info) {                                                 /* ... */
  eris_checkstack(info->L, 1);
  lua_pushnumber(info->L, READ_VALUE(lua_Number));                 /* ... num */

  eris_assert(lua_type(info->L, -1) == LUA_TNUMBER);
}

/** ======================================================================== */

static void
p_vector(Info *info) {                                             /* ... vec */
  const float *f = lua_tovector(info->L, -1);
  for (size_t i=0; i<LUA_VECTOR_SIZE; ++i) {
    WRITE_VALUE(*(f + i), float32);
  }
}

static void
u_vector(Info *info) {                                                 /* ... */
  if (info->u.upi.vector_components > LUA_VECTOR_SIZE) {
    eris_error(info, ERIS_ERR_TRUNC_SIZE);
  }

  eris_checkstack(info->L, 1);
  // Vectors are _specifically_ 32-bit floats.
  float v[LUA_VECTOR_SIZE];
  for (size_t i=0; i<LUA_VECTOR_SIZE; ++i) {
    v[i] = read_float32(info);
  }

#if LUA_VECTOR_SIZE == 4
  lua_pushvector(info->L, v[0], v[1], v[2], v[3]);                 /* ... vec */
#else
  lua_pushvector(info->L, v[0], v[1], v[2]);                       /* ... vec */
#endif

  eris_assert(lua_type(info->L, -1) == LUA_TVECTOR);
}


/** ======================================================================== */

static void
p_string(Info *info) {                                             /* ... str */
  size_t length;
  const char *value = lua_tolstring(info->L, -1, &length);
  WRITE_VALUE(length, size_t);
  WRITE_RAW(value, length);
}

static void
u_string(Info *info) {                                                 /* ... */
  eris_checkstack(info->L, 2);
  {
    /* TODO Can we avoid this copy somehow? (Without it getting too nasty) */
    const size_t length = READ_VALUE(size_t);
    char *value = (char*)lua_newuserdata(info->L, length * sizeof(char)); /* ... tmp */
    READ_RAW(value, length);
    lua_pushlstring(info->L, value, length);                   /* ... tmp str */
    lua_replace(info->L, -2);                                      /* ... str */
  }
  registerobject(info);

  eris_assert(lua_type(info->L, -1) == LUA_TSTRING);
}

/*
** ============================================================================
** Tables and userdata.
** ============================================================================
*/

static void
p_metatable(Info *info) {                                          /* ... obj */
  eris_checkstack(info->L, 1);
  pushpath(info, "@metatable");
  if (!lua_getmetatable(info->L, -1)) {                        /* ... obj mt? */
    lua_pushnil(info->L);                                      /* ... obj nil */
  }                                                         /* ... obj mt/nil */
  persist(info);                                            /* ... obj mt/nil */
  lua_pop(info->L, 1);                                             /* ... obj */
  poppath(info);
}

static void
u_metatable(Info *info) {                                          /* ... tbl */
  eris_checkstack(info->L, 1);
  pushpath(info, "@metatable");
  unpersist(info);                                         /* ... tbl mt/nil? */
  if (lua_istable(info->L, -1)) {                               /* ... tbl mt */
    lua_setmetatable(info->L, -2);                                 /* ... tbl */
  }
  else if (lua_isnil(info->L, -1)) {                           /* ... tbl nil */
    lua_pop(info->L, 1);                                           /* ... tbl */
  }
  else {                                                            /* tbl :( */
    eris_error(info, ERIS_ERR_METATABLE);
  }
  poppath(info);
}

/** ======================================================================== */

static void
p_literaltable(Info *info) {                                       /* ... tbl */
  eris_checkstack(info->L, 3);

  // write the original array and hash sizes so that we can ensure consistent
  // iteration order when the table is deserialized
  const TValue *table_tv = luaA_toobject(info->L, -1);
  eris_assert(ttistable(table_tv));
  Table* table = hvalue(table_tv);
  WRITE_VALUE(table->readonly, uint8_t);
  WRITE_VALUE(table->safeenv, uint8_t);
  WRITE_VALUE(table->sizearray, int);
  WRITE_VALUE(sizenode(table), int);

  int table_size = sizenode(table) + table->sizearray;
  /* Persist all key / value pairs. */
  for (int i = 0; i < table_size; ++i) {
    TValue key;
    TValue value;
    if (i < table->sizearray) {
      setnvalue(&key, i + 1);
      setobj(info->L, &value, &table->array[i]);
    } else {
      const LuaNode *node;
      int node_iter_idx = i - table->sizearray;
      if (table->iterorder) {
        // We're enforcing iteration order, figure out the node index to use
        // in iteration order.
        node_iter_idx = table->iterorder[node_iter_idx].node_idx;
        LUAU_ASSERT(node_iter_idx <= sizenode(table) && node_iter_idx >= -1);
        if (node_iter_idx != -1) {
          node = &table->node[node_iter_idx];
        } else {
          // This slot wasn't filled in the original iteration order, just use
          // the dummynode since it's a nil:nil node anyway.
          node = &luaH_dummynode;
        }
      } else {
        node = &table->node[node_iter_idx];
      }

      // We can't guarantee that these will be filled correctly until or unless
      // we get a lot smarter about how we deserialize tables. The order in
      // which you `nil` keys has an effect on which keys will still be present
      // with `nil` values, and which will have their keys `nil`ed implicitly.
      // That's not to mention the pitfalls with serializing GC-able dead keys.
      if (ttype(&node->key) == LUA_TDEADKEY || ttisnil(&node->val)) {
        setnilvalue(&key);
        setnilvalue(&value);
      } else {
        getnodekey(info->L, &key, node);
        setobj(info->L, &value, &node->val);
      }
    }

    luaA_pushobject(info->L, &key);                              /* ... tbl k */

    if (info->generatePath) {
      if (lua_type(info->L, -1) == LUA_TSTRING) {
        const char *str_key = lua_tostring(info->L, -1);
        pushpath(info, ".%s", str_key);
      }
      else {
        const char *str_key = luaL_tolstring(info->L, -1, nullptr);
        pushpath(info, "[%s]", str_key);
        lua_pop(info->L, 1);
      }
    }

    persist(info);                                               /* ... tbl k */
    lua_pop(info->L, 1);                                           /* ... tbl */
    luaA_pushobject(info->L, &value);                            /* ... tbl v */
    persist(info);                                               /* ... tbl v */
    lua_pop(info->L, 1);                                           /* ... tbl */
    poppath(info);
  }

  p_metatable(info);
}

static void
u_literaltable(Info *info) {                                           /* ... */
  eris_checkstack(info->L, 4);

  lua_newtable(info->L);                                           /* ... tbl */
  eris_ifassert(const int top = lua_gettop(info->L));

  /* Preregister table for handling of cycles (keys, values or metatable). */
  registerobject(info);

  bool read_only = READ_VALUE(uint8_t);
  bool safe_env = READ_VALUE(uint8_t);
  int array_size = READ_VALUE(int);
  int hash_size = READ_VALUE(int);

  /* Maintain a vector of keys in the order they were parsed so any existing
   * iterators won't be invalidated */
  // TODO: use lua vectors
  std::vector<std::pair<TValue, TValue>> ordered_keys;

  /* Unpersist all key / value pairs. */
  int total_elems = array_size + hash_size;
  for (int i=0; i<total_elems; ++i) {
    pushpath(info, "@key");
    unpersist(info);                                       /* ... tbl key/nil */
    poppath(info);

    if (info->generatePath) {
      if (lua_type(info->L, -1) == LUA_TSTRING) {
        const char *key = lua_tostring(info->L, -1);
        pushpath(info, ".%s", key);
      }
      else {
        const char *key = luaL_tolstring(info->L, -1, nullptr);
        pushpath(info, "[%s]", key);
        lua_pop(info->L, 1);
      }
    }

    unpersist(info);                                   /* ... tbl key? value? */

    // Store the keys and values for later insertion into the Table, it's fine to keep
    // these around here after popping since any GCable objects will also have a strong
    // reference in our reference table.
    ordered_keys.emplace_back(*luaA_toobject(info->L, -2), *luaA_toobject(info->L, -1));

    lua_pop(info->L, 2);                                           /* ... tbl */
    poppath(info);
  }

  eris_assert(top == lua_gettop(info->L));

  // Actually put things into the table
  Table *table = hvalue(luaA_toobject(info->L, -1));

  // Resize the array and hash portions so things will end up in the same place as the
  // original Table
  // Array resize has to happen first because it'll force the hash to be 0-sized if it
  // notices nothing is in it.
  luaH_resizearray(info->L, table, array_size);
  luaH_resizehash(info->L, table, hash_size);

  // Make sure the resize happened correctly
  eris_assert(array_size == table->sizearray);
  eris_assert(hash_size == sizenode(table));

  // push a table to (hopefully not) dedupe keys and store their expected iter index
  lua_newtable(info->L); /* tbl pos_tbl */

  int non_nil_nodes = 0;
  // For whatever reason, reverse insertion order is more likely to chain nodes in the
  // same way as the original Table's hash.
  for (ssize_t kv_idx = (ssize_t)ordered_keys.size() - 1; kv_idx >= 0; --kv_idx) {
    const auto &kv_it = &ordered_keys[kv_idx];
    if (ttisnil(&kv_it->first)) {
      // we only keep these nil keys for iteration order preservation, we can't
      // actually insert them into a Table!
      continue;
    }

    if (kv_idx >= array_size) {
      ++non_nil_nodes;
      // keep track of the key's original iter pos within the hash
      luaA_pushobject(info->L, &kv_it->first);         /* ... tbl pos_tbl key */
      lua_pushinteger(info->L, (int)(kv_idx - array_size));
                                                   /* ... tbl pos_tbl key pos */
      lua_rawset(info->L, -3);                             /* ... tbl pos_tbl */
    }

    // We still want to assign keys with nil values though, keys with tombstones are
    // distinct from keys that aren't present and may affect chaining.
    luaA_pushobject(info->L, &kv_it->first);           /* ... tbl pos_tbl key */
    luaA_pushobject(info->L, &kv_it->second);      /* ... tbl pos_tbl key val */

    lua_rawset(info->L, -4);                               /* ... tbl pos_tbl */
  }

  eris_assert(top + 1 == lua_gettop(info->L));

  // If array has changed size then things are badly broken and we can't maintain
  // iterators
  eris_assert(array_size == table->sizearray);
  // hash size may actually have grown due to bucketing differences, but it
  // better not have shrunk.
  int actual_hash_size = sizenode(table);
  eris_assert(hash_size <= actual_hash_size);

  // If our hashtable ends up with fewer entries than it originally had, then our
  // table somehow shrank during deserialization (possibly due to a perms table issue.)
  lua_pushnil(info->L);                           /* ... tbl pos_tbl key(nil) */
  int new_non_nil_nodes = 0;
  while(lua_next(info->L, -2)) {                       /* ... tbl pos_tbl key */
                                                   /* ... tbl pos_tbl key val */
    ++new_non_nil_nodes;
    lua_pop(info->L, 1);                               /* ... tbl pos_tbl key */
  }
                                                           /* ... tbl pos_tbl */
  eris_assert(new_non_nil_nodes == non_nil_nodes);
  eris_assert(top + 1 == lua_gettop(info->L));

  // If this table has a hash component we need to be careful about iteration order
  bool override_iterorder = false;
  if (table->lsizenode) {
    if (hash_size != actual_hash_size) {
      // If bucketing changed, then we obviously need to override.
      override_iterorder = true;
    } else {
      // We don't need to enforce an iteration order if things are in the order we
      // want them to be in anyway, verify that first.
      for (int i=0; i<hash_size; ++i) {
        const TValue *expected = &ordered_keys[i + array_size].first;
        if (!luaO_rawequalKey(gkey(gnode(table, i)), expected)) {
          override_iterorder = true;
          break;
        }
      }
    }
  }

  if (override_iterorder) {
    // force iteration to happen in a particular order until the table is mutated in
    // a way that would invalidate the order.
    luaH_overrideiterorder(info->L, table, true);

    for(int node_idx = 0; node_idx<actual_hash_size; ++node_idx) {
      LuaNode *node = &table->node[node_idx];
      // Lua will never stop iteration on a nil node, so nil nodes don't need to
      // point to their nil equivalent within the hash. They can just point to nothing.
      if (ttisnil(gkey(node))) {
        continue;
      }

      // Find the key's original iter pos
      TValue key_val;
      getnodekey(info->L, &key_val, node);
      luaA_pushobject(info->L, &key_val);              /* ... tbl pos_tbl key */
      lua_rawget(info->L, -2);                         /* ... tbl pos_tbl val */
      // if the lookup failed then something is seriously wrong.
      eris_assert(lua_type(info->L, -1) == LUA_TNUMBER);

      int iteridx = lua_tointeger(info->L, -1);
      lua_pop(info->L, 1);                                 /* ... tbl pos_tbl */

      table->iterorder[node_idx].node_to_iterorder_idx = iteridx;
      table->iterorder[iteridx].node_idx = (int)(node_idx);
    }
  }

  lua_pop(info->L, 1);                                             /* ... tbl */

  eris_ifassert(int cur_top = lua_gettop(info->L));
  eris_assert(top == cur_top);

  u_metatable(info);                                               /* ... tbl */

  // Set these last so we don't trigger any errors
  if (read_only)
    lua_setreadonly(info->L, -1, true);
  if (safe_env)
    lua_setsafeenv(info->L, -1, true);
}

/** ======================================================================== */

static void
p_literaluserdata(Info *info) {                                  /* ... udata */
  uint8_t utag = lua_userdatatag(info->L, -1);
  eris_assert(utag == UTAG_PROXY);
  const size_t size = lua_objlen(info->L, -1);
  const void *value = lua_touserdata(info->L, -1);
  WRITE_VALUE(utag, uint8_t);
  WRITE_VALUE(size, size_t);
  WRITE_RAW(value, size);
  p_metatable(info);                                             /* ... udata */
}

static void
u_literaluserdata(Info *info) {                                        /* ... */
  eris_checkstack(info->L, 1);
  {
    uint8_t utag = READ_VALUE(uint8_t);
    size_t size = READ_VALUE(size_t);
    void *value = lua_newuserdatatagged(info->L, size, utag);    /* ... udata */
    READ_RAW(value, size);                                       /* ... udata */
  }
  registerobject(info);
  u_metatable(info);
}

/** ======================================================================== */

typedef void (*Callback) (Info*);

static void
p_special(Info *info, Callback literal) {                          /* ... obj */
  int allow = (lua_type(info->L, -1) == LUA_TTABLE);
  eris_checkstack(info->L, 4);

  /* Check whether we should persist literally, or via the metafunction. */
  if (lua_getmetatable(info->L, -1)) {                          /* ... obj mt */
    lua_pushstring(info->L, info->u.pi.metafield);         /* ... obj mt pkey */
    lua_rawget(info->L, -2);                           /* ... obj mt persist? */
    switch (lua_type(info->L, -1)) {
      /* No entry, act according to default. */
      case LUA_TNIL:                                        /* ... obj mt nil */
        lua_pop(info->L, 2);                                       /* ... obj */
        break;

      /* Boolean value, tells us whether allowed or not. */
      case LUA_TBOOLEAN:                                   /* ... obj mt bool */
        allow = lua_toboolean(info->L, -1);
        lua_pop(info->L, 2);                                       /* ... obj */
        break;

      /* Function value, call it and don't persist literally. */
      case LUA_TFUNCTION:                                  /* ... obj mt func */
        lua_replace(info->L, -2);                             /* ... obj func */
        lua_pushvalue(info->L, -2);                       /* ... obj func obj */

        lua_call(info->L, 1, 1);                           /* ... obj func? */

        if (!lua_isfunction(info->L, -1)) {                     /* ... obj :( */
          eris_error(info, ERIS_ERR_SPER_FUNC, info->u.pi.metafield);
        }                                                     /* ... obj func */

        /* Special persistence, call this function when unpersisting. */
        WRITE_VALUE(true, uint8_t);
        persist(info);                                        /* ... obj func */
        lua_pop(info->L, 1);                                       /* ... obj */
        return;
      default:                                               /* ... obj mt :( */
        eris_error(info, ERIS_ERR_SPER_TYPE, info->u.pi.metafield);
        return; /* not reached */
    }
  }

  if (allow) {
    /* Not special but literally persisted object. */
    WRITE_VALUE(false, uint8_t);
    literal(info);                                                 /* ... obj */
  }
  else if (lua_type(info->L, -1) == LUA_TTABLE) {
    eris_error(info, ERIS_ERR_SPER_PROT);
  }
  else if (lua_userdatatag(info->L, -1) == UTAG_PROXY) {
    // Luau's userdata proxies are allowed because they're
    // literally just containers for metatables
    WRITE_VALUE(false, uint8_t);
    literal(info);                                                 /* ... obj */
  }
  else {
    eris_error(info, ERIS_ERR_USERDATA);
  }
}

static void
u_special(Info *info, int type, Callback literal) {                    /* ... */
  eris_checkstack(info->L, 2);
  if (READ_VALUE(uint8_t)) {
    int reference;
    /* Reserve entry in the reftable before unpersisting the function to keep
     * the reference order intact. We can set this to nil at first, because
     * there's no way the special function would access this. */
    lua_pushnil(info->L);                                          /* ... nil */
    reference = registerobject(info);
    lua_pop(info->L, 1);                                               /* ... */
    /* Increment reference counter by one to compensate for the increment when
     * persisting a special object. */
    unpersist(info);                                           /* ... spfunc? */
    if (!lua_isfunction(info->L, -1)) {                             /* ... :( */
      eris_error(info, ERIS_ERR_SPER_UFUNC);
    }                                                           /* ... spfunc */

    lua_call(info->L, 0, 1);                                    /* ... obj? */

    if (lua_type(info->L, -1) != type) {                            /* ... :( */
      const char *want = kTypenames[type];
      const char *have = kTypenames[lua_type(info->L, -1)];
      eris_error(info, ERIS_ERR_SPER_LOAD, want, have);
    }                                                              /* ... obj */

    /* Update the reftable entry. */
    lua_pushvalue(info->L, -1);                                /* ... obj obj */
    lua_rawseti(info->L, 2, reference);                            /* ... obj */
  }
  else {
    literal(info);                                                 /* ... obj */
  }
}

/** ======================================================================== */

static void
p_table(Info *info) {                                              /* ... tbl */
  p_special(info, p_literaltable);                                 /* ... tbl */
}

static void
u_table(Info *info) {                                                  /* ... */
  u_special(info, LUA_TTABLE, u_literaltable);                     /* ... tbl */

  eris_assert(lua_type(info->L, -1) == LUA_TTABLE);
}

/** ======================================================================== */

static void
p_userdata(Info *info) {                            /* perms reftbl ... udata */
  p_special(info, p_literaluserdata);
}

static void
u_userdata(Info *info) {                                               /* ... */
  u_special(info, LUA_TUSERDATA, u_literaluserdata);             /* ... udata */

  eris_assert(lua_type(info->L, -1) == LUA_TUSERDATA);
}

/*
** ============================================================================
** Closures and threads.
** ============================================================================
*/

/* We track the actual upvalues themselves by pushing their "id" (meaning a
 * pointer to them) as lightuserdata to the reftable. This is safe because
 * lightuserdata will not normally end up in there, because simple value types
 * are always persisted directly (because that'll be just as large, memory-
 * wise as when pointing to the first instance). Same for protos. */

static void
p_proto(Info *info) {                                            /* ... proto */
  int i;
  const Proto *p = (Proto*)lua_touserdata(info->L, -1);
  eris_checkstack(info->L, 3);

  /* Write function source code */
  pushtstring(info->L, p->source);
  persist(info);
  lua_pop(info->L, 1);

  /* Write general information. */
  WRITE_VALUE(p->bytecodeid, int);

  WRITE_VALUE(p->maxstacksize, uint8_t);
  WRITE_VALUE(p->numparams, uint8_t);
  WRITE_VALUE(p->nups, uint8_t);
  WRITE_VALUE(p->is_vararg, uint8_t);

  /* Write byte code. */
  WRITE_VALUE(p->sizecode, int);
  WRITE(p->code, p->sizecode, Instruction);

  /* Write constants. */
  WRITE_VALUE(p->sizek, int);
  pushpath(info, ".constants");
  for (i = 0; i < p->sizek; ++i) {
    pushpath(info, "[%d]", i);
    eris_setobj(info->L, info->L->top++, &p->k[i]);      /* ... lcl proto obj */
    persist(info);                                       /* ... lcl proto obj */
    lua_pop(info->L, 1);                                     /* ... lcl proto */
    poppath(info);
  }
  poppath(info);

  /* Write child protos. */
  WRITE_VALUE(p->sizep, int);
  pushpath(info, ".protos");
  for (i = 0; i < p->sizep; ++i) {
    pushpath(info, "[%d]", i);
    lua_pushlightuserdata(info->L, p->p[i]);           /* ... lcl proto proto */
    lua_pushvalue(info->L, -1);                  /* ... lcl proto proto proto */
    persist_keyed(info, LUA_TPROTO);                   /* ... lcl proto proto */
    lua_pop(info->L, 1);                                     /* ... lcl proto */
    poppath(info);
  }
  poppath(info);

  WRITE_VALUE(p->linedefined, int);
  pushtstring(info->L, p->debugname);
  persist(info);
  lua_pop(info->L, 1);

  // TODO: write line and debug info, just say we don't have either for now.
  WRITE_VALUE(0, uint8_t);
  WRITE_VALUE(0, uint8_t);
//  /* Write upvalues. */
//  WRITE_VALUE(p->sizeupvalues, int);
//  for (i = 0; i < p->sizeupvalues; ++i) {
////    WRITE_VALUE(p->upvalues[i].instack, uint8_t);
////    WRITE_VALUE(p->upvalues[i].idx, uint8_t);
//    WRITE_VALUE(0, uint8_t);
//    WRITE_VALUE(0, uint8_t);
//  }
//
//  /* If we don't have to persist debug information skip the rest. */
//  WRITE_VALUE(info->u.pi.writeDebugInfo, uint8_t);
//  if (!info->u.pi.writeDebugInfo) {
//    return;
//  }
//
//  /* Write function source code. */
//  pushtstring(info->L, p->source);                    /* ... lcl proto source */
//  persist(info);                                      /* ... lcl proto source */
//  lua_pop(info->L, 1);                                       /* ... lcl proto */
//
//  /* Write line information. */
//  WRITE_VALUE(p->sizelineinfo, int);
//  WRITE(p->lineinfo, p->sizelineinfo, int);
//
//  /* Write locals info. */
//  WRITE_VALUE(p->sizelocvars, int);
//  pushpath(info, ".locvars");
//  for (i = 0; i < p->sizelocvars; ++i) {
//    pushpath(info, "[%d]", i);
//    WRITE_VALUE(p->locvars[i].startpc, int);
//    WRITE_VALUE(p->locvars[i].endpc, int);
//    pushtstring(info->L, p->locvars[i].varname);     /* ... lcl proto varname */
//    persist(info);                                   /* ... lcl proto varname */
//    lua_pop(info->L, 1);                                     /* ... lcl proto */
//    poppath(info);
//  }
//  poppath(info);
//
//  /* Write upvalue names. */
//  pushpath(info, ".upvalnames");
//  for (i = 0; i < p->sizeupvalues; ++i) {
//    pushpath(info, "[%d]", i);
//    pushtstring(info->L, p->upvalues[i]);               /* ... lcl proto name */
//    persist(info);                                      /* ... lcl proto name */
//    lua_pop(info->L, 1);                                     /* ... lcl proto */
//    poppath(info);
//  }
//  poppath(info);
}

static void
u_proto(Info *info) {                                            /* ... proto */
  int i, n;
  Proto *p = (Proto*)lua_touserdata(info->L, -1);
  eris_assert(p);

  eris_checkstack(info->L, 2);

  /* Preregister proto for handling of cycles (probably impossible, but
   * maybe via the constants of the proto... not worth taking the risk). */
  registerobject(info);

  /* Read function source code. */
  unpersist(info);                                           /* ... proto str */
  copytstring(info->L, &p->source);
  lua_pop(info->L, 1);                                           /* ... proto */

  /* Read general information. */
  p->bytecodeid = READ_VALUE(int);

  p->maxstacksize = READ_VALUE(uint8_t);
  p->numparams = READ_VALUE(uint8_t);
  p->nups = READ_VALUE(uint8_t);
  p->is_vararg = READ_VALUE(uint8_t);

  /* Read byte code. */
  p->sizecode = READ_VALUE(int);
  eris_reallocvector(info->L, p->code, 0, p->sizecode, Instruction);
  READ(p->code, p->sizecode, Instruction);

  /* Read constants. */
  p->sizek = READ_VALUE(int);
  eris_reallocvector(info->L, p->k, 0, p->sizek, TValue);
  /* Set all values to nil to avoid confusing the GC. */
  for (i = 0, n = p->sizek; i < n; ++i) {
    eris_setnilvalue(&p->k[i]);
  }
  pushpath(info, ".constants");
  for (i = 0, n = p->sizek; i < n; ++i) {
    pushpath(info, "[%d]", i);
    // TODO: Import constants aren't supported yet, but I don't know
    //  if they're necessary with how we do constant serialization.
    //  What are the side-effects of an import?
    unpersist(info);                                         /* ... proto obj */
    eris_setobj(info->L, &p->k[i], info->L->top - 1);
    lua_pop(info->L, 1);                                         /* ... proto */
    poppath(info);
  }
  poppath(info);

  /* Read child protos. */
  p->sizep = READ_VALUE(int);
  eris_reallocvector(info->L, p->p, 0, p->sizep, Proto*);
  /* Null all entries to avoid confusing the GC. */
  memset(p->p, 0, p->sizep * sizeof(Proto*));
  pushpath(info, ".protos");
  for (i = 0, n = p->sizep; i < n; ++i) {
    Proto *cp;
    pushpath(info, "[%d]", i);
    p->p[i] = eris_newproto(info->L);
    lua_pushlightuserdata(info->L, (void*)p->p[i]);       /* ... proto nproto */
    unpersist(info);                        /* ... proto nproto nproto/oproto */
    cp = (Proto*)lua_touserdata(info->L, -1);
    if (cp != p->p[i]) {                           /* ... proto nproto oproto */
      /* Just overwrite it, GC will clean this up. */
      p->p[i] = cp;
    }
    lua_pop(info->L, 2);                                         /* ... proto */
    poppath(info);
  }
  poppath(info);

  p->linedefined = READ_VALUE(int);
  unpersist(info);
  copytstring(info->L, &p->debugname);
  lua_pop(info->L, 1);

  // TODO: line info
  /* Read line info if any is present. */
  if (READ_VALUE(uint8_t)) {
    eris_assert(!"reading line info isn't supported");
  }

  // TODO: debug info
  /* Read debug information if any is present. */
  if (READ_VALUE(uint8_t)) {
    eris_assert(!"reading debug info isn't supported");
  }
//
//  /* Read debug information if any is present. */
//  if (!READ_VALUE(uint8_t)) {
//    /* Match stack behaviour of alternative branch. */
//    lua_pushvalue(info->L, -1);                            /* ... proto proto */
//    return;
//  }
//
//  /* Read line information. */
//  p->sizelineinfo = READ_VALUE(int);
//  eris_reallocvector(info->L, p->lineinfo, 0, p->sizelineinfo, uint8_t);
//  READ(p->lineinfo, p->sizelineinfo, int);
//  eris_reallocvector(info->L, p->abslineinfo, 0, p->sizelineinfo, uint8_t);
//  READ(p->lineinfo, p->abslineinfo, int);
//
//  /* Read locals info. */
//  p->sizelocvars = READ_VALUE(int);
//  eris_reallocvector(info->L, p->locvars, 0, p->sizelocvars, LocVar);
//  /* Null the variable names to avoid confusing the GC. */
//  for (i = 0, n = p->sizelocvars; i < n; ++i) {
//    p->locvars[i].varname = nullptr;
//  }
//  pushpath(info, ".locvars");
//  for (i = 0, n = p->sizelocvars; i < n; ++i) {
//    pushpath(info, "[%d]", i);
//    p->locvars[i].startpc = READ_VALUE(int);
//    p->locvars[i].endpc = READ_VALUE(int);
//    unpersist(info);                                         /* ... proto str */
//    copytstring(info->L, &p->locvars[i].varname);
//    lua_pop(info->L, 1);                                         /* ... proto */
//    poppath(info);
//  }
//  poppath(info);
//
//  /* Read upvalue names. */
//  pushpath(info, ".upvalnames");
//  for (i = 0, n = p->sizeupvalues; i < n; ++i) {
//    pushpath(info, "[%d]", i);
//    unpersist(info);                                         /* ... proto str */
//    copytstring(info->L, &p->upvalues[i]);
//    lua_pop(info->L, 1);                                         /* ... proto */
//    poppath(info);
//  }
//  poppath(info);

  lua_pushvalue(info->L, -1);                              /* ... proto proto */

  eris_assert(lua_type(info->L, -1) == LUA_TLIGHTUSERDATA);
}

/** ======================================================================== */

static void
p_upval(Info *info) {                                              /* ... obj */
  persist(info);                                                   /* ... obj */
}

static void
u_upval(Info *info) {                                                  /* ... */
  eris_checkstack(info->L, 2);

  /* Create the table we use to store the stack location to the upval (1+2),
   * the value of the upval (3) and any references to the upvalue's value (4+).
   * References are stored as two entries each, the actual closure holding the
   * upvalue, and the index of the upvalue in that closure. */
  lua_createtable(info->L, 5, 0);                                  /* ... tbl */
  registerobject(info);
  unpersist(info);                                             /* ... tbl obj */
  lua_rawseti(info->L, -2, UVTVAL);                                /* ... tbl */

  eris_assert(lua_type(info->L, -1) == LUA_TTABLE);
}

/** ======================================================================== */

/* For Lua closures we write the upvalue ID, which is usually the memory
 * address at which it is stored. This is used to tell which upvalues are
 * identical when unpersisting. */
/* In either case we store the upvale *values*, i.e. the actual objects they
 * point to. As in Pluto, we will restore any upvalues of Lua closures as
 * closed as first, i.e. the upvalue will store the TValue itself. When
 * loading a thread containing the upvalue (meaning it's the actual owner of
 * the upvalue) we open it, i.e. we point it to the thread's upvalue list.
 * For C closures, upvalues are always closed. */
static void
p_closure(Info *info) {                              /* perms reftbl ... func */
  int nup;
  Closure *cl = clvalue(luaA_toobject(info->L, -1));
  eris_checkstack(info->L, 2);

  if (info->u.pi.persistingCFunc) {
    /* This is a case where we tried to persist a c function via the permtable
     * but failed, causing us to be re-passed the light userdata we sent into
     * persist_keyed().
     *
     * We cannot persist these. They have to be handled via the permtable. */
    eris_error(info, ERIS_ERR_CFUNC, cl->c.f, cl->c.debugname);
    return;
  }

  /* Mark whether it is a C closure. */
  WRITE_VALUE(cl->isC, uint8_t);

  /* Write the upvalue count first, since we have to know it when creating
   * a new closure when unpersisting. */
  WRITE_VALUE(cl->nupvalues, uint8_t);

  // Technically C closures, like Lua closures have an `env`, but it seems
  // that in practice these are always `GL->gt`, and `getfenv()` is
  // special-cased to lie about the fenv. Don't need to store an `env` for
  // them, then.
  // NOTE: Luau only has "C" and "Lua" closures, no light C functions!
  if (cl->isC) {
    /* If we got here it means that there was no closure instance for this C
     * function in the perms table. That may be the case for dynamically
     * created closures like those returned by `coroutine.wrap()`. As a last
     * resort, try seeing if we have _any_ closure that points to the same
     * underlying C function by pushing its pointer as a lightuserdata.
     *
     * Note that this isn't foolproof as the perms table entry is keyed solely
     * on the function pointer and any continuation function pointer. In
     * practice this shouldn't matter because a C closure with the same
     * pointer but different continuation pointer seems to be unusual.
     */
    info->u.pi.persistingCFunc = true;
    lua_pushlightuserdata(info->L, (void *)cl->c.f);
                                              /* perms reftbl ... ccl cfunc */
    persist_keyed(info, LUA_TFUNCTION);             /* perms reftbl ... ccl */
    info->u.pi.persistingCFunc = false;

    /* Persist the upvalues. Since for C closures all upvalues are always
     * closed we can just write the actual values. */
    pushpath(info, ".upvalues");
    for (nup = 1; nup <= cl->nupvalues; ++nup) {
      pushpath(info, "[%d]", nup);
      lua_getupvalue(info->L, -1, nup);         /* perms reftbl ... ccl obj */
      persist(info);                            /* perms reftbl ... ccl obj */
      lua_pop(info->L, 1);                          /* perms reftbl ... ccl */
      poppath(info);
    }
    poppath(info);
  }
  /* Lua function */
  else {               /* perms reftbl ... lcl */
    pushpath(info, ".env");
    /* Persist the environment (globals) table for the closure */
    lua_getfenv(info->L, -1);                   /* perms reftbl ... lcl env */
    persist(info);
    lua_pop(info->L, 1);                            /* perms reftbl ... lcl */
    poppath(info);

    /* Persist the function's prototype. Pass the proto as a parameter to
     * p_proto so that it can access it and register it in the ref table. */
    pushpath(info, ".proto");
    lua_pushlightuserdata(info->L, cl->l.p);  /* perms reftbl ... lcl proto */
    lua_pushvalue(info->L, -1);         /* perms reftbl ... lcl proto proto */
    persist_keyed(info, LUA_TPROTO);          /* perms reftbl ... lcl proto */
    lua_pop(info->L, 1);                            /* perms reftbl ... lcl */
    poppath(info);

    /* Persist the upvalues. We pretend to write these as their own type,
     * to get proper identity preservation. We also pass them as a parameter
     * to p_upval so it can register the upvalue in the reference table. */
    pushpath(info, ".upvalues");
    for (nup = 1; nup <= cl->nupvalues; ++nup) {
      // internally pushes the upvalue's value onto the stack
      const char *name = lua_getupvalue(info->L, -1, nup);
                                             /* perms reftbl ... lcl uv_val */
      // name is unlikely to be useful, but it shouldn't be null
      eris_assert(name != nullptr);
      pushpath(info, "[%d]", nup);

      // strictly used as a key for finding shared upvalue references!
      lua_pushlightuserdata(info->L, eris_getupvalueid_safe(info, -2, nup));
                                       /* perms reftbl ... lcl uv_val uv_id */

      persist_keyed(info, LUA_TUPVAL);       /* perms reftbl ... lcl uv_val */
      lua_pop(info->L, 1);                          /* perms reftbl ... lcl */
      poppath(info);
      // stack should be back to normal
      eris_assert(lua_type(info->L, -1) == LUA_TFUNCTION);
    }
    poppath(info);
  }
}

static void
u_closure(Info *info) {                                                /* ... */
  int nup;
  bool isCClosure = READ_VALUE(uint8_t);
  uint8_t nups = READ_VALUE(uint8_t);
  Closure *cl;

  if (isCClosure) {
    /* Reserve reference for the closure to avoid light C function or its
     * perm table key going first. */
    const int reference = ++(info->refcount);

    eris_checkstack(info->L, nups);

    /* Read the C function from the permanents table. */
    unpersist(info);                                             /* ... cfunc */
    if (!lua_iscfunction(info->L, -1)) {
      eris_error(info, ERIS_ERR_UCFUNC, kTypenames[lua_type(info->L, -1)]);
    }
    cl = clvalue(luaA_toobject(info->L, -1));
    if (!cl->c.f) {
      eris_error(info, ERIS_ERR_UCFUNCNULL);
    }
    // Ok to pop here, there's still a strong reference to the base closure
    // in the perms table.
    lua_pop(info->L, 1);

    /* Now this is a little roundabout, but we want to create the closure
     * before unpersisting the actual upvalues to avoid cycles. So we have to
     * create it with all nil first, then fill the upvalues in afterwards. */
    for (nup = 1; nup <= nups; ++nup) {
      lua_pushnil(info->L);                        /* ... nil[1] ... nil[nup] */
    }
    lua_pushcclosurek(info->L, cl->c.f, cl->c.debugname, nups, cl->c.cont);
                                                                   /* ... ccl */
    /* Create the entry in the reftable. */
    lua_pushvalue(info->L, -1);                   /* perms reftbl ... ccl ccl */
    lua_rawseti(info->L, REFTIDX, reference);         /* perms reftbl ... ccl */

    /* Unpersist actual upvalues. */
    pushpath(info, ".upvalues");
    for (nup = 1; nup <= nups; ++nup) {
      pushpath(info, "[%d]", nup);
      unpersist(info);                                         /* ... ccl obj */
      lua_setupvalue(info->L, -2, nup);                            /* ... ccl */
      poppath(info);
    }
    poppath(info);
                                                                   /* ... ccl */
  }
  else {
    Proto *p;

    eris_checkstack(info->L, 4);

    /* Create closure and anchor it on the stack (avoid collection via GC). */
    p = eris_newproto(info->L);
    // Pre-set this so luaC_validate() / validateclosure() doesn't explode if
    // we error while deserializing the proto. We expect it to get clobbered.
    p->nups = nups;
    // `info->L->gt` almost definitely isn't the proper env for this closure,
    // we'll replace it with the real one later.
    cl = luaF_newLclosure(info->L, nups, info->L->gt, p);
    setclvalue(info->L, info->L->top, cl);                         /* ... lcl */
    incr_top(info->L);

    /* Preregister closure for handling of cycles (upvalues). */
    registerobject(info);

    pushpath(info, ".fenv");
    /* Read env dict, this is generally the proxy table for globals */
    unpersist(info);                                            /* ... lcl gt */
    // Replace the placeholder env we had on the closure
    lua_setfenv(info->L, -2);                                      /* ... lcl */
    poppath(info);

    /* Read prototype. In general, we create protos (and upvalues) before
     * trying to read them and pass a pointer to the instance along to the
     * unpersist function. This way the instance is safely hooked up to an
     * object, so we don't have to worry about it getting GCed. */
    pushpath(info, ".proto");
    /* Push the proto into which to unpersist as a parameter to u_proto. */
    lua_pushlightuserdata(info->L, cl->l.p);                /* ... lcl nproto */
    unpersist(info);                          /* ... lcl nproto nproto/oproto */
    eris_assert(lua_type(info->L, -1) == LUA_TLIGHTUSERDATA);
    /* The proto we have now may differ, if we already unpersisted it before.
     * In that case we now have a reference to the originally unpersisted
     * proto so we'll use that. */
    p = (Proto*)lua_touserdata(info->L, -1);
    if (p != cl->l.p) {                              /* ... lcl nproto oproto */
      /* Just overwrite the old one, GC will clean this up. */
      cl->l.p = p;
    }
    lua_pop(info->L, 2);                                           /* ... lcl */
    eris_assert(cl->l.p->code != nullptr);
    eris_assert(cl->l.p->nups == nups);
    cl->stacksize = p->maxstacksize;
    poppath(info);

    /* Unpersist all upvalues. */
    pushpath(info, ".upvalues");
    for (nup = 1; nup <= nups; ++nup) {
      TValue *upval_cont = &cl->l.uprefs[nup - 1];
      /* Get the actual name of the upvalue, if possible. */
      if (p->upvalues && p->upvalues[nup - 1]) {
        pushpath(info, "[%s]", getstr(p->upvalues[nup - 1]));
      }
      else {
        pushpath(info, "[%d]", nup);
      }

      // upval will be unpersisted as a table describing the upval
      unpersist(info);                                         /* ... lcl tbl */
      eris_assert(lua_type(info->L, -1) == LUA_TTABLE);
      lua_rawgeti(info->L, -1, UVTOCL);               /* ... lcl tbl olcl/nil */
      if (lua_isnil(info->L, -1)) {                        /* ... lcl tbl nil */
        // Don't have an existing closure to pull this upval from, create an upval.
        lua_pop(info->L, 1);                                   /* ... lcl tbl */
        lua_pushvalue(info->L, -2);                        /* ... lcl tbl lcl */
        lua_rawseti(info->L, -2, UVTOCL);                      /* ... lcl tbl */
        lua_pushinteger(info->L, nup);                     /* ... lcl tbl nup */
        lua_rawseti(info->L, -2, UVTONU);                      /* ... lcl tbl */
        setupvalue(info->L, upval_cont, luaF_newupval(info->L));
      }
      else {                                              /* ... lcl tbl olcl */
        // This upval was already referenced in another closure, pull it off.
        Closure *ocl;
        int onup;
        eris_assert(lua_type(info->L, -1) == LUA_TFUNCTION);
        ocl = clvalue(info->L->top - 1);
        lua_pop(info->L, 1);                                   /* ... lcl tbl */
        lua_rawgeti(info->L, -1, UVTONU);                 /* ... lcl tbl onup */
        eris_assert(lua_type(info->L, -1) == LUA_TNUMBER);
        onup = lua_tointeger(info->L, -1);
        lua_pop(info->L, 1);                                   /* ... lcl tbl */
        // _not_ setupvalue(), we want the tvalue pointers to be the same!
        setobj(info->L, upval_cont, &ocl->l.uprefs[onup - 1]);
      }

      eris_assert(ttype(upval_cont) == LUA_TUPVAL);
      UpVal *uv = &upval_cont->value.gc->uv;
      luaC_objbarrier(info->L, cl, uv);

      /* Set the upvalue's actual value and add our reference to the upvalue to
       * the list, for reference patching if we have to open the upvalue in
       * u_thread. Either is only necessary if the upvalue is still closed. */
      if (!upisopen(uv)) {
        int i;
        /* Always update the value of the upvalue's value for closed upvalues,
         * even if we re-used one - if we had a cycle, it might have been
         * incorrectly initialized to nil before (or rather, not yet set). */
        lua_rawgeti(info->L, -1, UVTVAL);                  /* ... lcl tbl obj */
        eris_setobj(info->L, &uv->u.value, info->L->top - 1);
        lua_pop(info->L, 1);                                   /* ... lcl tbl */

        lua_pushinteger(info->L, nup);                     /* ... lcl tbl nup */
        lua_pushvalue(info->L, -3);                    /* ... lcl tbl nup lcl */
        if (lua_objlen(info->L, -3) >= UVTVAL) {
          /* Got a valid sequence (value already set), insert at the end. */
          i = lua_objlen(info->L, -3);
          lua_rawseti(info->L, -3, i + 1);                 /* ... lcl tbl nup */
          lua_rawseti(info->L, -2, i + 2);                     /* ... lcl tbl */
        }
        else {                                         /* ... lcl tbl nup lcl */
          /* Find where to insert. This can happen if we have cycles, in which
           * case the table is not fully initialized at this point, i.e. the
           * value is not in it, yet (we work around that by always setting it,
           * as seen above). */
          for (i = UVTREF;; i += 2) {
            lua_rawgeti(info->L, -3, i);       /* ... lcl tbl nup lcl lcl/nil */
            if (lua_isnil(info->L, -1)) {          /* ... lcl tbl nup lcl nil */
              lua_pop(info->L, 1);                     /* ... lcl tbl nup lcl */
              lua_rawseti(info->L, -3, i);                 /* ... lcl tbl nup */
              lua_rawseti(info->L, -2, i + 1);                 /* ... lcl tbl */
              break;
            }
            else {
              lua_pop(info->L, 1);                     /* ... lcl tbl nup lcl */
            }
          }                                                    /* ... lcl tbl */
        }
      }

      lua_pop(info->L, 1);                                         /* ... lcl */
      poppath(info);
    }
    poppath(info);

  }

  eris_assert(lua_type(info->L, -1) == LUA_TFUNCTION);
}

/** ======================================================================== */

static void
p_thread(Info *info) {                                          /* ... thread */
  eris_checkstack(info->L, 3);
  lua_State* thread = lua_tothread(info->L, -1);
  size_t level = 0, total = thread->top - thread->stack;
  CallInfo *ci;
  UpVal *uv;

  /* We cannot persist any running threads, because by definition we *are* that
   * running thread. And we use the stack. So yeah, really not a good idea. */
  if (thread == info->L) {
    eris_error(info, ERIS_ERR_THREAD);
    return; /* not reached */
  }

  /* Persist the globals table for the thread */
  lua_getfenv(info->L, -1);                                  /* ... thread gt */
  persist(info);
  lua_pop(info->L, 1);                                          /* ... thread */

  /* Persist the stack. Save the total size and used space first. */
  WRITE_VALUE(thread->stacksize, int);
  WRITE_VALUE(total, size_t);

  /* The Lua stack looks like this:
   * stack ... top ... stack_last
   * Where stack <= top <= stack_last, and "top" actually being the first free
   * element, i.e. there's nothing stored there. So we stop one below that. */
  pushpath(info, ".stack");
  lua_pushnil(info->L);                                     /* ... thread nil */
  /* Since the thread's stack may be re-allocated in the meantime, we cannot
   * use pointer arithmetic here (i.e. o = thread->stack; ...; ++o). Instead we
   * have to keep track of our position in the stack directly (which we do for
   * the path info anyway) and compute the actual address each time.
   */
  for (; level < total; ++level) {
    pushpath(info, "[%d]", level);
    eris_setobj(info->L, info->L->top - 1, thread->stack + level);
                                                            /* ... thread obj */
    persist(info);                                          /* ... thread obj */
    poppath(info);
  }
  lua_pop(info->L, 1);                                          /* ... thread */
  poppath(info);

  /* thread->oldpc always seems to be uninitialized, at least gdb always shows
   * it as 0xbaadf00d when I set a breakpoint here. */

  /* Write general information. */
  WRITE_VALUE(thread->status, uint8_t);
//  WRITE_VALUE(eris_savestackidx(thread,
//    eris_restorestack(thread, thread->errfunc)), size_t);
  // no err func!
  WRITE_VALUE(0, size_t);
  /* These are only used while a thread is being executed or can be deduced:
  WRITE_VALUE(thread->nCcalls, uint16_t);
  WRITE_VALUE(thread->allowhook, uint8_t); */

  /* Hooks are not supported, bloody can of worms, those.
  WRITE_VALUE(thread->hookmask, uint8_t);
  WRITE_VALUE(thread->basehookcount, int);
  WRITE_VALUE(thread->hookcount, int); */

  /* Write call information (stack frames). In 5.2 CallInfo is stored in a
   * linked list that originates in thead.base_ci. Upon initialization the
   * thread.ci is set to thread.base_ci. During thread calls this is extended
   * and always represents the tail of the callstack, though not necessarily of
   * the linked list (which can be longer if the callstack was deeper earlier,
   * but shrunk due to returns). */
  pushpath(info, ".callinfo");
  level = 0;

  // we expect that there's at least 1 CallInfo node, even for finished threads.
  // I'd like to use end_ci here but it looks like it's actually the end of the vector,
  // and not the ci just past the end of the last CI.
  int num_cis = (int)((thread->ci + 1) - thread->base_ci);
  WRITE_VALUE(num_cis, int);
  for (ci = thread->base_ci; ci <= thread->ci; ++ci) {
    pushpath(info, "[%d]", level++);
    WRITE_VALUE(eris_savestackidx(thread, ci->func), size_t);
    WRITE_VALUE(eris_savestackidx(thread, ci->top), size_t);
    WRITE_VALUE(eris_savestackidx(thread, ci->base), size_t);
    /* CallInfo.nresults is only set for actual functions */
    WRITE_VALUE(ttisfunction(ci->func) ? ci->nresults : 0, int);
    WRITE_VALUE(ci->flags, uint8_t);

    if (eris_isLua(ci)) {
      WRITE_VALUE(ERIS_CI_KIND_LUA, uint8_t);
      const Closure *lcl = eris_ci_func(ci);
      WRITE_VALUE(ci->savedpc - lcl->l.p->code, size_t);
    }
    else if (ttisfunction(ci->func)) {
      WRITE_VALUE(ERIS_CI_KIND_C, uint8_t);
      // Unlike eris, we don't write a status here. I'm assuming that
      // only _threads_ have statuses now, which I guess makes sense.
      // When would you ever expect them to differ anyway?
      const Closure *lcl = eris_ci_func(ci);
      const char *debugname = lcl->c.debugname;
      lua_pushcclosurek(info->L, lcl->c.f, debugname, lcl->nupvalues, lcl->c.cont);
                                                         /* ... thread func */
      persist(info);                                 /* ... thread func/nil */
      lua_pop(info->L, 1);                                    /* ... thread */
    } else {
      WRITE_VALUE(ERIS_CI_KIND_NONE, uint8_t);
      eris_assert(ttisnil(ci->func));
    }
    poppath(info);
  }

  poppath(info);

  pushpath(info, ".openupval");
  lua_pushnil(info->L);                                     /* ... thread nil */
  level = 0;
  for (uv = thread->openupval;
       uv != nullptr;
       uv = uv->u.open.threadnext)
  {
    pushpath(info, "[%d]", level++);
    WRITE_VALUE(eris_savestackidx(thread, uv->v) + 1, size_t);
    eris_setobj(info->L, info->L->top - 1, uv->v);          /* ... thread obj */
    lua_pushlightuserdata(info->L, uv);                  /* ... thread obj id */
    persist_keyed(info, LUA_TUPVAL);                        /* ... thread obj */
    poppath(info);
  }
  // terminate the openupval list
  WRITE_VALUE(0, size_t);
  lua_pop(info->L, 1);                                          /* ... thread */
  poppath(info);
  eris_assert(lua_type(info->L, -1) == LUA_TTHREAD);
}

/* Used in u_thread to validate read stack positions. */
#define validate(stackpos, inclmax) \
  if ((stackpos) < thread->stack || stackpos > (inclmax)) { \
    (stackpos) = thread->stack; \
    eris_error(info, ERIS_ERR_STACKBOUNDS); }

/* I had so hoped to get by without any 'hacks', but I surrender. We mark the
 * thread as incomplete to avoid the GC messing with it while we're building
 * it. Otherwise it may try to shrink its stack. We do this by setting its
 * stack field to null for every call that may trigger a GC run, since that
 * field is what's used to determine whether threads should be shrunk. See
 * lgc.c:699. Some of the locks could probably be joined (since nothing
 * inbetween requires the stack field to be valid), but I prefer to keep the
 * "invalid" blocks as small as possible to make it clearer. Also, locking and
 * unlocking are really just variable assignments, so they're really cheap. */
#define LOCK(L) (L->stack = nullptr)
#define UNLOCK(L) (L->stack = stack)

static void
u_thread(Info *info) {                                                 /* ... */
  lua_State* thread;
  size_t level;
  StkId stack, o;

  eris_checkstack(info->L, 3);
  thread = lua_newthread(info->L);                              /* ... thread */
  registerobject(info);

  // The created thread's globals table is currently shared with info->L, which
  // isn't what we want. Rehydrate the thread's global environment (including
  // its sandboxed-ness if applicable) and shove it onto `thread->gt`
  pushpath(info, ".gt");
  unpersist(info);                                           /* ... thread gt */
  lua_setfenv(info->L, -2);                                     /* ... thread */
  poppath(info);

  /* Unpersist the stack. Read size first and adjust accordingly. */
  eris_reallocstack(thread, READ_VALUE(int));
  stack = thread->stack; /* After the realloc in case the address changes. */
  thread->top = thread->stack + READ_VALUE(size_t);
  validate(thread->top, thread->stack_last);

  /* Read the elements one by one. */
  LOCK(thread);
  pushpath(info, ".stack");
  UNLOCK(thread);
  level = 0;
  for (o = stack; o < thread->top; ++o) {
    LOCK(thread);
    pushpath(info, "[%d]", level++);
    unpersist(info);                                        /* ... thread obj */
    UNLOCK(thread);
    eris_setobj(thread, o, info->L->top - 1);
    lua_pop(info->L, 1);                                        /* ... thread */
    LOCK(thread);
    poppath(info);
    UNLOCK(thread);
  }
  LOCK(thread);
  poppath(info);
  UNLOCK(thread);

  /* Read general information. */
  thread->status = READ_VALUE(uint8_t);
  size_t _errfunc = READ_VALUE(size_t);
  /* These are only used while a thread is being executed or can be deduced:
  thread->nCcalls = READ_VALUE(uint16_t);
  thread->allowhook = READ_VALUE(uint8_t); */

  /* Not supported.
  thread->hookmask = READ_VALUE(uint8_t);
  thread->basehookcount = READ_VALUE(int);
  thread->hookcount = READ_VALUE(int); */

  /* Read call information (stack frames). */
  LOCK(thread);
  pushpath(info, ".callinfo");
  UNLOCK(thread);

  int num_cis = READ_VALUE(int);
  luaD_reallocCI(thread, num_cis);
  thread->ci = thread->base_ci;
  level = 0;
  for (int ci_idx=0; ci_idx<num_cis; ++ci_idx) {
    // Need to add a callinfo if this isn't the first one
    if (ci_idx)
        incr_ci(thread);

    LOCK(thread);
    pushpath(info, "[%d]", level++);
    UNLOCK(thread);
    thread->ci->func = eris_restorestackidx(thread, READ_VALUE(size_t));
    validate(thread->ci->func, thread->top - 1);
    thread->ci->top = eris_restorestackidx(thread, READ_VALUE(size_t));
    validate(thread->ci->top, thread->stack_last);
    thread->ci->base = eris_restorestackidx(thread, READ_VALUE(size_t));
    validate(thread->ci->base, thread->top);
    thread->ci->nresults = READ_VALUE(int32_t);
    thread->ci->flags = READ_VALUE(uint8_t);

    auto ci_kind = (eris_CIKind)READ_VALUE(uint8_t);
    if (ci_kind == ERIS_CI_KIND_LUA) {
      Closure *lcl = eris_ci_func(thread->ci);
      thread->ci->savedpc = lcl->l.p->code + READ_VALUE(size_t);
      if (thread->ci->savedpc < lcl->l.p->code ||
          thread->ci->savedpc > lcl->l.p->code + lcl->l.p->sizecode)
      {
        thread->ci->savedpc = lcl->l.p->code; /* Just to be safe. */
        eris_error(info, ERIS_ERR_THREADPC);
      }
    } else if (ci_kind == ERIS_CI_KIND_C) {
      // ci->func is a StkIdx, so loading the stack should have loaded this.
      eris_assert(ttisfunction(thread->ci->func));

      // This function _should_ already be on the stack, let's make sure.
      LOCK(thread);
      unpersist(info);                                  /* ... thread func? */
      UNLOCK(thread);
      eris_assert(lua_type(info->L, -1) == LUA_TFUNCTION);

      Closure *func_cl = clvalue(thread->ci->func);
      eris_assert(clvalue(luaA_toobject(info->L, -1))->c.f == func_cl->c.f);
      // We don't actually use the function for anything, just checking!
      lua_pop(info->L, 1);                                    /* ... thread */
    } else {
      eris_assert(ci_kind == ERIS_CI_KIND_NONE);
    }
    LOCK(thread);
    poppath(info);
    UNLOCK(thread);
  }
  if (thread->status == LUA_YIELD) {
//    thread->ci->extra = eris_savestack(thread,
//      eris_restorestackidx(thread, READ_VALUE(size_t)));
//    o = eris_restorestack(thread, thread->ci->extra);
//    validate(o, thread->top);
//    if (eris_ttypenv(o) != LUA_TFUNCTION) {
//      eris_error(info, ERIS_ERR_THREADCI);
//    }
  }
  LOCK(thread);
  poppath(info);
  UNLOCK(thread);

  /* Get from context: only zero for dead threads, otherwise one. */
  thread->nCcalls = thread->status != LUA_OK || lua_gettop(thread) != 0;

  /* Proceed to open upvalues. These upvalues will already exist due to the
   * functions using them having been unpersisted (they'll usually be in the
   * stack of the thread). For this reason we store all previous references to
   * the upvalue in a table that is returned when we try to unpersist an
   * upvalue, so that we can adjust these references in here. */
  LOCK(thread);
  pushpath(info, ".openupval");
  UNLOCK(thread);
  level = 0;
  for (;;) {
    UpVal *nuv;
    StkId stk;
    /* Get the position of the upvalue on the stack. As a special value we pass
     * zero to indicate there are no more upvalues. */
    const size_t offset = READ_VALUE(size_t);
    if (offset == 0) {
      break;
    }
    LOCK(thread);
    pushpath(info, "[%d]", level);
    UNLOCK(thread);
    stk = eris_restorestackidx(thread, offset - 1);
    validate(stk, thread->top - 1);
    LOCK(thread);
    unpersist(info);                                        /* ... thread tbl */
    UNLOCK(thread);
    eris_assert(lua_type(info->L, -1) == LUA_TTABLE);

    /* Create the open upvalue either way. */
    LOCK(thread);
    nuv = eris_findupval(thread, stk);
    UNLOCK(thread);

    /* Then check if we need to patch some references. */
    lua_rawgeti(info->L, -1, UVTREF);               /* ... thread tbl lcl/nil */
    if (!lua_isnil(info->L, -1)) {                      /* ... thread tbl lcl */
      int i, n;
      eris_assert(lua_type(info->L, -1) == LUA_TFUNCTION);
      /* Already exists, replace it. To do this we have to patch all the
       * references to the already existing one, which we added to the table in
       * u_closure. */
      lua_pop(info->L, 1);                                  /* ... thread tbl */
      for (i = UVTREF, n = lua_objlen(info->L, -1); i <= n; i += 2) {
        Closure *cl;
        int nup;
        lua_rawgeti(info->L, -1, i);                    /* ... thread tbl lcl */
        cl = clvalue(info->L->top - 1);
        lua_pop(info->L, 1);                                /* ... thread tbl */
        lua_rawgeti(info->L, -1, i + 1);                /* ... thread tbl nup */
        nup = lua_tointeger(info->L, -1);
        lua_pop(info->L, 1);                                /* ... thread tbl */
        /* Open the upvalue by pointing to the stack and register in GC. */
        setupvalue(info->L, &cl->l.uprefs[nup - 1], nuv);
        luaC_objbarrier(info->L, cl, nuv);
      }
    }
    else {                                              /* ... thread tbl nil */
      eris_assert(lua_isnil(info->L, -1));
      lua_pop(info->L, 1);                                  /* ... thread tbl */
    }

    /* Store open upvalue in table for future references. */
    LOCK(thread);
    lua_pop(info->L, 1);                                        /* ... thread */
    poppath(info);
    UNLOCK(thread);
  }
  poppath(info);

  luaC_threadbarrier(thread);

  eris_assert(lua_type(info->L, -1) == LUA_TTHREAD);
}

#undef UNLOCK
#undef LOCK

#undef validate

/*
** ============================================================================
** Top-level delegator.
** ============================================================================
*/

static void
persist_typed(Info *info, int type) {                 /* perms reftbl ... obj */
  eris_ifassert(const int top = lua_gettop(info->L));
  if (info->level >= info->maxComplexity) {
    eris_error(info, ERIS_ERR_COMPLEXITY);
  }
  ++info->level;

  WRITE_VALUE(type, uint8_t);
  switch(type) {
    case LUA_TBOOLEAN:
      p_boolean(info);
      break;
    case LUA_TLIGHTUSERDATA:
      p_pointer(info);
      break;
    case LUA_TNUMBER:
      p_number(info);
      break;
    case LUA_TVECTOR:
      p_vector(info);
      break;
    case LUA_TSTRING:
      p_string(info);
      break;
    case LUA_TTABLE:
      p_table(info);
      break;
    case LUA_TFUNCTION:
      p_closure(info);
      break;
    case LUA_TUSERDATA:
      p_userdata(info);
      break;
    case LUA_TTHREAD:
      p_thread(info);
      break;
    case LUA_TPROTO:
      p_proto(info);
      break;
    case LUA_TUPVAL:
      p_upval(info);
      break;
    default:
      eris_error(info, ERIS_ERR_TYPEP, type);
  }                                                   /* perms reftbl ... obj */

  --info->level;
  eris_assert(top == lua_gettop(info->L));
}

/* Second-level delegating persist function, used for cases when persisting
 * data that's stored in the reftable with a key that is not the data itself,
 * namely upvalues and protos. */
static void
persist_keyed(Info *info, int type) {          /* perms reftbl ... obj refkey */
  eris_checkstack(info->L, 2);

  /* Keep a copy of the key for pushing it to the reftable, if necessary. */
  lua_pushvalue(info->L, -1);           /* perms reftbl ... obj refkey refkey */

  /* If the object has already been written, write a reference to it. */
  lua_rawget(info->L, REFTIDX);           /* perms reftbl ... obj refkey ref? */
  if (!lua_isnil(info->L, -1)) {           /* perms reftbl ... obj refkey ref */
    const int reference = lua_tointeger(info->L, -1);
    WRITE_VALUE(ERIS_REFERENCE, uint8_t);
    WRITE_VALUE(reference, int);
    lua_pop(info->L, 2);                              /* perms reftbl ... obj */
    return;
  }                                        /* perms reftbl ... obj refkey nil */
  lua_pop(info->L, 1);                         /* perms reftbl ... obj refkey */

  /* Copy the refkey for the perms check below. */
  lua_pushvalue(info->L, -1);           /* perms reftbl ... obj refkey refkey */

  /* Put the value in the reference table. This creates an entry pointing from
   * the object (or its key) to the id the object is referenced by. */
  lua_pushinteger(info->L, ++(info->refcount));
                                    /* perms reftbl ... obj refkey refkey ref */
  lua_rawset(info->L, REFTIDX);                /* perms reftbl ... obj refkey */

  /* At this point, we'll give the permanents table a chance to play. */
  lua_gettable(info->L, PERMIDX);            /* perms reftbl ... obj permkey? */
  if (!lua_isnil(info->L, -1)) {              /* perms reftbl ... obj permkey */
    type = lua_type(info->L, -2);
    /* Prepend permanent "type" so that we know it's a permtable key. This will
     * trigger u_permanent when unpersisting. Also write the original type, so
     * that we can verify what we get in the permtable when unpersisting is of
     * the same kind we had when persisting. */
    WRITE_VALUE(ERIS_PERMANENT, uint8_t);
    WRITE_VALUE(type, uint8_t);
    persist(info);                            /* perms reftbl ... obj permkey */
    lua_pop(info->L, 1);                              /* perms reftbl ... obj */
  }
  else {                                          /* perms reftbl ... obj nil */
    /* No entry in the permtable for this object, persist it directly. */
    lua_pop(info->L, 1);                              /* perms reftbl ... obj */
    persist_typed(info, type);                        /* perms reftbl ... obj */
  }                                                   /* perms reftbl ... obj */
}

/* Top-level delegating persist function. */
static void
persist(Info *info) {                                 /* perms reftbl ... obj */
  /* Grab the object's type. */
  const int type = lua_type(info->L, -1);

  /* If the object is nil, only write its type. */
  if (type == LUA_TNIL) {
    WRITE_VALUE(type, uint8_t);
  }
  /* Write simple values directly, because writing a "reference" would take up
   * just as much space and we can save ourselves work this way. */
  else if (type == LUA_TBOOLEAN ||
           type == LUA_TLIGHTUSERDATA ||
           type == LUA_TNUMBER)
  {
    persist_typed(info, type);                        /* perms reftbl ... obj */
  }
  /* For all non-simple values we keep a record in the reftable, so that we
   * keep references alive across persisting and unpersisting an object. This
   * has the nice side-effect of saving some space. */
  else {
    eris_checkstack(info->L, 1);
    lua_pushvalue(info->L, -1);                   /* perms reftbl ... obj obj */
    persist_keyed(info, type);                        /* perms reftbl ... obj */
  }
}

/** ======================================================================== */

static void
u_permanent(Info *info) {                                 /* perms reftbl ... */
  const int type = READ_VALUE(uint8_t);
  /* Reserve reference to avoid the key going first. */
  const int reference = ++(info->refcount);
  eris_checkstack(info->L, 1);
  unpersist(info);                                /* perms reftbl ... permkey */
  eris_assert(lua_type(info->L, PERMIDX) == LUA_TTABLE);
  lua_gettable(info->L, PERMIDX);                    /* perms reftbl ... obj? */
  if (lua_isnil(info->L, -1)) {                       /* perms reftbl ... nil */
    /* Since we may need permanent values to rebuild other structures, namely
     * closures and threads, we cannot allow perms to fail unpersisting. */
    eris_error(info, ERIS_ERR_SPER_UPERMNIL);
  }
  else if (lua_type(info->L, -1) != type) {            /* perms reftbl ... :( */
    /* For the same reason that we cannot allow nil we must also require the
     * unpersisted value to be of the correct type. */
    const char *want = kTypenames[type];
    const char *have = kTypenames[lua_type(info->L, -1)];
    eris_error(info, ERIS_ERR_SPER_UPERM, want, have);
  }                                                   /* perms reftbl ... obj */
  /* Create the entry in the reftable. */
  lua_pushvalue(info->L, -1);                     /* perms reftbl ... obj obj */
  lua_rawseti(info->L, REFTIDX, reference);           /* perms reftbl ... obj */
}

static void
unpersist(Info *info) {                                   /* perms reftbl ... */
  eris_ifassert(const int top = lua_gettop(info->L));
  if (info->level >= info->maxComplexity) {
    eris_error(info, ERIS_ERR_COMPLEXITY);
  }
  ++info->level;

  eris_checkstack(info->L, 1);
  {
    const uint8_t type = READ_VALUE(uint8_t);
    switch (type) {
      case LUA_TNIL:
        lua_pushnil(info->L);
        break;
      case LUA_TBOOLEAN:
        u_boolean(info);
        break;
      case LUA_TLIGHTUSERDATA:
        u_pointer(info);
        break;
      case LUA_TNUMBER:
        u_number(info);
        break;
      case LUA_TVECTOR:
        u_vector(info);
        break;
      case LUA_TSTRING:
        u_string(info);
        break;
      case LUA_TTABLE:
        u_table(info);
        break;
      case LUA_TFUNCTION:
        u_closure(info);
        break;
      case LUA_TUSERDATA:
        u_userdata(info);
        break;
      case LUA_TTHREAD:
        u_thread(info);
        break;
      case LUA_TPROTO:
        u_proto(info);
        break;
      case LUA_TUPVAL:
        u_upval(info);
        break;
      case ERIS_PERMANENT:
        u_permanent(info);
        break;
      case ERIS_REFERENCE: {
        const int reference = READ_VALUE(int);
        lua_rawgeti(info->L, REFTIDX, reference);   /* perms reftbl ud ... obj? */
        if (lua_isnil(info->L, -1)) {                 /* perms reftbl ud ... :( */
          eris_error(info, ERIS_ERR_REF, reference);
        }                                            /* perms reftbl ud ... obj */
        break;
      }
      default:
        eris_error(info, ERIS_ERR_TYPEU, type);
    }                                              /* perms reftbl ... obj? */
  }

  --info->level;
  eris_assert(top + 1 == lua_gettop(info->L));
}

/*
** {===========================================================================
** Library functions.
** ============================================================================
*/

static void
p_header(Info *info) {
  WRITE_RAW(kHeader, HEADER_LENGTH);
  WRITE_VALUE(sizeof(lua_Number), uint8_t);
  WRITE_VALUE(kHeaderNumber, lua_Number);
  WRITE_VALUE(sizeof(int), uint8_t);
  WRITE_VALUE(sizeof(size_t), uint8_t);
  WRITE_VALUE(LUA_VECTOR_SIZE, uint8_t);
}

static void
u_header(Info *info) {
  char header[HEADER_LENGTH];
  uint8_t number_size;
  READ_RAW(header, HEADER_LENGTH);
  if (strncmp(kHeader, header, HEADER_LENGTH) != 0) {
    luaL_error(info->L, "invalid data");
  }
  number_size = READ_VALUE(uint8_t);
  if (number_size != sizeof(lua_Number)) {
    luaL_error(info->L, "incompatible floating point type");
  }
  /* In this case we really do want floating point equality. */
  if (READ_VALUE(lua_Number) != kHeaderNumber) {
    luaL_error(info->L, "incompatible floating point representation");
  }
  info->u.upi.sizeof_int = READ_VALUE(uint8_t);
  info->u.upi.sizeof_size_t = READ_VALUE(uint8_t);
  info->u.upi.vector_components = READ_VALUE(uint8_t);
}

static void store_cfunc_perms(lua_State *L) {
  eris_ifassert(const int top = lua_gettop(L));
  // We need to do special things to handle permanent indexes for C functions,
  // there can be multiple closures for the same C function and their closure
  // objects will be considered unequal.

  // scan for c closures and store their function pointers
  lua_newtable(L);                                   /* ... perms new_perms */
  lua_pushnil(L);                                  /* ... perms new_perms k */
  while (lua_next(L, PERMIDX)) {                 /* ... perms new_perms k v */
    if (lua_type(L, -2) == LUA_TFUNCTION) {
      Closure *cl = clvalue(luaA_toobject(L, -2));
                                              /* ... perms new_perms k_cl v */
      if (cl->isC) {
        lua_pushvalue(L, -3);          /* ... perms new_perms k v new_perms */
        // TODO: technically this should be keyed on the
        //  function + continuation function pair. I suppose it's technically
        //  possible for multiple closures to be defined with the same
        //  function pointer, but distinct continuation pointers, although I
        //  don't know if that ever actually happens!
        lua_pushlightuserdata(L, (void*)cl->c.f);
                               /* ... perms new_perms k_cl v new_perms k_id */
        lua_pushvalue(L, -3);
                             /* ... perms new_perms k_cl v new_perms k_id v */
        lua_rawset(L, -3);          /* ... perms new_perms k_cl v new_perms */
        lua_pop(L, 1);                        /* ... perms new_perms k_cl v */
      }
    }
    lua_pop(L, 1);                                 /* ... perms new_perms k */
    eris_assert(lua_type(L, -2) == LUA_TTABLE);
  }

  // update the perms table with the function pointer -> id mappings
  lua_pushnil(L);                                  /* ... perms new_perms k */
  while (lua_next(L, -2)) {                      /* ... perms new_perms k v */
    lua_pushvalue(L, -2);                      /* ... perms new_perms k v k */
    lua_pushvalue(L, -2);                    /* ... perms new_perms k v k v */
    // assign to the old perms table
    lua_settable(L, PERMIDX);                    /* ... perms new_perms k v */
    lua_pop(L, 1);                                 /* ... perms new_perms k */
    eris_assert(lua_type(L, PERMIDX) == LUA_TTABLE);
  }
  lua_pop(L, 1);                                               /* ... perms */
  eris_assert(top == lua_gettop(L));
}

static void
unchecked_persist(lua_State *L, std::ostream *writer) {
  Info info;                                            /* perms buff rootobj */
  info.L = L;
  info.level = 0;
  info.refcount = 0;
  info.maxComplexity = kMaxComplexity;
  info.generatePath = kGeneratePath;
  info.persisting = true;
  info.u.pi.writer = writer;
  info.u.pi.metafield = kPersistKey;
  info.u.pi.writeDebugInfo = kWriteDebugInformation;
  info.u.pi.persistingCFunc = false;

  eris_checkstack(L, 6);

  if (get_setting(L, (void*)&kSettingMaxComplexity)) {
                                                  /* perms buff rootobj value */
    info.maxComplexity = lua_tounsigned(L, -1);
    lua_pop(L, 1);                                      /* perms buff rootobj */
  }
  if (get_setting(L, (void*)&kSettingGeneratePath)) {
                                                  /* perms buff rootobj value */
    info.generatePath = lua_toboolean(L, -1);
    lua_pop(L, 1);                                      /* perms buff rootobj */
  }
  if (get_setting(L, (void*)&kSettingMetafield)) {/* perms buff rootobj value */
    info.u.pi.metafield = lua_tostring(L, -1);
    lua_pop(L, 1);                                      /* perms buff rootobj */
  }
  if (get_setting(L, (void*)&kSettingWriteDebugInfo)) {
                                                  /* perms buff rootobj value */
    info.u.pi.writeDebugInfo = lua_toboolean(L, -1);
    lua_pop(L, 1);                                      /* perms buff rootobj */
  }

  lua_newtable(L);                               /* perms buff rootobj reftbl */
  lua_insert(L, REFTIDX);                        /* perms reftbl buff rootobj */
  if (info.generatePath) {
    lua_newtable(L);                        /* perms reftbl buff rootobj path */
    lua_insert(L, PATHIDX);                 /* perms reftbl buff path rootobj */
    pushpath(&info, "root");
  }

  /* Populate perms table with Lua internals. */
  lua_pushvalue(L, PERMIDX);         /* perms reftbl buff path? rootobj perms */
  populateperms(L, false);
  store_cfunc_perms(L);

  // Store a perm for the underlying globals object
  sethvalue(info.L, info.L->top, eris_getglobalsbase(&info));
  eris_incr_top(info.L);
  lua_pushstring(info.L, "eris__globals_base");
  lua_rawset(info.L, -3);

  lua_pop(L, 1);                           /* perms reftbl buff path? rootobj */

  p_header(&info);
  persist(&info);                          /* perms reftbl buff path? rootobj */

  if (info.generatePath) {                  /* perms reftbl buff path rootobj */
    lua_remove(L, PATHIDX);                      /* perms reftbl buff rootobj */
  }                                              /* perms reftbl buff rootobj */
  lua_remove(L, REFTIDX);                               /* perms buff rootobj */
}

static void
unchecked_unpersist(lua_State *L, std::istream *reader) {/* perms str? */
  Info info;
  info.L = L;
  info.level = 0;
  info.refcount = 0;
  info.maxComplexity = kMaxComplexity;
  info.generatePath = kGeneratePath;
  info.persisting = false;
  info.u.upi.reader = reader;

  eris_checkstack(L, 6);

  if (get_setting(L, (void*)&kSettingMaxComplexity)) {
                                                  /* perms buff rootobj value */
    info.maxComplexity = lua_tounsigned(L, -1);
    lua_pop(L, 1);                                      /* perms buff rootobj */
  }
  if (get_setting(L, (void*)&kSettingGeneratePath)) {
                                                 /* perms buff? rootobj value */
    info.generatePath = lua_toboolean(L, -1);
    lua_pop(L, 1);                                     /* perms buff? rootobj */
  }

  lua_newtable(L);                                       /* perms str? reftbl */
  lua_insert(L, REFTIDX);                                /* perms reftbl str? */
  if (info.generatePath) {
    /* Make sure the path is always at index 4, so that it's the same for
     * persist and unpersist. */
    lua_pushnil(L);                                  /* perms reftbl str? nil */
    lua_insert(L, BUFFIDX);                          /* perms reftbl nil str? */
    lua_newtable(L);                            /* perms reftbl nil str? path */
    lua_insert(L, PATHIDX);                     /* perms reftbl nil path str? */
    pushpath(&info, "root");
  }

  /* Populate perms table with Lua internals. */
  lua_pushvalue(L, PERMIDX);            /* perms reftbl nil? path? str? perms */
  populateperms(L, true);

  // Make sure the underlying globals object is correct
  lua_pushstring(info.L, "eris__globals_base");
  sethvalue(info.L, info.L->top, eris_getglobalsbase(&info));
  eris_incr_top(info.L);
  lua_rawset(info.L, -3);

  lua_pop(L, 1);                              /* perms reftbl nil? path? str? */

  // pause GC for the duration of deserialization - some objects we're creating aren't rooted
  {
    ScopedDisableGC _disable_gc(L);

    u_header(&info);
    unpersist(&info);                 /* perms reftbl nil? path? str? rootobj */
  }

  if (info.generatePath) {              /* perms reftbl nil path str? rootobj */
    lua_remove(L, PATHIDX);                  /* perms reftbl nil str? rootobj */
    lua_remove(L, BUFFIDX);                      /* perms reftbl str? rootobj */
  }                                              /* perms reftbl str? rootobj */
  lua_remove(L, REFTIDX);                               /* perms str? rootobj */
}

/** ======================================================================== */

static int
l_persist(lua_State *L) {                             /* perms? rootobj? ...? */

  /* See if we have anything at all. */
  luaL_checkany(L, 1);

  /* If we only have one object we assume it is the root object and that there
   * is no perms table, so we create an empty one for internal use. */
  if (lua_gettop(L) == 1) {                                        /* rootobj */
    eris_checkstack(L, 1);
    lua_newtable(L);                                         /* rootobj perms */
    lua_insert(L, PERMIDX);                                  /* perms rootobj */
  }
  else {
    luaL_checktype(L, 1, LUA_TTABLE);                  /* perms rootobj? ...? */
    luaL_checkany(L, 2);                                /* perms rootobj ...? */
    lua_settop(L, 2);                                        /* perms rootobj */
  }
  eris_checkstack(L, 1);
  lua_pushnil(L);                                       /* perms rootobj buff */
  lua_insert(L, 2);                                     /* perms buff rootobj */


  std::ostringstream writer;
  unchecked_persist(L, &writer);                        /* perms buff rootobj */

  /* Copy the buffer as the result string before removing it, to avoid the data
   * being garbage collected. */
  lua_pushlstring(L, writer.str().c_str(), writer.str().size());
                                                    /* perms buff rootobj str */

  return 1;
}

static int
l_unpersist(lua_State *L) {                               /* perms? str? ...? */

  /* See if we have anything at all. */
  luaL_checkany(L, 1);

  /* If we only have one object we assume it is the root object and that there
   * is no perms table, so we create an empty one for internal use. */
  if (lua_gettop(L) == 1) {                                           /* str? */
    eris_checkstack(L, 1);
    lua_newtable(L);                                            /* str? perms */
    lua_insert(L, PERMIDX);                                     /* perms str? */
  }
  else {
    luaL_checktype(L, 1, LUA_TTABLE);                      /* perms str? ...? */
  }

  size_t buff_len;
  const char *buff = luaL_checklstring(L, 2, &buff_len);
  std::istringstream reader(std::string(buff, buff_len));
  reader.seekg(0);
  lua_settop(L, 2);                                              /* perms str */

  unchecked_unpersist(L, &reader);                       /* perms str rootobj */

  return 1;
}

#define IS(s) strncmp(s, name, length < sizeof(s) ? length : sizeof(s)) == 0

static int
l_settings(lua_State *L) {                                /* name value? ...? */
  size_t length;
  const char *name = luaL_checklstring(L, 1, &length);
  if (lua_isnone(L, 2)) {                                        /* name ...? */
    lua_settop(L, 1);                                                 /* name */
    /* Get the current setting value and return it. */
    if (IS(kSettingMetafield)) {
      if (!get_setting(L, (void*)&kSettingMetafield)) {
        lua_pushstring(L, kPersistKey);
      }
    }
    else if (IS(kSettingWriteDebugInfo)) {
      if (!get_setting(L, (void*)&kSettingWriteDebugInfo)) {
        lua_pushboolean(L, kWriteDebugInformation);
      }
    }
    else if (IS(kSettingGeneratePath)) {
      if (!get_setting(L, (void*)&kSettingGeneratePath)) {
        lua_pushboolean(L, kGeneratePath);
      }
    }
    else if (IS(kSettingMaxComplexity)) {
      if (!get_setting(L, (void*)&kSettingMaxComplexity)) {
        lua_pushunsigned(L, kMaxComplexity);
      }
    }
    else {
      luaL_argerror(L, 1, "no such setting");
      return 0;
    }                                                           /* name value */
    return 1;
  }
  else {                                                   /* name value ...? */
    lua_settop(L, 2);                                           /* name value */
    /* Set a new value for the setting. */
    if (IS(kSettingMetafield)) {
      luaL_optstring(L, 2, nullptr);
      set_setting(L, (void*)&kSettingMetafield);
    }
    else if (IS(kSettingWriteDebugInfo)) {
      luaL_opt(L, checkboolean, 2, false);
      set_setting(L, (void*)&kSettingWriteDebugInfo);
    }
    else if (IS(kSettingGeneratePath)) {
      luaL_opt(L, checkboolean, 2, false);
      set_setting(L, (void*)&kSettingGeneratePath);
    }
    else if (IS(kSettingMaxComplexity)) {
      luaL_optunsigned(L, 2, 0);
      set_setting(L, (void*)&kSettingMaxComplexity);
    }
    else {
      luaL_argerror(L, 1, "no such setting");
      return 0;
    }                                                                 /* name */
    return 0;
  }
}

#undef IS

/** ======================================================================== */

static luaL_Reg erislib[] = {
  { "persist", l_persist },
  { "unpersist", l_unpersist },
  { "settings", l_settings },
  { nullptr, nullptr }
};

LUA_API int luaopen_eris(lua_State *L) {
  luaL_register(L, LUA_ERISLIBNAME, erislib);
  return 1;
}

/* }======================================================================== */

/*
** {===========================================================================
** Public API functions.
** ============================================================================
*/

LUA_API void
eris_dump(lua_State *L, std::ostream *writer) {     /* perms? rootobj? */
  if (lua_gettop(L) > 2) {
    luaL_error(L, "too many arguments");
  }
  luaL_checktype(L, 1, LUA_TTABLE);                         /* perms rootobj? */
  luaL_checkany(L, 2);                                       /* perms rootobj */
  lua_pushnil(L);                                        /* perms rootobj nil */
  lua_insert(L, -2);                                     /* perms nil rootobj */
  unchecked_persist(L, writer);                          /* perms nil rootobj */
  lua_remove(L, -2);                                         /* perms rootobj */
}

LUA_API void
eris_undump(lua_State *L, std::istream *reader) {                   /* perms? */
  if (lua_gettop(L) > 1) {
    luaL_error(L, "too many arguments");
  }
  luaL_checktype(L, 1, LUA_TTABLE);                                  /* perms */
  unchecked_unpersist(L, reader);                            /* perms rootobj */
}

/** ======================================================================== */

LUA_API void
eris_persist(lua_State *L, int perms, int value) {                    /* ...? */
  perms = lua_absindex(L, perms);
  value = lua_absindex(L, value);
  eris_checkstack(L, 3);
  lua_pushcfunction(L, l_persist, "l_persist");              /* ... l_persist */
  lua_pushvalue(L, perms);                             /* ... l_persist perms */
  lua_pushvalue(L, value);                     /* ... l_persist perms rootobj */
  lua_call(L, 2, 1);                                               /* ... str */
}

LUA_API void
eris_unpersist(lua_State *L, int perms, int value) {                   /* ... */
  perms = lua_absindex(L, perms);
  value = lua_absindex(L, value);
  eris_checkstack(L, 3);
  lua_pushcfunction(L, l_unpersist, "l_unpersist");        /* ... l_unpersist */
  lua_pushvalue(L, perms);                           /* ... l_unpersist perms */
  lua_pushvalue(L, value);                       /* ... l_unpersist perms str */
  lua_call(L, 2, 1);                                           /* ... rootobj */
}

LUA_API void
eris_get_setting(lua_State *L, const char *name) {                     /* ... */
  eris_checkstack(L, 2);
  lua_pushcfunction(L, l_settings, "l_settings");           /* ... l_settings */
  lua_pushstring(L, name);                             /* ... l_settings name */
  lua_call(L, 1, 1);                                             /* ... value */
}

LUA_API void
eris_set_setting(lua_State *L, const char *name, int value) {          /* ... */
  value = lua_absindex(L, value);
  eris_checkstack(L, 3);
  lua_pushcfunction(L, l_settings, "l_settings");           /* ... l_settings */
  lua_pushstring(L, name);                             /* ... l_settings name */
  lua_pushvalue(L, value);                       /* ... l_settings name value */
  lua_call(L, 2, 0);                                                   /* ... */
}

static void gatherfunctions(std::vector<Proto*>& results, Proto* proto) {
  if (results.size() <= size_t(proto->bytecodeid))
    results.resize(proto->bytecodeid + 1);

  // Skip protos that we've already compiled in this run: this happens because
  // at -O2, inlined functions get their protos reused
  if (results[proto->bytecodeid])
    return;

  results[proto->bytecodeid] = proto;

  for (int i = 0; i < proto->sizep; i++)
    gatherfunctions(results, proto->p[i]);
}

LUA_API lua_State *
eris_make_forkserver(lua_State *L) {
  lua_State *GL = lua_mainthread(L);
  // We can't clone the main thread!
  eris_assert(GL != L);
  // Using a running script as a forkserver gets nasty, so make sure this
  // isn't one.
  eris_assert(lua_gettop(L) == 1);
  eris_assert(lua_isLfunction(L, -1));
  // The thread we're serializing must be a sandboxed thread. We rely on
  // globals being provided through the environment proxy table to ensure
  // efficient serializability of state.
  eris_assert(L->gt->safeenv && GL->gt->readonly);

  // Make a new thread for the forkserver that will be used to serialize `L`
  // and spin up new instances of it on demand
  lua_State *Lforker = lua_newthread(GL);                  /* GL: ... Lforker */
  // Show paths on error
  lua_pushboolean(Lforker, 1);                            /* LForker: enabled */
  eris_set_setting(Lforker, kSettingGeneratePath, -1);
  lua_pop(Lforker, 1);                                            /* LForker: */
  luaL_sandboxthread(Lforker);

  // Add a table for state related to this forker and anchor it to the base of the
  // stack. This effectively works as a forker-local registry.
  lua_newtable(Lforker);                                    /* LForker: state */

  // Collect all protos reachable from the main func (should be all of them)
  // TODO: Verify that tables don't have untrusted __persist hooks on them!
  std::vector<Proto *> protos;
  gatherfunctions(protos, clvalue(luaA_toobject(L, -1))->l.p);

  // build the perm val -> id table for persisting
  lua_newtable(Lforker);                              /* Lforker: state perms */
  for (auto *proto : protos) {
    lua_pushlightuserdata(Lforker, proto);      /* Lforker: state perms proto */
    lua_pushfstring(Lforker, "proto_%d", proto->bytecodeid);
                                       /* Lforker: state perms proto proto_id */
    lua_rawset(Lforker, -3);                          /* Lforker: state perms */
  }

  lua_pushlightuserdata(Lforker, (void *)&kForkerPermsTable);
                                                  /* LForker: state perms key */
  lua_pushvalue(Lforker, -2);               /* LForker: state perms key perms */
  lua_rawset(Lforker, FS_STATE_IDX);                  /* LForker: state perms */

  // push the thread to fork
  setthvalue(Lforker, Lforker->top, L);            /* Lforker: state perms th */
  eris_incr_top(Lforker);
  // args stay on stack, but serialized form added
  eris_persist(Lforker, -2, -1);               /* Lforker: state perms th ser */

  // Anchor the base thread we're forking in the registry so it stays alive as
  // long as we do.
  lua_pushlightuserdata(Lforker, (void *)&kForkerBaseThread);
                                           /* LForker: state perms th ser key */
  lua_pushvalue(Lforker, -3);           /* LForker: state perms th ser key th */
  lua_rawset(Lforker, FS_STATE_IDX);           /* LForker: state perms th ser */

  // Store the "base" state for new scripts of this type
  lua_pushlightuserdata(Lforker, (void *)&kForkerBaseState);
                                           /* LForker: state perms th ser key */
  lua_pushvalue(Lforker, -2);          /* LForker: state perms th ser key ser */
  lua_rawset(Lforker, FS_STATE_IDX);           /* LForker: state perms th ser */

  lua_pop(Lforker, 3);                                      /* LForker: state */

  // Make a new table for keys to use when deserializing script states
  lua_newtable(Lforker);                             /* Lforker: state uperms */

  // rebuild the perms table to work for deserialization
  for (auto *proto : protos) {
    lua_pushfstring(Lforker, "proto_%d", proto->bytecodeid);
                                            /* Lforker: state uperms proto_id */
    lua_pushlightuserdata(Lforker, proto);
                                      /* Lforker: state uperms proto_id proto */
    lua_rawset(Lforker, -3);                         /* Lforker: state uperms */
  }

  // store the uperms table to the state table
  lua_pushlightuserdata(Lforker, (void *)&kForkerUPermsTable);
                                                 /* LForker: state uperms key */
  lua_pushvalue(Lforker, -2);             /* LForker: state uperms key uperms */
  lua_rawset(Lforker, FS_STATE_IDX);                 /* LForker: state uperms */

  lua_pop(Lforker, 1);                                      /* LForker: state */

  // Don't log paths when deserializing, it's expensive.
  lua_pushboolean(Lforker, 0);                      /* LForker: state enabled */
  eris_set_setting(Lforker, kSettingGeneratePath, -1);
  lua_pop(Lforker, 1);                                      /* LForker: state */

  // only the state table should be left
  LUAU_ASSERT(lua_gettop(Lforker) == 1);

  return Lforker;
}

LUA_API lua_State*
eris_fork_thread(lua_State *Lforker, uint8_t default_state) {
                                               /* Lforker: state default_ser? */
  lua_State *GL = lua_mainthread(Lforker);
  // We can't clone the main thread!
  LUAU_ASSERT(GL != Lforker);

  // serialized script state isn't already on the stack, push the default state
  if (default_state) {
    lua_pushlightuserdata(Lforker, (void *)&kForkerBaseState);
    lua_rawget(Lforker, FS_STATE_IDX);           /* Lforker: state serialized */
  }

  lua_pushlightuserdata(Lforker, (void*)&kForkerUPermsTable);
  lua_rawget(Lforker, FS_STATE_IDX);      /* Lforker: state serialized uperms */
  // eris_unpersist() will mutate the uperms table, so clone it first
  Table *cloned_uperms = luaH_clone(Lforker, hvalue(luaA_toobject(Lforker, -1)));
  // must be anchored on the stack before we pop in case of a realloc
  sethvalue(Lforker, Lforker->top, cloned_uperms);
                            /* Lforker: state serialized uperms uperms_cloned */
  eris_incr_top(Lforker);
  lua_remove(Lforker, -2);                /* Lforker: state serialized uperms */

  eris_unpersist(Lforker, -1, -2); /* Lforker: state serialized uperms new_th */
  eris_assert(lua_isthread(Lforker, -1));

  // Anchor the reference to the unpersisted thread to the main thread
  lua_xmove(Lforker, GL, 1);
         /* GL: ... Lforker ... new_th */ /* Lforker: state serialized uperms */

  lua_State *Lchild = thvalue(luaA_toobject(GL, -1));
  lua_pop(Lforker, 2);                                      /* Lforker: state */
  return Lchild;
}

LUA_API void
eris_serialize_thread(lua_State *Lforker, lua_State *L) {
  eris_assert(lua_mainthread(L) == lua_mainthread(Lforker));
  lua_pushlightuserdata(Lforker, (void *)&kForkerPermsTable);
  lua_rawget(Lforker, FS_STATE_IDX);                  /* LForker: state perms */

  // eris_persist() will mutate the perms table, so clone it first
  Table *cloned_perms = luaH_clone(Lforker, hvalue(luaA_toobject(Lforker, -1)));
  // must be anchored on the stack before we pop in case of a realloc
  sethvalue(Lforker, Lforker->top, cloned_perms);
                                         /* Lforker: state perms perms_cloned */
  eris_incr_top(Lforker);
  lua_remove(Lforker, -2);                 /* Lforker: state serialized perms */

  // push the thread to fork
  setthvalue(Lforker, Lforker->top, L);            /* Lforker: state perms th */
  eris_incr_top(Lforker);
  // args stay on stack, but serialized form added
  eris_persist(Lforker, -2, -1);               /* Lforker: state perms th ser */
  lua_remove(Lforker, -2);
  lua_remove(Lforker, -2);                              /* Lforker: state ser */
  // only serialized left on stack
}

/* }======================================================================== */

