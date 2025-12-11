#pragma once

// assume this also defines things like lua_cn_handle and creation/registration
// of them (example def of handle provided below)
#include <cn-executable/utils.h>

typedef struct { int lua_ref; } lua_cn_handle;

lua_cn_handle lua_append(lua_cn_handle xs, lua_cn_handle ys) {
    lua_getglobal(L, "append");

    lua_rawgeti(L, LUA_REGISTRYINDEX, xs.lua_ref);
    lua_rawgeti(L, LUA_REGISTRYINDEX, ys.lua_ref);

    lua_call(L, 2, 1);

    int ref = luaL_ref(L, LUA_REGISTRY_INDEX);
    return lua_create_lua_cn_handle(ref);
}

lua_cn_handle lua_IntList(lua_cn_handle handle, int mode) {
    lua_getglobal(L, "IntList");

    lua_rawgeti(L, LUA_REGISTRYINDEX, handle.lua_ref);
    lua_pushinteger(L, mode);

    lua_call(L, 2, 1);

    int ref = luaL_ref(L, LUA_REGISTRYINDEX);
    return lua_create_lua_cn_handle(ref);
}

bool lua_seq_equal(lua_cn_handle a, lua_cn_handle b) {
    lua_getglobal(L, "seq_equal");

    lua_rawgeti(L, LUA_REGISTRYINDEX, a.lua_ref);
    lua_rawgeti(L, LUA_REGISTRYINDEX, b.lua_ref);

    lua_call(L, 2, 1);

    bool r = lua_toboolean(L, -1);
    lua_pop(L, 1);

    return r;
}
