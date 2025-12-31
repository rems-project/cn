#include "lua_cn_runtime_core_wrappers.h"

#include <assert.h>

// Lua globals

lua_State *lua_state = NULL;
int lua_cn_runtime_ref = LUA_NOREF;

// Core Lua State

void lua_init() {
    lua_state = luaL_newstate();
    luaL_openlibs(lua_state);
}

void lua_deinit() {
    lua_close(lua_state);
    lua_state = NULL;
}

lua_State* lua_get_state() { return lua_state; }

// Lua CN Runtime State

void lua_cn_load_runtime(const char* filename) {
    assert(lua_state != NULL);

    lua_getglobal(lua_state, "package");
    lua_getfield(lua_state, -1, "path"); // get package.path
    const char *current_path = lua_tostring(lua_state, -1);
    lua_pop(lua_state, 1);
    char new_path[1024];
    snprintf(new_path, sizeof(new_path), "%s;%s/?.lua", current_path, "./lua_output");
    lua_pushstring(lua_state, new_path);
    lua_setfield(lua_state, -2, "path");

    if (luaL_dofile(lua_state, filename) != LUA_OK) {
        fprintf(stderr, "Unable to load lua cn runtime: %s\n", lua_tostring(lua_state, -1));
    }

    lua_cn_runtime_ref = luaL_ref(lua_state, LUA_REGISTRYINDEX);
}

void lua_cn_unload_runtime()
{
    assert(lua_cn_runtime_ref != LUA_NOREF);

    luaL_unref(lua_state, LUA_REGISTRYINDEX, lua_cn_runtime_ref);
    lua_cn_runtime_ref = LUA_NOREF;
}

int lua_cn_get_runtime_ref() { return lua_cn_runtime_ref; }

void lua_cn_register_c_func(const char* func_name, lua_CFunction func) {
    lua_rawgeti(lua_state, LUA_REGISTRYINDEX, lua_cn_runtime_ref);
    lua_getfield(lua_state, -1, "C");
    lua_pushcfunction(lua_state, func);
    lua_setfield(lua_state, -2, func_name);
    lua_pop(lua_state, 2);
}

// Lua CN Ghost State
void lua_cn_ghost_add(void* ptr, size_t size) {
    int64_t addr = lua_convert_ptr_to_int(ptr);

    lua_rawgeti(lua_state, LUA_REGISTRYINDEX, lua_cn_runtime_ref);
    lua_getfield(lua_state, -1, "ghost_state");
    lua_getfield(lua_state, -1, "Add");

    lua_pushinteger(lua_state, addr);
    lua_pushinteger(lua_state, size);

    if (lua_pcall(lua_state, 2, 0, 0) != LUA_OK) {
        fprintf(stderr, "Error calling CN.ghost_state.Add: %s\n", lua_tostring(lua_state, -1));
        lua_pop(lua_state, 1);
    }

    lua_pop(lua_state, 2);
}

void lua_cn_ghost_remove(void* ptr, size_t size) {
    int64_t addr = lua_convert_ptr_to_int(ptr);

    lua_rawgeti(lua_state, LUA_REGISTRYINDEX, lua_cn_runtime_ref);
    lua_getfield(lua_state, -1, "ghost_state");
    lua_getfield(lua_state, -1, "Remove");

    lua_pushinteger(lua_state, addr);
    lua_pushinteger(lua_state, size);

    if (lua_pcall(lua_state, 2, 0, 0) != LUA_OK) {
        fprintf(stderr, "Error calling CN.ghost_state.Remove: %s\n", lua_tostring(lua_state, -1));
        lua_pop(lua_state, 1);
    }

    lua_pop(lua_state, 2);
}

// Lua CN Error Handling
void lua_cn_error_push(const char* msg) {
    lua_rawgeti(lua_state, LUA_REGISTRYINDEX, lua_cn_runtime_ref);
    lua_getfield(lua_state, -1, "error_stack");
    lua_getfield(lua_state, -1, "Push");

    lua_pushstring(lua_state, msg);

    if (lua_pcall(lua_state, 1, 0, 0) != LUA_OK) {
        fprintf(stderr, "Error calling CN.error_stack.Push: %s\n", lua_tostring(lua_state, -1));
        lua_pop(lua_state, 1);
    }

    lua_pop(lua_state, 2);
}

void lua_cn_error_pop() {
    lua_rawgeti(lua_state, LUA_REGISTRYINDEX, lua_cn_runtime_ref);
    lua_getfield(lua_state, -1, "error_stack");
    lua_getfield(lua_state, -1, "Pop");

    if (lua_pcall(lua_state, 0, 0, 0) != LUA_OK) {
        fprintf(stderr, "Error calling CN.error_stack.Pop: %s\n", lua_tostring(lua_state, -1));
        lua_pop(lua_state, 1);
    }

    lua_pop(lua_state, 2);
}

// Types Utils
int64_t lua_convert_ptr_to_int(void* ptr) {
    return (int64_t)(uintptr_t)ptr;
}