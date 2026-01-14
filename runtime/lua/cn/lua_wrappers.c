#include "lua_wrappers.h"

#include <cn-executable/utils.h>

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

// C wrappers

static int c_assert_wrapper() {
    bool cond  = lua_toboolean(lua_state, 1);
    int64_t spec_mode = (int64_t)luaL_checkinteger(lua_state, 2);

    cn_assert(convert_to_cn_bool(cond), (enum spec_mode)spec_mode);

    return 0;
}

static int c_add_to_ghost_state_wrapper() {
    int64_t addr  = (int64_t)luaL_checkinteger(lua_state, 1);
    int64_t size = (int64_t)luaL_checkinteger(lua_state, 2);

    c_add_to_ghost_state((void*)addr, (size_t)size, get_cn_stack_depth());

    return 0;
}

static int c_remove_from_ghost_state_wrapper() {
    int64_t addr  = (int64_t)luaL_checkinteger(lua_state, 1);
    int64_t size = (int64_t)luaL_checkinteger(lua_state, 2);

    c_remove_from_ghost_state((void*)addr, (size_t)size);

    return 0;
}

static int c_get_or_put_ownership_wrapper() {
    int64_t spec_mode = (int64_t)luaL_checkinteger(lua_state, 1);
    int64_t addr  = (int64_t)luaL_checkinteger(lua_state, 2);
    int64_t size = (int64_t)luaL_checkinteger(lua_state, 3);

    //@note for now, we're not using loop ownershio. 
    // But we will as soon as I start looking at other
    // testcases.
    cn_get_or_put_ownership(
        (enum spec_mode)spec_mode, 
        (void*)addr, 
        (size_t)size,
        0
    );

    return 0;
}

static int c_ghost_state_depth_incr() {
    ghost_stack_depth_incr();
    return 0;
}

static int c_ghost_state_depth_decr() {
    ghost_stack_depth_decr();
    return 0;
}

static int c_postcondition_leak_check_wrapper() {
    cn_postcondition_leak_check();
    return 0;
}

static int c_update_error_msg_info_wrapper() {
    size_t length;
    const char* msg = luaL_checklstring(lua_state, 1, &length);

    update_cn_error_message_info(msg);

    return 0;
}

static int c_pop_msg_wrapper() {
    cn_pop_msg_info();
    return 0;
}

static int c_dump_error_msgs_wrapper() {
    cn_dump_error_msgs();
    return 0;
}

static int c_get_integer() {
    int* addr = (int*)luaL_checkinteger(lua_state, 1);
    lua_pushinteger(lua_state, *addr);
    return 1;
}

static int c_get_pointer() {
    void** addr = (void**)luaL_checkinteger(lua_state, 1);
    lua_pushinteger(lua_state, lua_convert_ptr_to_int(*addr));
    return 1;
}

void bind_cn_c_functions() {
    // C assert
    lua_cn_register_c_func("assert", c_assert_wrapper);

    // C ghost state
    lua_cn_register_c_func("add_to_ghost_state", c_add_to_ghost_state_wrapper);
    lua_cn_register_c_func("remove_from_ghost_state", c_remove_from_ghost_state_wrapper);
    lua_cn_register_c_func("get_or_put_ownership", c_get_or_put_ownership_wrapper);
    lua_cn_register_c_func("ghost_state_depth_incr", c_ghost_state_depth_incr);
    lua_cn_register_c_func("ghost_state_depth_decr", c_ghost_state_depth_decr);
    lua_cn_register_c_func("postcondition_leak_check", c_postcondition_leak_check_wrapper);

    // C error handling
    lua_cn_register_c_func("update_error_msg_info", c_update_error_msg_info_wrapper);
    lua_cn_register_c_func("pop_msg_info", c_pop_msg_wrapper);
    lua_cn_register_c_func("dump_error_msgs", c_dump_error_msgs_wrapper);

    // C type reading
    lua_cn_register_c_func("get_integer", c_get_integer);
    lua_cn_register_c_func("get_pointer", c_get_pointer);
}

void lua_cn_load_runtime(const char* filename) {
    assert(lua_state != NULL);

    // C runtime (keeping this as is for now, especially because some of the Lua
    // runtime still binds to C)
    initialise_ownership_ghost_state();
    initialise_ghost_stack_depth();
    alloc_ghost_array(0);
    initialise_exec_c_locs_mode(0);
    initialise_ownership_stack_mode(0);

    lua_getglobal(lua_state, "package");
    lua_getfield(lua_state, -1, "path"); // get package.path
    const char *current_path = lua_tostring(lua_state, -1);
    lua_pop(lua_state, 1);
    char new_path[1024];
    snprintf(new_path, sizeof(new_path), "%s;%s/?.lua", current_path, "./runtime/lua/cn");
    lua_pushstring(lua_state, new_path);
    lua_setfield(lua_state, -2, "path");
    lua_pop(lua_state, 1);

    if (luaL_dofile(lua_state, filename) != LUA_OK) {
        fprintf(stderr, "Unable to load lua cn runtime: %s\n", lua_tostring(lua_state, -1));
        lua_pop(lua_state, 1);
        return;
    }

    lua_cn_runtime_ref = luaL_ref(lua_state, LUA_REGISTRYINDEX);

    bind_cn_c_functions();
}

void lua_cn_unload_runtime()
{
    assert(lua_cn_runtime_ref != LUA_NOREF);

    luaL_unref(lua_state, LUA_REGISTRYINDEX, lua_cn_runtime_ref);
    lua_cn_runtime_ref = LUA_NOREF;

    // C runtime stuff
    free_ghost_array();
}

int lua_cn_get_runtime_ref() { return lua_cn_runtime_ref; }

void lua_cn_register_c_func(const char* func_name, lua_CFunction func) {
    lua_rawgeti(lua_state, LUA_REGISTRYINDEX, lua_cn_runtime_ref);
    lua_getfield(lua_state, -1, "c");
    lua_pushcfunction(lua_state, func);
    lua_setfield(lua_state, -2, func_name);
    lua_pop(lua_state, 2);
}

// Lua CN Ghost State
void lua_cn_ghost_add(void* ptr, size_t size, signed long stack_depth) {
    //@note: Kept in C for now
    c_add_to_ghost_state(ptr, size, stack_depth);
}

void lua_cn_ghost_remove(void* ptr, size_t size) {
    //@note: Kept in C for now
    c_remove_from_ghost_state(ptr, size);
}

signed long lua_cn_get_stack_depth() {
    //@note: Kept in C for now
    return get_cn_stack_depth();
}

// Lua CN Error Handling
void lua_cn_error_push(const char* msg) {
    //@note: Kept in C for now
    update_cn_error_message_info(msg);
}

void lua_cn_error_pop() {
    //@note: Kept in C for now
    cn_pop_msg_info();
}

// Lua CN Frames
void lua_cn_frame_push() {
  lua_rawgeti(lua_state, LUA_REGISTRYINDEX, lua_cn_get_runtime_ref());
  lua_getfield(lua_state, -1, "frames");
  lua_getfield(lua_state, -1, "push");

  if (lua_pcall(lua_state, 0, 0, 0) != LUA_OK) {
      fprintf(stderr, "Error calling cn.frames.push: %s\n", lua_tostring(lua_state, -1));
      lua_pop(lua_state, 1);
  }

  lua_pop(lua_state, 2);
}

void lua_cn_frame_pop() {
    lua_rawgeti(lua_state, LUA_REGISTRYINDEX, lua_cn_get_runtime_ref());
    lua_getfield(lua_state, -1, "frames");
    lua_getfield(lua_state, -1, "pop");

    if (lua_pcall(lua_state, 0, 0, 0) != LUA_OK) {
        fprintf(stderr, "Error calling cn.frames.pop: %s\n", lua_tostring(lua_state, -1));
        lua_pop(lua_state, 1);
    }

    lua_pop(lua_state, 2);
}

// Types Utils
int64_t lua_convert_ptr_to_int(void* ptr) {
    return (int64_t)(uintptr_t)ptr;
}