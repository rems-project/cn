/**
 * v2 Pass of hand-writing a version of this file that uses a Lua runtime
 * 
 * The main change from last time is that I've tried to make the Lua-C boundary one-directional
 * i.e. as much as possible, data/logic flows into Lua from C instead of marshalling types back
 * and forth. I think this is now giving a more cleaner divide in the architecture and the resulting
 * instrumented code also appears to be a lot simpler. 
 * 
 * More specifically,
 * 
 * - the entire pre and post conditions now live entirely in the Lua CN runtime land
 * - ghost allocs and error handling also live in Lua land and are only called in from C.
 * 
 * Some additional notes:
 * - I considered making two setup and exit functions that would handle both the pre/post conditions
 *   as the ghost allocs/deallocs for the function parameters and return but ghost allocs have enough granularity
 *   that I think it's better to keep them separate from the pre/post condition checks. Can discuss more in call.
 * - I've tried fleshing out more of the core Lua-CN runtime as well in this pass. See lua_cn_runtime_core.lua and 
 *   lua_cn_runtime_core_wrappers.h/c (this maps out all the wrapper calls into the lua runtime core)
 */

/**
 * This includes all the core Lua-CN utils (think of it as the corollary to cn-executable/utils.h for Lua).
 * I've tried to take any and all common logic and put it here so that the file specific work is as minimal as
 * possible.
 */
#include "lua_cn_runtime_core_wrappers.h"

/**
 * TBD on how much of these stay relevant
 */
#include <cn-executable/utils.h>
#include <cn-executable/cerb_types.h>


// C Types
struct int_list {
  signed int head;
  struct int_list* tail;
};

/**
 * File-specific Lua-CN Wrapper Prototypes
 * 
 * NOTE how this is basically only the file-specific condition wrapper calls and the ONE case where
 * we need Lua to call into C to understand types. Everything else is now in the core runtime
 * and doesn't need to be individually generated per instrumented file.
 */
static int lua_cn_read_int_list();
void lua_cn_IntList_append_precondition(struct int_list* xs, struct int_list* ys);
void lua_cn_IntList_append_postcondition(struct int_list* ret);

struct int_list* IntList_append(struct int_list* xs, struct int_list* ys)
/*@ requires take L1 = IntList(xs);
             take L2 = IntList(ys);
    ensures take L3 = IntList(return);
            L3 == append(L1, L2); @*/
{
  struct int_list* ret;

  /* EXECUTABLE CN PRECONDITION */
  lua_cn_IntList_append_precondition(xs, ys);
  
  /* C OWNERSHIP - PRE */
  lua_cn_ghost_add(&xs, sizeof(struct int_list));
  lua_cn_ghost_add(&ys, sizeof(struct int_list));
  
  /** ACTUAL C FUNCTION */
  if (xs == 0) {
      lua_cn_error_push("/*@ unfold append(L1, L2); @*/\n           ^./tests/cn/append.c:41:8-33");
      ret = ys;
      lua_cn_error_pop();
  } else {
      lua_cn_error_push("/*@ unfold append(L1, L2); @*/\n           ^./tests/cn/append.c:44:8-33");
      
      struct int_list* new_tail = IntList_append(xs->tail, ys);

      lua_cn_ghost_add(&new_tail, sizeof(struct int_list));

      xs->tail = new_tail;
      ret = xs; 

      lua_cn_ghost_remove(&new_tail, sizeof(struct int_list));

      lua_cn_error_pop();
  }

  /* C OWNERSHIP - POST */
  lua_cn_ghost_remove(&xs, sizeof(struct int_list));
  lua_cn_ghost_remove(&ys, sizeof(struct int_list));

  /* EXECUTABLE CN POSTCONDITION */
  lua_cn_IntList_append_postcondition(ret);

  return ret;
}

int main(void)
/*@ trusted; @*/
{
  signed int ret = 0;

  lua_init();
  lua_cn_load_runtime("./lua_output/append.lua");
  lua_cn_register_c_func("read_int_list", lua_cn_read_int_list);
  
  struct int_list i1 = {.head = 2, .tail = 0};
  lua_cn_ghost_add((&i1), sizeof(struct int_list));

  struct int_list i3 = {.head = 4, .tail = 0};
  lua_cn_ghost_add((&i3), sizeof(struct int_list));

  struct int_list i2 = {.head = 3, .tail = &i3};
  lua_cn_ghost_add((&i2), sizeof(struct int_list));

  struct int_list *il3 = IntList_append(&i1, &i2);
  lua_cn_ghost_add((&il3), sizeof(struct int_list*));

  lua_cn_ghost_remove((&il3), sizeof(struct int_list*));
  lua_cn_ghost_remove((&i2), sizeof(struct int_list));
  lua_cn_ghost_remove((&i3), sizeof(struct int_list));
  lua_cn_ghost_remove((&i1), sizeof(struct int_list));

  lua_cn_unload_runtime();
  lua_deinit();

  return ret;
}

// Lua CN File-specific wrapper Implementations

/**
 * See append.cn.lua for more info on this.
 */
static int lua_cn_read_int_list() {
  lua_State* L = lua_get_state();

  int64_t ptr = luaL_checkinteger(L, 1);
  struct int_list *p = (struct int_list *)ptr;

  lua_pushinteger(L, p->head);
  lua_pushinteger(L, (int64_t)p->tail);

  return 2;
}

void lua_cn_IntList_append_precondition(struct int_list* xs, struct int_list* ys) {
  lua_State* L = lua_get_state();

  lua_rawgeti(L, LUA_REGISTRYINDEX, lua_cn_get_runtime_ref());
  lua_getfield(L, -1, "IntList_append");
  lua_getfield(L, -1, "Precondition");

  lua_pushinteger(L, lua_convert_ptr_to_int(xs));
  lua_pushinteger(L, lua_convert_ptr_to_int(ys));

  if (lua_pcall(L, 2, 0, 0) != LUA_OK) {
      fprintf(stderr, "Error calling IntList_append.Precondition: %s\n", lua_tostring(L, -1));
      lua_pop(L, 1);
  }

  lua_pop(L, 2);
}

void lua_cn_IntList_append_postcondition(struct int_list* ret) {
  lua_State* L = lua_get_state();

  lua_rawgeti(L, LUA_REGISTRYINDEX, lua_cn_get_runtime_ref());
  lua_getfield(L, -1, "IntList_append");
  lua_getfield(L, -1, "Postcondition");

  lua_pushinteger(L, lua_convert_ptr_to_int(ret));

  if (lua_pcall(L, 1, 0, 0) != LUA_OK) {
      fprintf(stderr, "Error calling CN.IntList_append.Postcondition: %s\n", lua_tostring(L, -1));
      lua_pop(L, 1);
  }

  lua_pop(L, 2);
}

/**
 * ADDTL. QUESTIONS I AM PONDERING
 * 
 * 1. I ran Fulminate on append.c after removing the pre/post conditions to observe just the basic
 * instrumented load/stores and ghost allocs. Why is the stack depth only increased if there is a
 * provided pre/post condition? Surely the ghost allocations that happen still need to map to a 
 * valid stack depth (I guess it doesn't need to be 'correct' so much as consistent, so using the previous 
 * stack depth should still work but it feels like a misuse)
 */
