#pragma once

/**
 * v1 Pass of hand-writing a version of this file that uses a Lua runtime
 * 
 * My general strategy here has been to analyse the core components of our informal C based runtime
 * and try to formalise a general strategy that 
 *  i) explicitly demarcates what the informal runtime does, and
 * ii) maps out how to do this in Lua.
 * 
 * In general, I think there's a couple things being defined by the choice of runtime:
 * - CN-types/funcs/predicates generation in C
 * - CN-specification (pre/post cond.) testing (which employs the gen'd types/funcs/predicates)
 * - Ghost state additions/removals of C allocations (which happens even if no specifications are added)
 * - Instrumentation of loads/stores (which happens even if no specifications are added)
 * - Memory management for both specification testing and ghost state tracking
 * - Error handling
 * 
 * For now, I've mapped all CN stuff directly into Lua (see append.lua-types.lua) and have a wrapper C
 * file (append.lua-wrappers.c) that can call these functions. I think there's a clear win here in terms
 * of how much cleaner/radically smaller the code becomes from not having to translate CN to a rigid 
 * C representation.
 * 
 * The ghost state tracking feels...less useful to push into Lua. Lua's GC might've been a clear win here, but 
 * having to track handles between C and Lua to reference Lua objects (which, per my understanding, is the best/desired
 * way) means GC doesn't kick in until I manually unref these handles from C (which makes it functionally equivalent
 * to manual deallocs). Still, it feels arbitary to not push this runtime element in the same place as anything 
 * else so for this pass, all of this is in Lua too. I also don't think it's entirely possible not to have this happen
 * in Lua, since some of the CN spec testing also directly manipulates the ghost state.
 * 
 * I haven't really dealt with some of the file-agnostic Lua utils (stuff that you can currently find in utils.h/c 
 * for the C runtime). I think once we have a clearer shape of this runtime, mapping out the core Lua utils will
 * be more useful.
 */

/** 
 * There will likely need to be a lua version of utils.h that abstracts the file-agnostic Lua utils out. Either that
 * or a common util library that switches between a c vs lua implementation under the hood based on the chosen 
 * runtime. For now, pretend this does all the util stuff for Lua too (specifically defining spec modes, ghost 
 * state handling, cn handles etc)
 * 
 * Some file agnostic util functions that are called within this file:
 * - lua_ghost_stack_depth_incr/decr
 * - lua_get_ghost_stack_depth
 * - lua_push/pop_error_message
 * - lua_ghost_add/remove
 * - lua_assert
 * - lua_cn_region_push/pop
 * - lua_runtime_init/deinit
 * 
 * Note that save for maybe the last two (which have been explained at their first callsites below), 
 * the first 4 have basically very easily equivalent versions in the C runtime.
 */
#include <cn-executable/utils.h>

/** 
 * I'm not sure about the need to keep this anymore. Does the raw C code really use it in any place? 
 * Or was this exclusively used for CN->C translations?
 */
#include <cn-executable/cerb_types.h>

/**
 * My goal is to push all the lua types/structs/gen functions we need (which are going to 
 * be considerably less than the C version) to a separate file to keep the main exec C file 
 * nice and tidy. I think the number one priority is for end users to be able to read this 
 * file and easily map it out to the original C file. In my personal/naive/uneducated opinion, 
 * I think the current state of the exec file makes this really difficult with all the other 
 * boilerplate junk it spits out. In an ideal world, the exec file contains the *same* types and 
 * functions as the original file (in this case, IntList, IntList_append and a main) and no more/less.
 * 
 * The wrapper C file included below contains all other types. I envision that there will also be 
 * one another file that will contain the actual generated lua code that the wrapper file shall 
 * call. This .lua file may be linked in as a library at init time in main.
 */
#include <append.lua-wrappers.c>

struct int_list {
  signed int head;
  struct int_list* tail;
};

struct int_list* IntList_append(struct int_list* xs, struct int_list* ys)
/*@ requires take L1 = IntList(xs);
             take L2 = IntList(ys);
    ensures take L3 = IntList(return);
            L3 == append(L1, L2); @*/
{
  struct int_list* ret;

  /* ENTRY BOOKKEEPING */
  // Instead of manually having to unref each cn handle, this is in many ways similar to the current bump allocator
  // being used. The implementation (for starters at least), will just be a region in C that tracks the current frame's handles 
  // to the mostly recently pushed region. Upon popping, we go and unref all the handles, and then Lua's free to go GC them all
  // whenever it wants. This still feels pretty manual though :(.
  lua_cn_region_id region_id = lua_cn_region_push(); 
  lua_ghost_stack_depth_incr();

  /* EXECUTABLE CN PRECONDITION */
  lua_cn_handle xs_cn = lua_convert_to_cn_pointer(xs);
  lua_cn_handle ys_cn = lua_convert_to_cn_pointer(ys);

  lua_push_error_message("/*@ requires take L1 = IntList(xs);\n          ^./tests/cn/append.c:35:19:");
  lua_cn_handle L1_cn = lua_IntList(xs_cn, PRE);
  lua_pop_error_message();

  lua_push_error_message("/*@ requires take L2 = IntList(xs);\n          ^./tests/cn/append.c:36:19:");
  lua_cn_handle L2_cn = lua_IntList(ys_cn, PRE);
  lua_pop_error_message();
  
  /* C OWNERSHIP - PRE */
  lua_ghost_add(&xs, sizeof(struct int_list), lua_get_ghost_stack_depth());
  lua_ghost_add(&ys, sizeof(struct int_list), lua_get_ghost_stack_depth());
  
  /** ACTUAL C FUNCTION */
  if (CN_LOAD(xs) == 0) {
      lua_push_error_message("/*@ unfold append(L1, L2); @*/\n           ^./tests/cn/append.c:41:8-33");
      ret = ys;
      lua_pop_error_message();
  } else {
      lua_push_error_message("/*@ unfold append(L1, L2); @*/\n           ^./tests/cn/append.c:44:8-33");
      
      struct int_list* new_tail = IntList_append(CN_LOAD(CN_LOAD(xs)->tail), CN_LOAD(ys));

      lua_ghost_add(&new_tail, sizeof(struct int_list), lua_get_ghost_stack_depth());

      CN_STORE(CN_LOAD(xs)->tail, CN_LOAD(new_tail));
      ret = CN_LOAD(xs); 

      lua_ghost_remove(&new_tail, sizeof(struct int_list));

      lua_pop_error_message();
  }

  /* C OWNERSHIP - POST */
  lua_ghost_remove(&xs, sizeof(struct int_list));
  lua_ghost_remove(&ys, sizeof(struct int_list));

  /* EXECUTABLE CN POSTCONDITION */
  lua_cn_handle ret_cn = lua_convert_to_cn_pointer(ret);

  lua_push_error_message("take L3 = IntList(return)                      ^./tests/cn/append.c:37:13:");
  lua_cn_handle L3 = lua_IntList(ret_cn, POST);
  lua_pop_error_message();

  lua_push_error_message("L3 == append(L1, L2)                           ^./tests/cn/append.c:38:13:");
  lua_cn_handle L1_L2 = lua_append(L1_cn, L2_cn);
  lua_pop_error_message();
  lua_assert(lua_seq_equal(L3, L1_L2));
  lua_pop_error_message();

  /** EXIT BOOKKEEPING */
  lua_ghost_stack_depth_decr();
  lua_cn_region_pop(region_id);
}

int main(void)
/*@ trusted; @*/
{
  signed int ret = 0;

  // This should, among other things, do the following work:
  // - create the global Lua state
  // - open the core Lua library
  // - open the generated Lua library for this file (see append.lua-types.lua)
  // - create management structures (ghost state, region handling)
  lua_runtime_init();
  
  struct int_list i1 = {.head = 2, .tail = 0};
  lua_ghost_add((&i1), sizeof(struct int_list), get_cn_stack_depth());

  struct int_list i3 = {.head = 4, .tail = 0};
  lua_ghost_add((&i3), sizeof(struct int_list), get_cn_stack_depth());

  struct int_list i2 = {.head = 3, .tail = &i3};
  lua_ghost_add((&i2), sizeof(struct int_list), get_cn_stack_depth());

  struct int_list *il3 = IntList_append(&i1, &i2);
  lua_ghost_add((&il3), sizeof(struct int_list*), get_cn_stack_depth());

  lua_ghost_remove((&il3), sizeof(struct int_list*), get_cn_stack_depth());
  lua_ghost_remove((&i2), sizeof(struct int_list));
  lua_ghost_remove((&i3), sizeof(struct int_list));
  lua_ghost_remove((&i1), sizeof(struct int_list));

  lua_runtime_deinit();

  return ret;
}

/**
 * QUESTIONS I'M LEFT PONDERING
 * 
 * 1. In the C runtime instrumentation, why are there SO many generated funcs for equalities/comparisons/defaults of each user
 * type even when they are not actually used? Do we just run everything through the automatic code gen regardless of usage?
 * If so, what are example test cases where these are used? I'm lacking context about why these are necessary
 * 
 * 2. In virtually every place we add to the ghost state, we have a followup line that proceeds to convert 
 * the address of the C arg to a cn pointer. In most cases, this CN pointer remains unused. What's the utility of this?
 * 
 * 3. This is my lack of clear understanding of CN/separation logic as a whole but I'm still a little unsure
 * about the goal behind the assertions being made in the CN specc'ed predicate for IntList. Why are we taking a C int_list
 * and converting it into a seq? And does the RW entail ensuring that each part of the seq is adequately tracked in the
 * ghost state?
 */
