#define __CN_INSTRUMENT
#include <cn-executable/utils.h>
#include <cn-executable/cerb_types.h>
typedef __cerbty_intptr_t intptr_t;
typedef __cerbty_uintptr_t uintptr_t;
typedef __cerbty_intmax_t intmax_t;
typedef __cerbty_uintmax_t uintmax_t;
static const int __cerbvar_INT_MAX = 0x7fffffff;
static const int __cerbvar_INT_MIN = ~0x7fffffff;
static const unsigned long long __cerbvar_SIZE_MAX = ~(0ULL);
_Noreturn void abort(void);
/* ORIGINAL C STRUCTS AND UNIONS */

struct int_list {
  signed int head;
  struct int_list* tail;
};


/* CN VERSIONS OF C STRUCTS */

struct int_list_cn {
  cn_bits_i32* head;
  cn_pointer* tail;
};

/* CN DATATYPES */enum seq_tag {
  SEQ_NIL,
  SEQ_CONS
};

struct cn_datatype {
  signed int tag;
  struct cntype* cntype;
};

struct cntype {
  ;
};

struct seq_nil {
  ;
};

struct seq_cons {
  struct seq* tail;
  cn_bits_i32* head;
};

union seq_union {
  struct seq_nil* seq_nil;
  struct seq_cons* seq_cons;
};

struct seq {
  enum seq_tag tag;
  struct cntype* cntype;
  union seq_union u;
};



/* OWNERSHIP FUNCTIONS */

static struct int_list_cn* owned_struct_int_list(cn_pointer*, enum spec_mode, struct loop_ownership*);
/* CONVERSION FUNCTIONS */

/* GENERATED STRUCT FUNCTIONS */

static struct seq* default_struct_seq();
static void* cn_map_get_struct_seq(cn_map*, cn_integer*);
static struct int_list_cn* default_struct_int_list_cn();
static void* cn_map_get_struct_int_list_cn(cn_map*, cn_integer*);
static cn_bool* struct_seq_equality(struct seq*, struct seq*);
static cn_bool* struct_int_list_cn_equality(void*, void*);
static struct int_list convert_from_struct_int_list_cn(struct int_list_cn*);
static struct int_list_cn* convert_to_struct_int_list_cn(struct int_list);
/* RECORD FUNCTIONS */

/* CN FUNCTIONS */

static struct seq* append(struct seq*, struct seq*);
static cn_bool* addr_eq(cn_pointer*, cn_pointer*);
static cn_bool* prov_eq(cn_pointer*, cn_pointer*);
static cn_bool* ptr_eq(cn_pointer*, cn_pointer*);
static cn_bool* is_null(cn_pointer*);
static cn_bool* not(cn_bool*);
static cn_bits_u8* MINu8();
static cn_bits_u8* MAXu8();
static cn_bits_u16* MINu16();
static cn_bits_u16* MAXu16();
static cn_bits_u32* MINu32();
static cn_bits_u32* MAXu32();
static cn_bits_u64* MINu64();
static cn_bits_u64* MAXu64();
static cn_bits_i8* MINi8();
static cn_bits_i8* MAXi8();
static cn_bits_i16* MINi16();
static cn_bits_i16* MAXi16();
static cn_bits_i32* MINi32();
static cn_bits_i32* MAXi32();
static cn_bits_i64* MINi64();
static cn_bits_i64* MAXi64();

static struct seq* IntList(cn_pointer*, enum spec_mode, struct loop_ownership*);
enum CN_GHOST_ENUM {
  CLEARED,
  EMPTY
};
enum CN_GHOST_ENUM ghost_call_site;
#ifndef offsetof
#define offsetof(st, m) ((__cerbty_size_t)((char *)&((st *)0)->m - (char *)0))
#endif
#pragma GCC diagnostic ignored "-Wattributes"

/* GLOBAL ACCESSORS */
void* memcpy(void* dest, const void* src, __cerbty_size_t count );

# 1 "./tests/cn/append.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3








# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/Users/saljukgondal/.opam/5.2.0/lib/cerberus-lib/runtime/libc/include/builtins.h" 1
// Some gcc builtins we support
[[ cerb::hidden ]] int __builtin_ffs (int x);
[[ cerb::hidden ]] int __builtin_ffsl (long x);
[[ cerb::hidden ]] int __builtin_ffsll (long long x);
[[ cerb::hidden ]] int __builtin_ctz (unsigned int x);
[[ cerb::hidden ]] int __builtin_ctzl (unsigned long x);
[[ cerb::hidden ]] int __builtin_ctzll (unsigned long long x);
[[ cerb::hidden ]] __cerbty_uint16_t __builtin_bswap16 (__cerbty_uint16_t x);
[[ cerb::hidden ]] __cerbty_uint32_t __builtin_bswap32 (__cerbty_uint32_t x);
[[ cerb::hidden ]] __cerbty_uint64_t __builtin_bswap64 (__cerbty_uint64_t x);
[[ cerb::hidden ]] void __builtin_unreachable(void);

// this is an optimisation hint that we can erase
# 2 "<built-in>" 2
# 1 "./tests/cn/append.c" 2
struct int_list;



struct int_list* IntList_append(struct int_list* xs, struct int_list* ys)
/*@ requires take L1 = IntList(xs);
             take L2 = IntList(ys);
    ensures take L3 = IntList(return);
            L3 == append(L1, L2); @*/
{
  /* EXECUTABLE CN PRECONDITION */
  struct int_list* __cn_ret;
  cn_bump_frame_id __cn_bump_count_a_683 = cn_bump_get_frame_id();
  ghost_stack_depth_incr();
  cn_pointer* xs_cn = convert_to_cn_pointer(xs);
  cn_pointer* ys_cn = convert_to_cn_pointer(ys);
  ghost_call_site = CLEARED;
  update_cn_error_message_info("/*@ requires take L1 = IntList(xs);\n                  ^./tests/cn/append.c:35:19:");
  struct seq* L1_cn = IntList(xs_cn, PRE, 0);
  cn_pop_msg_info();
  update_cn_error_message_info("             take L2 = IntList(ys);\n                  ^./tests/cn/append.c:36:19:");
  struct seq* L2_cn = IntList(ys_cn, PRE, 0);
  cn_pop_msg_info();
  
	/* C OWNERSHIP */

  c_add_to_ghost_state((&xs), sizeof(struct int_list*), get_cn_stack_depth());
  cn_pointer* xs_addr_cn = convert_to_cn_pointer((&xs));
  c_add_to_ghost_state((&ys), sizeof(struct int_list*), get_cn_stack_depth());
  cn_pointer* ys_addr_cn = convert_to_cn_pointer((&ys));
  
  if (CN_LOAD(xs) == 0) {
    update_cn_error_message_info("    /*@ unfold append(L1, L2); @*/\n       ^~~~~~~~~~~~~~~~~~~~~~~~~ ./tests/cn/append.c:41:8-33");

update_cn_error_message_info("    /*@ unfold append(L1, L2); @*/\n        ^~~~~~~~~~~~~~~~~~~~~~ ./tests/cn/append.c:41:9-31");

cn_pop_msg_info();

cn_pop_msg_info();

    { __cn_ret = CN_LOAD(ys); goto __cn_epilogue; }
  } else {
    update_cn_error_message_info("    /*@ unfold append(L1, L2); @*/\n       ^~~~~~~~~~~~~~~~~~~~~~~~~ ./tests/cn/append.c:44:8-33");

update_cn_error_message_info("    /*@ unfold append(L1, L2); @*/\n        ^~~~~~~~~~~~~~~~~~~~~~ ./tests/cn/append.c:44:9-31");

cn_pop_msg_info();

cn_pop_msg_info();

    struct int_list *new_tail = (
({
  ghost_call_site = EMPTY;
  0;
})
, IntList_append(CN_LOAD(CN_LOAD(xs)->tail), CN_LOAD(ys)));
c_add_to_ghost_state((&new_tail), sizeof(struct int_list*), get_cn_stack_depth());


cn_pointer* new_tail_addr_cn = convert_to_cn_pointer((&new_tail));

    CN_STORE(CN_LOAD(xs)->tail, CN_LOAD(new_tail));
    { __cn_ret = CN_LOAD(xs); 
c_remove_from_ghost_state((&new_tail), sizeof(struct int_list*));
goto __cn_epilogue; }
  
c_remove_from_ghost_state((&new_tail), sizeof(struct int_list*));
}

/* EXECUTABLE CN POSTCONDITION */
__cn_epilogue:

  
	/* C OWNERSHIP */


  c_remove_from_ghost_state((&xs), sizeof(struct int_list*));

  c_remove_from_ghost_state((&ys), sizeof(struct int_list*));

{
  cn_pointer* return_cn = convert_to_cn_pointer(__cn_ret);
  update_cn_error_message_info("    ensures take L3 = IntList(return);\n                 ^./tests/cn/append.c:37:18:");
  struct seq* L3_cn = IntList(return_cn, POST, 0);
  cn_pop_msg_info();
  update_cn_error_message_info("            L3 == append(L1, L2); @*/\n            ^~~~~~~~~~~~~~~~~~~~~ ./tests/cn/append.c:38:13-34");
  cn_assert(struct_seq_equality(L3_cn, append(L1_cn, L2_cn)), POST);
  cn_pop_msg_info();
  ghost_stack_depth_decr();
  cn_postcondition_leak_check();
}

  cn_bump_free_after(__cn_bump_count_a_683);

return __cn_ret;

}

int main(void)
/*@ trusted; @*/
{
  /* EXECUTABLE CN PRECONDITION */
  signed int __cn_ret = 0;
  initialise_ownership_ghost_state();
  initialise_ghost_stack_depth();
  alloc_ghost_array(0);
  initialise_exec_c_locs_mode(0);
  initialise_ownership_stack_mode(0);
  
  struct int_list i1 = {.head = 2, .tail = 0};
c_add_to_ghost_state((&i1), sizeof(struct int_list), get_cn_stack_depth());


cn_pointer* i1_addr_cn = convert_to_cn_pointer((&i1));

  struct int_list i3 = {.head = 4, .tail = 0};
c_add_to_ghost_state((&i3), sizeof(struct int_list), get_cn_stack_depth());


cn_pointer* i3_addr_cn = convert_to_cn_pointer((&i3));

  struct int_list i2 = {.head = 3, .tail = &i3};
c_add_to_ghost_state((&i2), sizeof(struct int_list), get_cn_stack_depth());


cn_pointer* i2_addr_cn = convert_to_cn_pointer((&i2));


  struct int_list *il3 = (
({
  ghost_call_site = EMPTY;
  0;
})
, IntList_append(&i1, &i2));
c_add_to_ghost_state((&il3), sizeof(struct int_list*), get_cn_stack_depth());


cn_pointer* il3_addr_cn = convert_to_cn_pointer((&il3));


c_remove_from_ghost_state((&i1), sizeof(struct int_list));


c_remove_from_ghost_state((&i3), sizeof(struct int_list));


c_remove_from_ghost_state((&i2), sizeof(struct int_list));


c_remove_from_ghost_state((&il3), sizeof(struct int_list*));

/* EXECUTABLE CN POSTCONDITION */
__cn_epilogue:

  free_ghost_array();

return __cn_ret;

}

/* RECORD */

/* CONVERSION */

/* GENERATED STRUCT FUNCTIONS */

static struct seq* default_struct_seq()
{
  struct seq* res = (struct seq*) cn_bump_malloc(sizeof(struct seq));
  res->tag = SEQ_NIL;
  res->u.seq_nil = (struct seq_nil*) cn_bump_malloc(sizeof(struct seq_nil));
  return res;
}
static void* cn_map_get_struct_seq(cn_map* m, cn_integer* key)
{
  void* ret = ht_get(m, (&key->val));
  if (0 == ret)
    return (void*) default_struct_seq();
  else
    return ret;
}
static struct int_list_cn* default_struct_int_list_cn()
{
  struct int_list_cn* a_893 = (struct int_list_cn*) cn_bump_malloc(sizeof(struct int_list_cn));
  a_893->head = default_cn_bits_i32();
  a_893->tail = default_cn_pointer();
  return a_893;
}
static void* cn_map_get_struct_int_list_cn(cn_map* m, cn_integer* key)
{
  void* ret = ht_get(m, (&key->val));
  if (0 == ret)
    return (void*) default_struct_int_list_cn();
  else
    return ret;
}
static cn_bool* struct_seq_equality(struct seq* x, struct seq* y)
{
  if (x->tag != y->tag)
    return convert_to_cn_bool(0);
  else
    switch (x->tag)
  {
    case SEQ_NIL:
    {
      return convert_to_cn_bool(1);
    }
    case SEQ_CONS:
    {
      struct seq_cons* a_869 = x->u.seq_cons;
      struct seq_cons* a_870 = y->u.seq_cons;
      return cn_bool_and(struct_seq_equality(a_869->tail, a_870->tail), cn_bool_and(cn_bits_i32_equality(a_869->head, a_870->head), convert_to_cn_bool(1)));
    }
  }
  return (void*) 0;
}
static cn_bool* struct_int_list_cn_equality(void* x, void* y)
{
  struct int_list_cn* x_cast = (struct int_list_cn*) x;
  struct int_list_cn* y_cast = (struct int_list_cn*) y;
  return cn_bool_and(cn_bool_and(convert_to_cn_bool(true), cn_bits_i32_equality(x_cast->head, y_cast->head)), cn_pointer_equality(x_cast->tail, y_cast->tail));
}
static struct int_list convert_from_struct_int_list_cn(struct int_list_cn* int_list)
{
  struct int_list res;
  res.head = convert_from_cn_bits_i32(int_list->head);
  res.tail = convert_from_cn_pointer(int_list->tail);
  return res;
}
static struct int_list_cn* convert_to_struct_int_list_cn(struct int_list int_list)
{
  struct int_list_cn* res = (struct int_list_cn*) cn_bump_malloc(sizeof(struct int_list_cn));
  res->head = convert_to_cn_bits_i32(int_list.head);
  res->tail = convert_to_cn_pointer(int_list.tail);
  return res;
}
/* OWNERSHIP FUNCTIONS */

/* OWNERSHIP FUNCTIONS */

static struct int_list_cn* owned_struct_int_list(cn_pointer* cn_ptr, enum spec_mode spec_mode, struct loop_ownership* loop_ownership)
{
  void* generic_c_ptr = (void*) (struct int_list*) cn_ptr->ptr;
  cn_get_or_put_ownership(spec_mode, generic_c_ptr, sizeof(struct int_list), loop_ownership);
  return convert_to_struct_int_list_cn((*(struct int_list*) cn_ptr->ptr));
}
/* CN FUNCTIONS */
static struct seq* append(struct seq* xs, struct seq* ys)
{
  switch (xs->tag)
  {
    case SEQ_NIL:
    {
      struct seq_nil* seq_nil_1 = xs->u.seq_nil;
      return ys;
    }
    case SEQ_CONS:
    {
      struct seq_cons* seq_cons_1 = xs->u.seq_cons;
      struct seq* zs = seq_cons_1->tail;
      cn_bits_i32* h = seq_cons_1->head;
      struct seq* a_812 = (struct seq*) cn_bump_malloc(sizeof(struct seq));
      a_812->tag = SEQ_CONS;
      a_812->u.seq_cons = (struct seq_cons*) cn_bump_malloc(sizeof(struct seq_cons));
      a_812->u.seq_cons->head = h;
      a_812->u.seq_cons->tail = append(zs, ys);
      return a_812;
    }
    default:
    abort();
  }
}
static cn_bool* addr_eq(cn_pointer* arg1, cn_pointer* arg2)
{
  return cn_bits_u64_equality(cast_cn_pointer_to_cn_bits_u64(arg1), cast_cn_pointer_to_cn_bits_u64(arg2));
}
static cn_bool* prov_eq(cn_pointer* arg1, cn_pointer* arg2)
{
  return cn_alloc_id_equality(convert_to_cn_alloc_id(0), convert_to_cn_alloc_id(0));
}
static cn_bool* ptr_eq(cn_pointer* arg1, cn_pointer* arg2)
{
  return cn_pointer_equality(arg1, arg2);
}
static cn_bool* is_null(cn_pointer* arg)
{
  return cn_pointer_equality(arg, convert_to_cn_pointer(0));
}
static cn_bool* not(cn_bool* arg)
{
  return cn_bool_not(arg);
}
static cn_bits_u8* MINu8()
{
  return convert_to_cn_bits_u8(0UL);
}
static cn_bits_u8* MAXu8()
{
  return convert_to_cn_bits_u8(255UL);
}
static cn_bits_u16* MINu16()
{
  return convert_to_cn_bits_u16(0ULL);
}
static cn_bits_u16* MAXu16()
{
  return convert_to_cn_bits_u16(65535ULL);
}
static cn_bits_u32* MINu32()
{
  return convert_to_cn_bits_u32(0ULL);
}
static cn_bits_u32* MAXu32()
{
  return convert_to_cn_bits_u32(4294967295ULL);
}
static cn_bits_u64* MINu64()
{
  return convert_to_cn_bits_u64(0ULL);
}
static cn_bits_u64* MAXu64()
{
  return convert_to_cn_bits_u64(18446744073709551615ULL);
}
static cn_bits_i8* MINi8()
{
  return convert_to_cn_bits_i8((-127L - 1L));
}
static cn_bits_i8* MAXi8()
{
  return convert_to_cn_bits_i8(127L);
}
static cn_bits_i16* MINi16()
{
  return convert_to_cn_bits_i16((-32767LL - 1LL));
}
static cn_bits_i16* MAXi16()
{
  return convert_to_cn_bits_i16(32767LL);
}
static cn_bits_i32* MINi32()
{
  return convert_to_cn_bits_i32((-2147483647LL - 1LL));
}
static cn_bits_i32* MAXi32()
{
  return convert_to_cn_bits_i32(2147483647LL);
}
static cn_bits_i64* MINi64()
{
  return convert_to_cn_bits_i64((-9223372036854775807LL - 1LL));
}
static cn_bits_i64* MAXi64()
{
  return convert_to_cn_bits_i64(9223372036854775807LL);
}


/* CN PREDICATES */

static struct seq* IntList(cn_pointer* p, enum spec_mode spec_mode, struct loop_ownership* loop_ownership)
{
  if (convert_from_cn_bool(is_null(p))) {
    struct seq* a_816 = (struct seq*) cn_bump_malloc(sizeof(struct seq));
    a_816->tag = SEQ_NIL;
    return a_816;
  }
  else {
    update_cn_error_message_info("    take H = RW<struct int_list>(p);\n         ^./tests/cn/append.c:27:10:");
    struct int_list_cn* H = owned_struct_int_list(p, spec_mode, loop_ownership);
    cn_pop_msg_info();
    update_cn_error_message_info("    take tl = IntList(H.tail);\n         ^./tests/cn/append.c:28:10:");
    struct seq* tl = IntList(H->tail, spec_mode, loop_ownership);
    cn_pop_msg_info();
    struct seq* a_823 = (struct seq*) cn_bump_malloc(sizeof(struct seq));
    a_823->tag = SEQ_CONS;
    a_823->u.seq_cons = (struct seq_cons*) cn_bump_malloc(sizeof(struct seq_cons));
    a_823->u.seq_cons->head = H->head;
    a_823->u.seq_cons->tail = tl;
    return a_823;
  }
}

/* CN LEMMAS */

