#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <bennet/utils/vector.h>
#include <cn-executable/eval.h>
#include <cn-smt/datatypes.h>
#include <cn-smt/eval.h>
#include <cn-smt/functions.h>
#include <cn-smt/memory/intern.h>
#include <cn-smt/memory/test_alloc.h>
#include <cn-smt/records.h>
#include <cn-smt/structs.h>

// Generate vector implementation for cn_member_pair, cn_term_ptr, and cn_match_case
BENNET_VECTOR_IMPL(cn_member_pair)
BENNET_VECTOR_IMPL(cn_term_ptr)
BENNET_VECTOR_IMPL(cn_match_case)

// Generate hash table implementation for cn_sym -> void_ptr
BENNET_HASH_TABLE_IMPL(cn_sym, void_ptr)

// Evaluation context stack (linked list of hash tables for scoped bindings)
typedef struct cn_eval_context_node {
  bennet_hash_table(cn_sym, void_ptr) table;
  struct cn_eval_context_node* next;  // Next node down the stack
} cn_eval_context_node;

typedef struct cn_eval_context_stack {
  cn_eval_context_node* head;  // Top of the stack
} cn_eval_context_stack;

// Forward declarations
static cn_bits_info get_bits_info(cn_term* term);
static void* cn_eval_term_aux(cn_eval_context_stack* context, cn_term* term);

// Static stack operation functions for eval context
static cn_eval_context_stack* create_eval_context_stack(void) {
  cn_eval_context_stack* stack = cn_test_malloc(sizeof(cn_eval_context_stack));
  assert(stack);

  // Allocate and initialize the first node
  cn_eval_context_node* node = cn_test_malloc(sizeof(cn_eval_context_node));
  assert(node);

  bennet_hash_table_init(cn_sym, void_ptr)(
      &node->table, bennet_hash_cn_sym, bennet_eq_cn_sym);
  node->next = NULL;

  stack->head = node;
  return stack;
}

static void free_eval_context_stack(cn_eval_context_stack* stack) {
  if (!stack) {
    return;
  }

  // Pop and free all nodes
  while (stack->head) {
    cn_eval_context_node* node = stack->head;
    stack->head = node->next;
    bennet_hash_table_free(cn_sym, void_ptr)(&node->table);
    cn_test_free(node);
  }

  cn_test_free(stack);
}

static void push_eval_context(cn_eval_context_stack* stack) {
  assert(stack);

  // Allocate and initialize new node
  cn_eval_context_node* node = cn_test_malloc(sizeof(cn_eval_context_node));
  assert(node);

  bennet_hash_table_init(cn_sym, void_ptr)(
      &node->table, bennet_hash_cn_sym, bennet_eq_cn_sym);
  node->next = stack->head;

  // Make it the new head
  stack->head = node;
}

static void pop_eval_context(cn_eval_context_stack* stack) {
  assert(stack);
  assert(stack->head);  // Cannot pop from empty stack

  // Remove head node
  cn_eval_context_node* node = stack->head;
  stack->head = node->next;

  // Free the node's table and the node itself
  bennet_hash_table_free(cn_sym, void_ptr)(&node->table);
  cn_test_free(node);
}

static void set_eval_binding(cn_eval_context_stack* stack, cn_sym key, void* value) {
  assert(stack);
  assert(stack->head);
  assert(value);

  bennet_hash_table_set(cn_sym, void_ptr)(&stack->head->table, key, value);
}

static void* get_eval_binding(cn_eval_context_stack* stack, cn_sym key) {
  if (!stack) {
    return NULL;
  }

  // Search from top (head) down the stack
  for (cn_eval_context_node* node = stack->head; node != NULL; node = node->next) {
    bennet_optional(void_ptr) result =
        bennet_hash_table_get(cn_sym, void_ptr)(&node->table, key);
    if (bennet_optional_is_some(result)) {
      return bennet_optional_unwrap(result);
    }
  }

  return NULL;
}

static void* cn_eval_term_aux(cn_eval_context_stack* context, cn_term* term) {
  assert(context);
  assert(term);

  switch (term->type) {
    case CN_TERM_CONST: {
      switch (term->data.const_val.type) {
        case CN_CONST_Z:
          return convert_to_cn_integer(term->data.const_val.data.z);
        case CN_CONST_BOOL:
          return convert_to_cn_bool(term->data.const_val.data.boolean);
        case CN_CONST_BITS: {
          cn_bits_info info = term->data.const_val.data.bits.info;
          if (info.is_signed) {
            switch (info.size_bits) {
              case 8:
                return convert_to_cn_bits_i8(term->data.const_val.data.bits.value);
              case 16:
                return convert_to_cn_bits_i16(term->data.const_val.data.bits.value);
              case 32:
                return convert_to_cn_bits_i32(term->data.const_val.data.bits.value);
              case 64:
                return convert_to_cn_bits_i64(term->data.const_val.data.bits.value);
            }
          } else {
            switch (info.size_bits) {
              case 8:
                return convert_to_cn_bits_u8(term->data.const_val.data.bits.value);
              case 16:
                return convert_to_cn_bits_u16(term->data.const_val.data.bits.value);
              case 32:
                return convert_to_cn_bits_u32(term->data.const_val.data.bits.value);
              case 64:
                return convert_to_cn_bits_u64(term->data.const_val.data.bits.value);
            }
          }
          assert(false);
        }
        case CN_CONST_Q:
          // Rational values represented as double
          // Note: Need to implement cn_rational type if precise rationals are needed
          assert(false);
        case CN_CONST_POINTER:
          return convert_to_cn_pointer((void*)term->data.const_val.data.pointer);
        case CN_CONST_UNIT:
          // Unit type - no data, just return a marker
          // Could use a singleton unit value or just return a special pointer
          assert(false);
        case CN_CONST_NULL:
          return convert_to_cn_pointer(NULL);
        case CN_CONST_CTYPE_CONST:
          // C type constants - likely need custom handling
          assert(false);
        case CN_CONST_DEFAULT: {
          // Default values based on the type
          switch (term->data.const_val.data.default_type.tag) {
            case CN_BASE_BOOL:
              return default_cn_bool();
            case CN_BASE_INTEGER:
              return convert_to_cn_integer(0);
            case CN_BASE_LOC:
              return default_cn_pointer();
            case CN_BASE_MAP:
              return default_cn_map();
            case CN_BASE_STRUCT: {
              const char* struct_tag =
                  term->data.const_val.data.default_type.data.struct_tag.tag;
              return cn_smt_struct_default(struct_tag);
            }
            case CN_BASE_BITS: {
              cn_bits_info bits_info =
                  cn_base_type_get_bits_info(term->data.const_val.data.default_type);
              if (bits_info.is_signed) {
                switch (bits_info.size_bits) {
                  case 8:
                    return default_cn_bits_i8();
                  case 16:
                    return default_cn_bits_i16();
                  case 32:
                    return default_cn_bits_i32();
                  case 64:
                    return default_cn_bits_i64();
                  default:
                    assert(false && "Unsupported signed bit width");
                }
              } else {
                switch (bits_info.size_bits) {
                  case 8:
                    return default_cn_bits_u8();
                  case 16:
                    return default_cn_bits_u16();
                  case 32:
                    return default_cn_bits_u32();
                  case 64:
                    return default_cn_bits_u64();
                  default:
                    assert(false && "Unsupported unsigned bit width");
                }
              }
            }
            default:
              assert(false);
          }
        }
        default:
          assert(false);
      }
    }

    case CN_TERM_SYM: {
      // Symbol evaluation - lookup in context stack
      void* result = get_eval_binding(context, term->data.sym);
      assert(result);  // Symbol must be in context
      return result;
    }

    case CN_TERM_UNOP: {
      void* operand_val = cn_eval_term_aux(context, term->data.unop.operand);
      assert(operand_val);

      switch (term->data.unop.op) {
        case CN_UNOP_NOT:
          return cn_bool_not((cn_bool*)operand_val);
        case CN_UNOP_NEGATE: {
          if (cn_base_type_is(term->data.unop.operand->base_type, CN_BASE_BITS)) {
            cn_bits_info info = get_bits_info(term->data.unop.operand);
            if (info.is_signed) {
              switch (info.size_bits) {
                case 8:
                  return cn_bits_i8_negate((cn_bits_i8*)operand_val);
                case 16:
                  return cn_bits_i16_negate((cn_bits_i16*)operand_val);
                case 32:
                  return cn_bits_i32_negate((cn_bits_i32*)operand_val);
                case 64:
                  return cn_bits_i64_negate((cn_bits_i64*)operand_val);
              }
            } else {
              switch (info.size_bits) {
                case 8:
                  return cn_bits_u8_negate((cn_bits_u8*)operand_val);
                case 16:
                  return cn_bits_u16_negate((cn_bits_u16*)operand_val);
                case 32:
                  return cn_bits_u32_negate((cn_bits_u32*)operand_val);
                case 64:
                  return cn_bits_u64_negate((cn_bits_u64*)operand_val);
              }
            }
            assert(false);
          } else {
            return cn_integer_negate((cn_integer*)operand_val);
          }
        }
        case CN_UNOP_BW_COMPL: {
          if (cn_base_type_is(term->data.unop.operand->base_type, CN_BASE_BITS)) {
            cn_bits_info info = get_bits_info(term->data.unop.operand);
            if (info.is_signed) {
              switch (info.size_bits) {
                case 8:
                  return cn_bits_i8_bw_compl((cn_bits_i8*)operand_val);
                case 16:
                  return cn_bits_i16_bw_compl((cn_bits_i16*)operand_val);
                case 32:
                  return cn_bits_i32_bw_compl((cn_bits_i32*)operand_val);
                case 64:
                  return cn_bits_i64_bw_compl((cn_bits_i64*)operand_val);
              }
            } else {
              switch (info.size_bits) {
                case 8:
                  return cn_bits_u8_bw_compl((cn_bits_u8*)operand_val);
                case 16:
                  return cn_bits_u16_bw_compl((cn_bits_u16*)operand_val);
                case 32:
                  return cn_bits_u32_bw_compl((cn_bits_u32*)operand_val);
                case 64:
                  return cn_bits_u64_bw_compl((cn_bits_u64*)operand_val);
              }
            }
            assert(false);
          } else {
            assert(false);
          }
        }
        case CN_UNOP_BW_CLZ_NOSMT:
        case CN_UNOP_BW_CTZ_NOSMT:
        case CN_UNOP_BW_FFS_NOSMT:
          assert(false);
        case CN_UNOP_BW_FLS_NOSMT: {
          if (cn_base_type_is(term->data.unop.operand->base_type, CN_BASE_BITS)) {
            cn_bits_info info = get_bits_info(term->data.unop.operand);
            if (!info.is_signed) {
              switch (info.size_bits) {
                case 32:
                  return cn_bits_u32_fls((cn_bits_u32*)operand_val);
                case 64:
                  return cn_bits_u64_flsl((cn_bits_u64*)operand_val);
                default:
                  assert(false);
              }
            } else {
              assert(false);
            }
          } else {
            assert(false);
          }
        }
        default:
          assert(false);
      }
    }

    case CN_TERM_BINOP: {
      void* left_val = cn_eval_term_aux(context, term->data.binop.left);
      void* right_val = cn_eval_term_aux(context, term->data.binop.right);
      assert(left_val && right_val);

      if (cn_base_type_is(term->data.binop.left->base_type, CN_BASE_BITS)) {
        cn_bits_info info = get_bits_info(term->data.binop.left);
        if (info.is_signed) {
          switch (info.size_bits) {
            case 8: {
              cn_bits_i8* left = (cn_bits_i8*)left_val;
              cn_bits_i8* right = (cn_bits_i8*)right_val;
              switch (term->data.binop.op) {
                case CN_BINOP_ADD:
                  return cn_bits_i8_add(left, right);
                case CN_BINOP_SUB:
                  return cn_bits_i8_sub(left, right);
                case CN_BINOP_MUL:
                case CN_BINOP_MULNOSMT:
                  return cn_bits_i8_multiply(left, right);
                case CN_BINOP_DIV:
                case CN_BINOP_DIVNOSMT:
                  return cn_bits_i8_divide(left, right);
                case CN_BINOP_REM:
                case CN_BINOP_REMNOSMT:
                  return cn_bits_i8_rem(left, right);
                case CN_BINOP_MOD:
                case CN_BINOP_MODNOSMT:
                  return cn_bits_i8_mod(left, right);
                case CN_BINOP_BW_XOR:
                  return cn_bits_i8_xor(left, right);
                case CN_BINOP_BW_AND:
                  return cn_bits_i8_bwand(left, right);
                case CN_BINOP_BW_OR:
                  return cn_bits_i8_bwor(left, right);
                case CN_BINOP_SHIFT_LEFT:
                  return cn_bits_i8_shift_left(left, right);
                case CN_BINOP_SHIFT_RIGHT:
                  return cn_bits_i8_shift_right(left, right);
                case CN_BINOP_LT:
                  return cn_bits_i8_lt(left, right);
                case CN_BINOP_LE:
                  return cn_bits_i8_le(left, right);
                case CN_BINOP_MIN:
                  return cn_bits_i8_min(left, right);
                case CN_BINOP_MAX:
                  return cn_bits_i8_max(left, right);
                case CN_BINOP_EQ:
                  return cn_bits_i8_equality(left, right);
                default:
                  // This should never happen - logical ops don't apply to bitvectors
                  assert(false);
              }
              break;
            }
            case 16: {
              cn_bits_i16* left = (cn_bits_i16*)left_val;
              cn_bits_i16* right = (cn_bits_i16*)right_val;
              switch (term->data.binop.op) {
                case CN_BINOP_ADD:
                  return cn_bits_i16_add(left, right);
                case CN_BINOP_SUB:
                  return cn_bits_i16_sub(left, right);
                case CN_BINOP_MUL:
                case CN_BINOP_MULNOSMT:
                  return cn_bits_i16_multiply(left, right);
                case CN_BINOP_DIV:
                case CN_BINOP_DIVNOSMT:
                  return cn_bits_i16_divide(left, right);
                case CN_BINOP_REM:
                case CN_BINOP_REMNOSMT:
                  return cn_bits_i16_rem(left, right);
                case CN_BINOP_MOD:
                case CN_BINOP_MODNOSMT:
                  return cn_bits_i16_mod(left, right);
                case CN_BINOP_BW_XOR:
                  return cn_bits_i16_xor(left, right);
                case CN_BINOP_BW_AND:
                  return cn_bits_i16_bwand(left, right);
                case CN_BINOP_BW_OR:
                  return cn_bits_i16_bwor(left, right);
                case CN_BINOP_SHIFT_LEFT:
                  return cn_bits_i16_shift_left(left, right);
                case CN_BINOP_SHIFT_RIGHT:
                  return cn_bits_i16_shift_right(left, right);
                case CN_BINOP_LT:
                  return cn_bits_i16_lt(left, right);
                case CN_BINOP_LE:
                  return cn_bits_i16_le(left, right);
                case CN_BINOP_MIN:
                  return cn_bits_i16_min(left, right);
                case CN_BINOP_MAX:
                  return cn_bits_i16_max(left, right);
                case CN_BINOP_EQ:
                  return cn_bits_i16_equality(left, right);
                default:
                  // This should never happen - logical ops don't apply to bitvectors
                  assert(false);
              }
              break;
            }
            case 32: {
              cn_bits_i32* left = (cn_bits_i32*)left_val;
              cn_bits_i32* right = (cn_bits_i32*)right_val;
              switch (term->data.binop.op) {
                case CN_BINOP_ADD:
                  return cn_bits_i32_add(left, right);
                case CN_BINOP_SUB:
                  return cn_bits_i32_sub(left, right);
                case CN_BINOP_MUL:
                case CN_BINOP_MULNOSMT:
                  return cn_bits_i32_multiply(left, right);
                case CN_BINOP_DIV:
                case CN_BINOP_DIVNOSMT:
                  return cn_bits_i32_divide(left, right);
                case CN_BINOP_REM:
                case CN_BINOP_REMNOSMT:
                  return cn_bits_i32_rem(left, right);
                case CN_BINOP_MOD:
                case CN_BINOP_MODNOSMT:
                  return cn_bits_i32_mod(left, right);
                case CN_BINOP_BW_XOR:
                  return cn_bits_i32_xor(left, right);
                case CN_BINOP_BW_AND:
                  return cn_bits_i32_bwand(left, right);
                case CN_BINOP_BW_OR:
                  return cn_bits_i32_bwor(left, right);
                case CN_BINOP_SHIFT_LEFT:
                  return cn_bits_i32_shift_left(left, right);
                case CN_BINOP_SHIFT_RIGHT:
                  return cn_bits_i32_shift_right(left, right);
                case CN_BINOP_LT:
                  return cn_bits_i32_lt(left, right);
                case CN_BINOP_LE:
                  return cn_bits_i32_le(left, right);
                case CN_BINOP_MIN:
                  return cn_bits_i32_min(left, right);
                case CN_BINOP_MAX:
                  return cn_bits_i32_max(left, right);
                case CN_BINOP_EQ:
                  return cn_bits_i32_equality(left, right);
                default:
                  // This should never happen - logical ops don't apply to bitvectors
                  assert(false);
              }
              break;
            }
            case 64: {
              cn_bits_i64* left = (cn_bits_i64*)left_val;
              cn_bits_i64* right = (cn_bits_i64*)right_val;
              switch (term->data.binop.op) {
                case CN_BINOP_ADD:
                  return cn_bits_i64_add(left, right);
                case CN_BINOP_SUB:
                  return cn_bits_i64_sub(left, right);
                case CN_BINOP_MUL:
                case CN_BINOP_MULNOSMT:
                  return cn_bits_i64_multiply(left, right);
                case CN_BINOP_DIV:
                case CN_BINOP_DIVNOSMT:
                  return cn_bits_i64_divide(left, right);
                case CN_BINOP_REM:
                case CN_BINOP_REMNOSMT:
                  return cn_bits_i64_rem(left, right);
                case CN_BINOP_MOD:
                case CN_BINOP_MODNOSMT:
                  return cn_bits_i64_mod(left, right);
                case CN_BINOP_BW_XOR:
                  return cn_bits_i64_xor(left, right);
                case CN_BINOP_BW_AND:
                  return cn_bits_i64_bwand(left, right);
                case CN_BINOP_BW_OR:
                  return cn_bits_i64_bwor(left, right);
                case CN_BINOP_SHIFT_LEFT:
                  return cn_bits_i64_shift_left(left, right);
                case CN_BINOP_SHIFT_RIGHT:
                  return cn_bits_i64_shift_right(left, right);
                case CN_BINOP_LT:
                  return cn_bits_i64_lt(left, right);
                case CN_BINOP_LE:
                  return cn_bits_i64_le(left, right);
                case CN_BINOP_MIN:
                  return cn_bits_i64_min(left, right);
                case CN_BINOP_MAX:
                  return cn_bits_i64_max(left, right);
                case CN_BINOP_EQ:
                  return cn_bits_i64_equality(left, right);
                default:
                  // This should never happen - logical ops don't apply to bitvectors
                  assert(false);
              }
              break;
            }
          }
        } else {
          // unsigned bitvector cases
          switch (info.size_bits) {
            case 8: {
              cn_bits_u8* left = (cn_bits_u8*)left_val;
              cn_bits_u8* right = (cn_bits_u8*)right_val;
              switch (term->data.binop.op) {
                case CN_BINOP_ADD:
                  return cn_bits_u8_add(left, right);
                case CN_BINOP_SUB:
                  return cn_bits_u8_sub(left, right);
                case CN_BINOP_MUL:
                case CN_BINOP_MULNOSMT:
                  return cn_bits_u8_multiply(left, right);
                case CN_BINOP_DIV:
                case CN_BINOP_DIVNOSMT:
                  return cn_bits_u8_divide(left, right);
                case CN_BINOP_REM:
                case CN_BINOP_REMNOSMT:
                  return cn_bits_u8_rem(left, right);
                case CN_BINOP_MOD:
                case CN_BINOP_MODNOSMT:
                  return cn_bits_u8_mod(left, right);
                case CN_BINOP_BW_XOR:
                  return cn_bits_u8_xor(left, right);
                case CN_BINOP_BW_AND:
                  return cn_bits_u8_bwand(left, right);
                case CN_BINOP_BW_OR:
                  return cn_bits_u8_bwor(left, right);
                case CN_BINOP_SHIFT_LEFT:
                  return cn_bits_u8_shift_left(left, right);
                case CN_BINOP_SHIFT_RIGHT:
                  return cn_bits_u8_shift_right(left, right);
                case CN_BINOP_LT:
                  return cn_bits_u8_lt(left, right);
                case CN_BINOP_LE:
                  return cn_bits_u8_le(left, right);
                case CN_BINOP_MIN:
                  return cn_bits_u8_min(left, right);
                case CN_BINOP_MAX:
                  return cn_bits_u8_max(left, right);
                case CN_BINOP_EQ:
                  return cn_bits_u8_equality(left, right);
                default:
                  // This should never happen - logical ops don't apply to bitvectors
                  assert(false);
              }
              break;
            }
            case 16: {
              cn_bits_u16* left = (cn_bits_u16*)left_val;
              cn_bits_u16* right = (cn_bits_u16*)right_val;
              switch (term->data.binop.op) {
                case CN_BINOP_ADD:
                  return cn_bits_u16_add(left, right);
                case CN_BINOP_SUB:
                  return cn_bits_u16_sub(left, right);
                case CN_BINOP_MUL:
                case CN_BINOP_MULNOSMT:
                  return cn_bits_u16_multiply(left, right);
                case CN_BINOP_DIV:
                case CN_BINOP_DIVNOSMT:
                  return cn_bits_u16_divide(left, right);
                case CN_BINOP_REM:
                case CN_BINOP_REMNOSMT:
                  return cn_bits_u16_rem(left, right);
                case CN_BINOP_MOD:
                case CN_BINOP_MODNOSMT:
                  return cn_bits_u16_mod(left, right);
                case CN_BINOP_BW_XOR:
                  return cn_bits_u16_xor(left, right);
                case CN_BINOP_BW_AND:
                  return cn_bits_u16_bwand(left, right);
                case CN_BINOP_BW_OR:
                  return cn_bits_u16_bwor(left, right);
                case CN_BINOP_SHIFT_LEFT:
                  return cn_bits_u16_shift_left(left, right);
                case CN_BINOP_SHIFT_RIGHT:
                  return cn_bits_u16_shift_right(left, right);
                case CN_BINOP_LT:
                  return cn_bits_u16_lt(left, right);
                case CN_BINOP_LE:
                  return cn_bits_u16_le(left, right);
                case CN_BINOP_MIN:
                  return cn_bits_u16_min(left, right);
                case CN_BINOP_MAX:
                  return cn_bits_u16_max(left, right);
                case CN_BINOP_EQ:
                  return cn_bits_u16_equality(left, right);
                default:
                  // This should never happen - logical ops don't apply to bitvectors
                  assert(false);
              }
              break;
            }
            case 32: {
              cn_bits_u32* left = (cn_bits_u32*)left_val;
              cn_bits_u32* right = (cn_bits_u32*)right_val;
              switch (term->data.binop.op) {
                case CN_BINOP_ADD:
                  return cn_bits_u32_add(left, right);
                case CN_BINOP_SUB:
                  return cn_bits_u32_sub(left, right);
                case CN_BINOP_MUL:
                case CN_BINOP_MULNOSMT:
                  return cn_bits_u32_multiply(left, right);
                case CN_BINOP_DIV:
                case CN_BINOP_DIVNOSMT:
                  return cn_bits_u32_divide(left, right);
                case CN_BINOP_REM:
                case CN_BINOP_REMNOSMT:
                  return cn_bits_u32_rem(left, right);
                case CN_BINOP_MOD:
                case CN_BINOP_MODNOSMT:
                  return cn_bits_u32_mod(left, right);
                case CN_BINOP_BW_XOR:
                  return cn_bits_u32_xor(left, right);
                case CN_BINOP_BW_AND:
                  return cn_bits_u32_bwand(left, right);
                case CN_BINOP_BW_OR:
                  return cn_bits_u32_bwor(left, right);
                case CN_BINOP_SHIFT_LEFT:
                  return cn_bits_u32_shift_left(left, right);
                case CN_BINOP_SHIFT_RIGHT:
                  return cn_bits_u32_shift_right(left, right);
                case CN_BINOP_LT:
                  return cn_bits_u32_lt(left, right);
                case CN_BINOP_LE:
                  return cn_bits_u32_le(left, right);
                case CN_BINOP_MIN:
                  return cn_bits_u32_min(left, right);
                case CN_BINOP_MAX:
                  return cn_bits_u32_max(left, right);
                case CN_BINOP_EQ:
                  return cn_bits_u32_equality(left, right);
                default:
                  // This should never happen - logical ops don't apply to bitvectors
                  assert(false);
              }
              break;
            }
            case 64: {
              cn_bits_u64* left = (cn_bits_u64*)left_val;
              cn_bits_u64* right = (cn_bits_u64*)right_val;
              switch (term->data.binop.op) {
                case CN_BINOP_ADD:
                  return cn_bits_u64_add(left, right);
                case CN_BINOP_SUB:
                  return cn_bits_u64_sub(left, right);
                case CN_BINOP_MUL:
                case CN_BINOP_MULNOSMT:
                  return cn_bits_u64_multiply(left, right);
                case CN_BINOP_DIV:
                case CN_BINOP_DIVNOSMT:
                  return cn_bits_u64_divide(left, right);
                case CN_BINOP_REM:
                case CN_BINOP_REMNOSMT:
                  return cn_bits_u64_rem(left, right);
                case CN_BINOP_MOD:
                case CN_BINOP_MODNOSMT:
                  return cn_bits_u64_mod(left, right);
                case CN_BINOP_BW_XOR:
                  return cn_bits_u64_xor(left, right);
                case CN_BINOP_BW_AND:
                  return cn_bits_u64_bwand(left, right);
                case CN_BINOP_BW_OR:
                  return cn_bits_u64_bwor(left, right);
                case CN_BINOP_SHIFT_LEFT:
                  return cn_bits_u64_shift_left(left, right);
                case CN_BINOP_SHIFT_RIGHT:
                  return cn_bits_u64_shift_right(left, right);
                case CN_BINOP_LT:
                  return cn_bits_u64_lt(left, right);
                case CN_BINOP_LE:
                  return cn_bits_u64_le(left, right);
                case CN_BINOP_MIN:
                  return cn_bits_u64_min(left, right);
                case CN_BINOP_MAX:
                  return cn_bits_u64_max(left, right);
                case CN_BINOP_EQ:
                  return cn_bits_u64_equality(left, right);
                default:
                  // This should never happen - logical ops don't apply to bitvectors
                  assert(false);
              }
              break;
            }
          }
        }
        assert(false);
      } else {
        // Non-bitvector operations (integers, booleans, etc.)
        switch (term->data.binop.op) {
          case CN_BINOP_ADD:
            return cn_integer_add((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_SUB:
            return cn_integer_sub((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_MUL:
          case CN_BINOP_MULNOSMT:
            return cn_integer_multiply((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_DIV:
          case CN_BINOP_DIVNOSMT:
            return cn_integer_divide((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_REM:
          case CN_BINOP_REMNOSMT:
            return cn_integer_rem((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_MOD:
          case CN_BINOP_MODNOSMT:
            return cn_integer_mod((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_LT:
            return cn_integer_lt((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_LE:
            return cn_integer_le((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_MIN:
            return cn_integer_min((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_MAX:
            return cn_integer_max((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_BW_XOR:
            return cn_integer_xor((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_BW_AND:
            return cn_integer_bwand((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_BW_OR:
            return cn_integer_bwor((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_SHIFT_LEFT:
            return cn_integer_shift_left((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_SHIFT_RIGHT:
            return cn_integer_shift_right((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_AND:
            return cn_bool_and((cn_bool*)left_val, (cn_bool*)right_val);
          case CN_BINOP_OR:
            return cn_bool_or((cn_bool*)left_val, (cn_bool*)right_val);
          case CN_BINOP_IMPLIES:
            return cn_bool_implies((cn_bool*)left_val, (cn_bool*)right_val);
          case CN_BINOP_LT_POINTER:
            return cn_pointer_lt((cn_pointer*)left_val, (cn_pointer*)right_val);
          case CN_BINOP_LE_POINTER:
            return cn_pointer_le((cn_pointer*)left_val, (cn_pointer*)right_val);
          case CN_BINOP_EQ: {
            if (cn_base_type_is(term->data.binop.left->base_type, CN_BASE_BOOL)) {
              return cn_bool_equality((cn_bool*)left_val, (cn_bool*)right_val);
            } else if (cn_base_type_is(term->data.binop.left->base_type, CN_BASE_LOC)) {
              return cn_pointer_equality(left_val, right_val);
            } else {
              return cn_integer_equality(left_val, right_val);
            }
          }
          // Set operations - placeholders for now
          case CN_BINOP_SET_UNION:
          case CN_BINOP_SET_INTERSECTION:
          case CN_BINOP_SET_DIFFERENCE:
          case CN_BINOP_SET_MEMBER:
          case CN_BINOP_SUBSET:
            assert(false);
            // No-SMT variants - placeholders
          case CN_BINOP_EXP:
          case CN_BINOP_EXPNOSMT:
            assert(false);
          default:
            assert(false);
        }
      }
    }

    case CN_TERM_ITE: {
      cn_bool* cond_val = (cn_bool*)cn_eval_term_aux(context, term->data.ite.cond);
      assert(cond_val);

      if (convert_from_cn_bool(cond_val)) {
        return cn_eval_term_aux(context, term->data.ite.then_term);
      } else {
        return cn_eval_term_aux(context, term->data.ite.else_term);
      }
    }

    case CN_TERM_CAST: {
      void* value_val = cn_eval_term_aux(context, term->data.cast.value);
      assert(value_val);

      cn_base_type source_type = term->data.cast.value->base_type;
      cn_base_type target_type = term->data.cast.target_type;

      // If source and target types are the same, no cast needed
      if (source_type.tag == target_type.tag) {
        // For CN_BASE_BITS, also check sign and width are equal
        if (source_type.tag == CN_BASE_BITS) {
          if (source_type.data.bits.is_signed == target_type.data.bits.is_signed &&
              source_type.data.bits.size_bits == target_type.data.bits.size_bits) {
            return value_val;
          }
        } else {
          return value_val;
        }
      }

      // Handle pointer casts
      if (cn_base_type_is(target_type, CN_BASE_LOC)) {
        if (cn_base_type_is(source_type, CN_BASE_BITS)) {
          cn_bits_info info = get_bits_info(term->data.cast.value);
          if (!info.is_signed) {
            switch (info.size_bits) {
              case 8:
                return cast_cn_bits_u8_to_cn_pointer((cn_bits_u8*)value_val);
              case 16:
                return cast_cn_bits_u16_to_cn_pointer((cn_bits_u16*)value_val);
              case 32:
                return cast_cn_bits_u32_to_cn_pointer((cn_bits_u32*)value_val);
              case 64:
                return cast_cn_bits_u64_to_cn_pointer((cn_bits_u64*)value_val);
            }
          } else {
            switch (info.size_bits) {
              case 8:
                return cast_cn_bits_i8_to_cn_pointer((cn_bits_i8*)value_val);
              case 16:
                return cast_cn_bits_i16_to_cn_pointer((cn_bits_i16*)value_val);
              case 32:
                return cast_cn_bits_i32_to_cn_pointer((cn_bits_i32*)value_val);
              case 64:
                return cast_cn_bits_i64_to_cn_pointer((cn_bits_i64*)value_val);
            }
          }
        } else if (cn_base_type_is(source_type, CN_BASE_INTEGER)) {
          return cast_cn_integer_to_cn_pointer((cn_integer*)value_val);
        }
      }

      // Handle casts from pointer to numeric types
      if (cn_base_type_is(source_type, CN_BASE_LOC) &&
          cn_base_type_is(target_type, CN_BASE_BITS)) {
        // Extract bits info from target type
        cn_bits_info target_bits = cn_base_type_get_bits_info(target_type);

        // Select appropriate cast function based on signedness and size
        if (target_bits.is_signed) {
          switch (target_bits.size_bits) {
            case 8:
              return cast_cn_pointer_to_cn_bits_i8((cn_pointer*)value_val);
            case 16:
              return cast_cn_pointer_to_cn_bits_i16((cn_pointer*)value_val);
            case 32:
              return cast_cn_pointer_to_cn_bits_i32((cn_pointer*)value_val);
            case 64:
              return cast_cn_pointer_to_cn_bits_i64((cn_pointer*)value_val);
            default:
              assert(false);
              return NULL;
          }
        } else {
          switch (target_bits.size_bits) {
            case 8:
              return cast_cn_pointer_to_cn_bits_u8((cn_pointer*)value_val);
            case 16:
              return cast_cn_pointer_to_cn_bits_u16((cn_pointer*)value_val);
            case 32:
              return cast_cn_pointer_to_cn_bits_u32((cn_pointer*)value_val);
            case 64:
              return cast_cn_pointer_to_cn_bits_u64((cn_pointer*)value_val);
            default:
              assert(false);
              return NULL;
          }
        }
      }
      if (cn_base_type_is(source_type, CN_BASE_LOC) &&
          cn_base_type_is(target_type, CN_BASE_INTEGER)) {
        return cast_cn_pointer_to_cn_integer((cn_pointer*)value_val);
      }

      // Handle bitvector to bitvector casts
      if (cn_base_type_is(source_type, CN_BASE_BITS) &&
          cn_base_type_is(target_type, CN_BASE_BITS)) {
        cn_bits_info source_bits = get_bits_info(term->data.cast.value);
        cn_bits_info target_bits = cn_base_type_get_bits_info(target_type);

        // For signed source types
        if (source_bits.is_signed) {
          switch (source_bits.size_bits) {
            case 8:
              if (target_bits.is_signed) {
                switch (target_bits.size_bits) {
                  case 8:
                    return value_val;  // i8 to i8
                  case 16:
                    return cast_cn_bits_i8_to_cn_bits_i16((cn_bits_i8*)value_val);
                  case 32:
                    return cast_cn_bits_i8_to_cn_bits_i32((cn_bits_i8*)value_val);
                  case 64:
                    return cast_cn_bits_i8_to_cn_bits_i64((cn_bits_i8*)value_val);
                }
              } else {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_i8_to_cn_bits_u8((cn_bits_i8*)value_val);
                  case 16:
                    return cast_cn_bits_i8_to_cn_bits_u16((cn_bits_i8*)value_val);
                  case 32:
                    return cast_cn_bits_i8_to_cn_bits_u32((cn_bits_i8*)value_val);
                  case 64:
                    return cast_cn_bits_i8_to_cn_bits_u64((cn_bits_i8*)value_val);
                }
              }
              break;
            case 16:
              if (target_bits.is_signed) {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_i16_to_cn_bits_i8((cn_bits_i16*)value_val);
                  case 16:
                    return value_val;  // i16 to i16
                  case 32:
                    return cast_cn_bits_i16_to_cn_bits_i32((cn_bits_i16*)value_val);
                  case 64:
                    return cast_cn_bits_i16_to_cn_bits_i64((cn_bits_i16*)value_val);
                }
              } else {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_i16_to_cn_bits_u8((cn_bits_i16*)value_val);
                  case 16:
                    return cast_cn_bits_i16_to_cn_bits_u16((cn_bits_i16*)value_val);
                  case 32:
                    return cast_cn_bits_i16_to_cn_bits_u32((cn_bits_i16*)value_val);
                  case 64:
                    return cast_cn_bits_i16_to_cn_bits_u64((cn_bits_i16*)value_val);
                }
              }
              break;
            case 32:
              if (target_bits.is_signed) {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_i32_to_cn_bits_i8((cn_bits_i32*)value_val);
                  case 16:
                    return cast_cn_bits_i32_to_cn_bits_i16((cn_bits_i32*)value_val);
                  case 32:
                    return value_val;  // i32 to i32
                  case 64:
                    return cast_cn_bits_i32_to_cn_bits_i64((cn_bits_i32*)value_val);
                }
              } else {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_i32_to_cn_bits_u8((cn_bits_i32*)value_val);
                  case 16:
                    return cast_cn_bits_i32_to_cn_bits_u16((cn_bits_i32*)value_val);
                  case 32:
                    return cast_cn_bits_i32_to_cn_bits_u32((cn_bits_i32*)value_val);
                  case 64:
                    return cast_cn_bits_i32_to_cn_bits_u64((cn_bits_i32*)value_val);
                }
              }
              break;
            case 64:
              if (target_bits.is_signed) {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_i64_to_cn_bits_i8((cn_bits_i64*)value_val);
                  case 16:
                    return cast_cn_bits_i64_to_cn_bits_i16((cn_bits_i64*)value_val);
                  case 32:
                    return cast_cn_bits_i64_to_cn_bits_i32((cn_bits_i64*)value_val);
                  case 64:
                    return value_val;  // i64 to i64
                }
              } else {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_i64_to_cn_bits_u8((cn_bits_i64*)value_val);
                  case 16:
                    return cast_cn_bits_i64_to_cn_bits_u16((cn_bits_i64*)value_val);
                  case 32:
                    return cast_cn_bits_i64_to_cn_bits_u32((cn_bits_i64*)value_val);
                  case 64:
                    return cast_cn_bits_i64_to_cn_bits_u64((cn_bits_i64*)value_val);
                }
              }
              break;
          }
        } else {
          // For unsigned source types
          switch (source_bits.size_bits) {
            case 8:
              if (target_bits.is_signed) {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_u8_to_cn_bits_i8((cn_bits_u8*)value_val);
                  case 16:
                    return cast_cn_bits_u8_to_cn_bits_i16((cn_bits_u8*)value_val);
                  case 32:
                    return cast_cn_bits_u8_to_cn_bits_i32((cn_bits_u8*)value_val);
                  case 64:
                    return cast_cn_bits_u8_to_cn_bits_i64((cn_bits_u8*)value_val);
                }
              } else {
                switch (target_bits.size_bits) {
                  case 8:
                    return value_val;  // u8 to u8
                  case 16:
                    return cast_cn_bits_u8_to_cn_bits_u16((cn_bits_u8*)value_val);
                  case 32:
                    return cast_cn_bits_u8_to_cn_bits_u32((cn_bits_u8*)value_val);
                  case 64:
                    return cast_cn_bits_u8_to_cn_bits_u64((cn_bits_u8*)value_val);
                }
              }
              break;
            case 16:
              if (target_bits.is_signed) {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_u16_to_cn_bits_i8((cn_bits_u16*)value_val);
                  case 16:
                    return cast_cn_bits_u16_to_cn_bits_i16((cn_bits_u16*)value_val);
                  case 32:
                    return cast_cn_bits_u16_to_cn_bits_i32((cn_bits_u16*)value_val);
                  case 64:
                    return cast_cn_bits_u16_to_cn_bits_i64((cn_bits_u16*)value_val);
                }
              } else {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_u16_to_cn_bits_u8((cn_bits_u16*)value_val);
                  case 16:
                    return value_val;  // u16 to u16
                  case 32:
                    return cast_cn_bits_u16_to_cn_bits_u32((cn_bits_u16*)value_val);
                  case 64:
                    return cast_cn_bits_u16_to_cn_bits_u64((cn_bits_u16*)value_val);
                }
              }
              break;
            case 32:
              if (target_bits.is_signed) {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_u32_to_cn_bits_i8((cn_bits_u32*)value_val);
                  case 16:
                    return cast_cn_bits_u32_to_cn_bits_i16((cn_bits_u32*)value_val);
                  case 32:
                    return cast_cn_bits_u32_to_cn_bits_i32((cn_bits_u32*)value_val);
                  case 64:
                    return cast_cn_bits_u32_to_cn_bits_i64((cn_bits_u32*)value_val);
                }
              } else {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_u32_to_cn_bits_u8((cn_bits_u32*)value_val);
                  case 16:
                    return cast_cn_bits_u32_to_cn_bits_u16((cn_bits_u32*)value_val);
                  case 32:
                    return value_val;  // u32 to u32
                  case 64:
                    return cast_cn_bits_u32_to_cn_bits_u64((cn_bits_u32*)value_val);
                }
              }
              break;
            case 64:
              if (target_bits.is_signed) {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_u64_to_cn_bits_i8((cn_bits_u64*)value_val);
                  case 16:
                    return cast_cn_bits_u64_to_cn_bits_i16((cn_bits_u64*)value_val);
                  case 32:
                    return cast_cn_bits_u64_to_cn_bits_i32((cn_bits_u64*)value_val);
                  case 64:
                    return cast_cn_bits_u64_to_cn_bits_i64((cn_bits_u64*)value_val);
                }
              } else {
                switch (target_bits.size_bits) {
                  case 8:
                    return cast_cn_bits_u64_to_cn_bits_u8((cn_bits_u64*)value_val);
                  case 16:
                    return cast_cn_bits_u64_to_cn_bits_u16((cn_bits_u64*)value_val);
                  case 32:
                    return cast_cn_bits_u64_to_cn_bits_u32((cn_bits_u64*)value_val);
                  case 64:
                    return value_val;  // u64 to u64
                }
              }
              break;
          }
        }

        assert(false);
        return NULL;
      }

      // Handle bitvector to integer casts
      if (cn_base_type_is(source_type, CN_BASE_BITS) &&
          cn_base_type_is(target_type, CN_BASE_INTEGER)) {
        cn_bits_info info = get_bits_info(term->data.cast.value);
        if (info.is_signed) {
          switch (info.size_bits) {
            case 8:
              return cast_cn_bits_i8_to_cn_integer((cn_bits_i8*)value_val);
            case 16:
              return cast_cn_bits_i16_to_cn_integer((cn_bits_i16*)value_val);
            case 32:
              return cast_cn_bits_i32_to_cn_integer((cn_bits_i32*)value_val);
            case 64:
              return cast_cn_bits_i64_to_cn_integer((cn_bits_i64*)value_val);
          }
        } else {
          switch (info.size_bits) {
            case 8:
              return cast_cn_bits_u8_to_cn_integer((cn_bits_u8*)value_val);
            case 16:
              return cast_cn_bits_u16_to_cn_integer((cn_bits_u16*)value_val);
            case 32:
              return cast_cn_bits_u32_to_cn_integer((cn_bits_u32*)value_val);
            case 64:
              return cast_cn_bits_u64_to_cn_integer((cn_bits_u64*)value_val);
          }
        }
      }

      // Handle integer to bitvector casts
      if (cn_base_type_is(source_type, CN_BASE_INTEGER) &&
          cn_base_type_is(target_type, CN_BASE_BITS)) {
        // Extract bits info from target type
        cn_bits_info target_bits = cn_base_type_get_bits_info(target_type);
        cn_integer* int_val = (cn_integer*)value_val;

        // Select appropriate conversion function based on signedness and size
        if (target_bits.is_signed) {
          switch (target_bits.size_bits) {
            case 8:
              return cast_cn_integer_to_cn_bits_i8(int_val);
            case 16:
              return cast_cn_integer_to_cn_bits_i16(int_val);
            case 32:
              return cast_cn_integer_to_cn_bits_i32(int_val);
            case 64:
              return cast_cn_integer_to_cn_bits_i64(int_val);
            default:
              assert(false);
              return NULL;
          }
        } else {
          switch (target_bits.size_bits) {
            case 8:
              return cast_cn_integer_to_cn_bits_u8(int_val);
            case 16:
              return cast_cn_integer_to_cn_bits_u16(int_val);
            case 32:
              return cast_cn_integer_to_cn_bits_u32(int_val);
            case 64:
              return cast_cn_integer_to_cn_bits_u64(int_val);
            default:
              assert(false);
              return NULL;
          }
        }
      }

      assert(false);
    }

    case CN_TERM_MEMBER_SHIFT: {
      void* base_val = cn_eval_term_aux(context, term->data.member_shift.base);
      assert(base_val);

      cn_pointer* base_ptr = (cn_pointer*)base_val;
      size_t offset = term->data.member_shift.offset;

      // Calculate new pointer: base + offset
      void* new_ptr = (char*)base_ptr->ptr + offset;
      return convert_to_cn_pointer(new_ptr);
    }

    case CN_TERM_ARRAY_SHIFT: {
      void* base_val = cn_eval_term_aux(context, term->data.array_shift.base);
      void* index_val = cn_eval_term_aux(context, term->data.array_shift.index);
      assert(base_val && index_val);

      cn_pointer* base_ptr = (cn_pointer*)base_val;
      size_t element_size = term->data.array_shift.element_size;

      switch (term->data.array_shift.index->base_type.tag) {
        case CN_BASE_INTEGER: {
          cn_integer* index = (cn_integer*)index_val;
          return cn_array_shift(base_ptr, element_size, index);
        }
        case CN_BASE_BITS: {
          // Need to check the specific bitvector type and cast to cn_integer
          cn_bits_info info = get_bits_info(term->data.array_shift.index);
          if (info.is_signed) {
            switch (info.size_bits) {
              case 8: {
                cn_bits_i8* index = (cn_bits_i8*)index_val;
                return cn_array_shift(base_ptr, element_size, index);
              }
              case 16: {
                cn_bits_i16* index = (cn_bits_i16*)index_val;
                return cn_array_shift(base_ptr, element_size, index);
              }
              case 32: {
                cn_bits_i32* index = (cn_bits_i32*)index_val;
                return cn_array_shift(base_ptr, element_size, index);
              }
              case 64: {
                cn_bits_i64* index = (cn_bits_i64*)index_val;
                return cn_array_shift(base_ptr, element_size, index);
              }
              default:
                assert(false);
            }
          } else {
            switch (info.size_bits) {
              case 8: {
                cn_bits_u8* index = (cn_bits_u8*)index_val;
                return cn_array_shift(base_ptr, element_size, index);
              }
              case 16: {
                cn_bits_u16* index = (cn_bits_u16*)index_val;
                return cn_array_shift(base_ptr, element_size, index);
              }
              case 32: {
                cn_bits_u32* index = (cn_bits_u32*)index_val;
                return cn_array_shift(base_ptr, element_size, index);
              }
              case 64: {
                cn_bits_u64* index = (cn_bits_u64*)index_val;
                return cn_array_shift(base_ptr, element_size, index);
              }
              default:
                assert(false);
            }
          }
          break;
        }
        default:
          assert(false);
      }

      // Unreachable
      assert(false);
    }

    case CN_TERM_MAP_SET: {
      void* map_val = cn_eval_term_aux(context, term->data.map_set.map);
      void* key_val = cn_eval_term_aux(context, term->data.map_set.key);
      void* value_val = cn_eval_term_aux(context, term->data.map_set.value);
      assert(map_val && key_val && value_val);

      cn_map* map = (cn_map*)map_val;
      cn_integer* key = (cn_integer*)key_val;

      // Create a new map with the updated value
      cn_map* new_map = cn_map_deep_copy(map);
      return cn_map_set(new_map, key, value_val);
    }

    case CN_TERM_MAP_GET: {
      void* map_val = cn_eval_term_aux(context, term->data.map_get.map);
      void* key_val = cn_eval_term_aux(context, term->data.map_get.key);
      assert(map_val && key_val);

      // Use appropriate map get function based on result type
      // For now, assume we're getting a generic pointer
      assert(false);
    }

    case CN_TERM_WRAPI: {
      assert(false);
    }

    case CN_TERM_LET: {
      // Let binding - evaluate value and extend context for body

      // Evaluate the right-hand side with current context
      void* value_result = cn_eval_term_aux(context, term->data.let.value);
      assert(value_result);

      // Push new scope and add the binding
      push_eval_context(context);
      set_eval_binding(context, term->data.let.var, value_result);

      // Evaluate the body with extended context
      void* body_result = cn_eval_term_aux(context, term->data.let.body);

      // Pop scope
      pop_eval_context(context);

      return body_result;
    }

    case CN_TERM_STRUCT: {
      // Create struct from members using registered handler
      const char* struct_tag = term->data.struct_val.tag;

      // Convert cn_term_ptr hash table to void_ptr hash table for member values
      bennet_hash_table(const_char_ptr, void_ptr) member_values;
      bennet_hash_table_init(const_char_ptr, void_ptr)(
          &member_values, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

      // Evaluate all member terms and store in the void_ptr hash table
      bennet_vector(cn_member_pair)* members = &term->data.struct_val.members;
      for (size_t i = 0; i < bennet_vector_size(cn_member_pair)(members); i++) {
        cn_member_pair* pair = bennet_vector_get(cn_member_pair)(members, i);
        const char* member_name = pair->name;
        cn_term* member_term = pair->value;
        void* member_val = cn_eval_term_aux(context, member_term);
        assert(member_val);
        bennet_hash_table_set(const_char_ptr, void_ptr)(
            &member_values, member_name, member_val);
      }

      // Call the registered struct creation function
      void* result = cn_smt_struct_create(struct_tag, &member_values);

      // Cleanup temporary hash table
      bennet_hash_table_free(const_char_ptr, void_ptr)(&member_values);

      return result;
    }

    case CN_TERM_STRUCT_MEMBER: {
      void* struct_val = cn_eval_term_aux(context, term->data.struct_member.struct_term);
      assert(struct_val);

      // Extract struct member using registered handler
      // We need to get the struct type name from the base type
      cn_term* struct_term = term->data.struct_member.struct_term;
      assert(cn_base_type_is(struct_term->base_type, CN_BASE_STRUCT));

      const char* struct_tag = struct_term->base_type.data.struct_tag.tag;
      const char* member_name = term->data.struct_member.member_name;
      return cn_smt_struct_get_member(struct_tag, struct_val, member_name);
    }

    case CN_TERM_STRUCT_UPDATE: {
      void* struct_val = cn_eval_term_aux(context, term->data.struct_update.struct_term);
      void* new_val = cn_eval_term_aux(context, term->data.struct_update.new_value);
      assert(struct_val && new_val);

      // Update struct member using registered handler
      // We need to get the struct type name from the base type
      cn_term* struct_term = term->data.struct_update.struct_term;
      assert(cn_base_type_is(struct_term->base_type, CN_BASE_STRUCT));

      const char* struct_tag = struct_term->base_type.data.struct_tag.tag;
      const char* member_name = term->data.struct_update.member_name;
      return cn_smt_struct_update_member(struct_tag, struct_val, member_name, new_val);
    }

    case CN_TERM_RECORD: {
      // Create record from member values using registered handler

      // First, extract member names and types to compute record hash
      bennet_vector(cn_member_pair)* members = &term->data.record.members;
      size_t member_count = bennet_vector_size(cn_member_pair)(members);

      if (member_count == 0) {
        // Empty record - return a sentinel value
        return convert_to_cn_pointer(NULL);
      }

      // Collect member names and types for hash computation
      const char** member_names = cn_test_malloc(member_count * sizeof(const char*));
      cn_base_type* member_types = cn_test_malloc(member_count * sizeof(cn_base_type));

      for (size_t i = 0; i < member_count; i++) {
        cn_member_pair* pair = bennet_vector_get(cn_member_pair)(members, i);
        member_names[i] = pair->name;
        member_types[i] = pair->value->base_type;
      }

      // Compute record hash from member composition
      record_hash_t record_hash =
          cn_record_member_hash(member_count, member_names, member_types);

      // Convert cn_term_ptr hash table to void_ptr hash table for member values
      bennet_hash_table(const_char_ptr, void_ptr) member_values;
      bennet_hash_table_init(const_char_ptr, void_ptr)(
          &member_values, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

      // Evaluate all member terms and store in the void_ptr hash table
      for (size_t i = 0; i < member_count; i++) {
        cn_member_pair* pair = bennet_vector_get(cn_member_pair)(members, i);
        const char* member_name = pair->name;
        cn_term* member_term = pair->value;
        void* member_val = cn_eval_term_aux(context, member_term);
        assert(member_val);
        bennet_hash_table_set(const_char_ptr, void_ptr)(
            &member_values, member_name, member_val);
      }

      // Call the registered record creation function
      void* result = cn_smt_record_create(record_hash, &member_values);

      // Cleanup temporary memory and hash table
      cn_test_free(member_names);
      cn_test_free(member_types);
      bennet_hash_table_free(const_char_ptr, void_ptr)(&member_values);

      return result;
    }

    case CN_TERM_RECORD_MEMBER: {
      void* record_val = cn_eval_term_aux(context, term->data.record_member.record_term);
      assert(record_val);

      // Extract record member using registered handler
      // We need to get the record type from the base type of the record term
      cn_term* record_term = term->data.record_member.record_term;
      assert(cn_base_type_is(record_term->base_type, CN_BASE_RECORD));

      // Compute record hash from the record term's type information
      cn_base_type record_type = record_term->base_type;
      size_t member_count = record_type.data.record.count;
      const char** member_names = record_type.data.record.names;
      cn_base_type* member_types = record_type.data.record.types;

      record_hash_t record_hash =
          cn_record_member_hash(member_count, member_names, member_types);

      const char* member_name = term->data.record_member.member_name;
      return cn_smt_record_get_member(record_hash, record_val, member_name);
    }

    case CN_TERM_RECORD_UPDATE: {
      void* record_val = cn_eval_term_aux(context, term->data.record_update.record_term);
      void* new_val = cn_eval_term_aux(context, term->data.record_update.new_value);
      assert(record_val && new_val);

      // Update record member using registered handler
      // We need to get the record type from the base type of the record term
      cn_term* record_term = term->data.record_update.record_term;
      assert(cn_base_type_is(record_term->base_type, CN_BASE_RECORD));

      // Compute record hash from the record term's type information
      cn_base_type record_type = record_term->base_type;
      size_t member_count = record_type.data.record.count;
      const char** member_names = record_type.data.record.names;
      cn_base_type* member_types = record_type.data.record.types;

      record_hash_t record_hash =
          cn_record_member_hash(member_count, member_names, member_types);

      const char* member_name = term->data.record_update.member_name;
      return cn_smt_record_update_member(record_hash, record_val, member_name, new_val);
    }

    case CN_TERM_CONSTRUCTOR: {
      // Apply data constructor
      // Extract datatype name from base type
      assert(term->base_type.tag == CN_BASE_DATATYPE);
      const char* datatype_name = term->base_type.data.datatype_tag.tag;
      assert(datatype_name);

      // Extract constructor name
      const char* constructor_name = term->data.constructor.constructor_name;
      assert(constructor_name);

      // Create vector for evaluated arguments
      bennet_vector(void_ptr) args_vec;
      bennet_vector_init(void_ptr)(&args_vec);

      // Evaluate each argument and add to vector
      bennet_vector(cn_member_pair)* args = &term->data.constructor.args;
      for (size_t i = 0; i < bennet_vector_size(cn_member_pair)(args); i++) {
        cn_member_pair* pair = bennet_vector_get(cn_member_pair)(args, i);
        void* evaluated_arg = cn_eval_term_aux(context, pair->value);
        bennet_vector_push(void_ptr)(&args_vec, evaluated_arg);
      }

      // Call the registered constructor function
      void* result =
          cn_smt_datatype_apply_constructor(datatype_name, constructor_name, &args_vec);

      // Clean up vector (but not the contents, as they're owned by the result)
      bennet_vector_free(void_ptr)(&args_vec);

      return result;
    }

    case CN_TERM_APPLY: {
      // Function application - evaluate arguments and apply function
      const char* base_function_name = term->data.apply.function_name;
      assert(base_function_name);

      // Construct full function name with "_func" suffix (matching fn_def_name)
      char buffer[256];
      snprintf(buffer, sizeof(buffer), "%s_func", base_function_name);
      const char* function_name = cn_intern_string(buffer);

      // Get argument count from the term
      bennet_vector(cn_term_ptr)* args_vec = &term->data.apply.args;
      size_t arg_count = bennet_vector_size(cn_term_ptr)(args_vec);

      // Allocate array for evaluated arguments
      void** evaluated_args = NULL;
      if (arg_count > 0) {
        evaluated_args = cn_test_malloc(arg_count * sizeof(void*));
        assert(evaluated_args);
      }

      // Evaluate arguments left-to-right
      for (size_t i = 0; i < arg_count; i++) {
        cn_term** arg_ptr = bennet_vector_get(cn_term_ptr)(args_vec, i);
        assert(arg_ptr && *arg_ptr);
        evaluated_args[i] = cn_eval_term_aux(context, *arg_ptr);
        assert(evaluated_args[i]);
      }

      // Lookup and call the function handler
      cn_func_handler handler = cn_get_func_handler(function_name);
      assert(handler);
      void* result = handler(evaluated_args);

      // Cleanup and return
      cn_test_free(evaluated_args);
      return result;
    }

    case CN_TERM_EACHI: {
      // Universal quantification - for SMT solving, not direct evaluation
      assert(false);
    }

    case CN_TERM_MATCH: {
      // Evaluate the scrutinee (value being matched)
      void* scrutinee_val = cn_eval_term_aux(context, term->data.match_data.scrutinee);
      assert(scrutinee_val);

      // Get datatype name from scrutinee base type
      assert(term->data.match_data.scrutinee->base_type.tag == CN_BASE_DATATYPE);
      const char* dt_name =
          term->data.match_data.scrutinee->base_type.data.datatype_tag.tag;

      // Get destructor function for this datatype
      cn_datatype_destructor_fn destructor = cn_get_datatype_destructor(dt_name);
      assert(destructor);

      // Try each pattern case
      bennet_vector(cn_match_case)* cases = &term->data.match_data.cases;
      for (size_t i = 0; i < bennet_vector_size(cn_match_case)(cases); i++) {
        cn_match_case* match_case = bennet_vector_get(cn_match_case)(cases, i);

        // Call destructor to check if constructor matches and get members
        void** members = destructor(match_case->constructor_tag, scrutinee_val);

        if (members == NULL) {
          // Constructor doesn't match, try next pattern
          continue;
        }

        // Constructor matches! Push new scope and bind pattern variables
        push_eval_context(context);

        // Bind non-wildcard pattern variables to member values
        for (size_t j = 0; j < match_case->pattern_var_count; j++) {
          // Check if this is not a wildcard (name is not NULL)
          if (match_case->pattern_vars[j].name != NULL) {
            set_eval_binding(context, match_case->pattern_vars[j], members[j]);
          }
          // Wildcards are skipped - they don't bind
        }

        // Evaluate the body with extended context
        void* result = cn_eval_term_aux(context, match_case->body_term);

        // Cleanup
        cn_test_free(members);
        pop_eval_context(context);

        return result;
      }

      // No pattern matched - this should not happen in well-typed programs
      assert(false);
    }

    default:
      assert(false);
  }
}

void* cn_eval_term(cn_term* term) {
  // Create an empty context stack for evaluation
  cn_eval_context_stack* context = create_eval_context_stack();

  // Evaluate the term with the empty context
  void* result = cn_eval_term_aux(context, term);

  // Cleanup the context
  free_eval_context_stack(context);

  return result;
}

static cn_bits_info get_bits_info(cn_term* term) {
  assert(term->base_type.tag == CN_BASE_BITS);
  return term->base_type.data.bits;
}
