#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/utils.h>
#include <cn-smt/eval.h>
// Forward declarations
static cn_bits_info get_bits_info(cn_term* term);

// Helper function to cast void* to specific cn_* types
#define EVAL(term) cn_eval_term(term)

void* cn_eval_term(cn_term* term) {
  if (!term) {
    return NULL;
  }

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
            default:
              assert(false);
          }
        }
        default:
          assert(false);
      }
    }

    case CN_TERM_UNOP: {
      void* operand_val = EVAL(term->data.unop.operand);
      if (!operand_val)
        return NULL;

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
      void* left_val = EVAL(term->data.binop.left);
      void* right_val = EVAL(term->data.binop.right);
      if (!left_val || !right_val)
        return NULL;

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
                  return cn_bits_i8_multiply(left, right);
                case CN_BINOP_DIV:
                  return cn_bits_i8_divide(left, right);
                case CN_BINOP_REM:
                  return cn_bits_i8_rem(left, right);
                case CN_BINOP_MOD:
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
                  return cn_bits_i16_multiply(left, right);
                case CN_BINOP_DIV:
                  return cn_bits_i16_divide(left, right);
                case CN_BINOP_REM:
                  return cn_bits_i16_rem(left, right);
                case CN_BINOP_MOD:
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
                  return cn_bits_i32_multiply(left, right);
                case CN_BINOP_DIV:
                  return cn_bits_i32_divide(left, right);
                case CN_BINOP_REM:
                  return cn_bits_i32_rem(left, right);
                case CN_BINOP_MOD:
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
                  return cn_bits_i64_multiply(left, right);
                case CN_BINOP_DIV:
                  return cn_bits_i64_divide(left, right);
                case CN_BINOP_REM:
                  return cn_bits_i64_rem(left, right);
                case CN_BINOP_MOD:
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
                  return cn_bits_u8_multiply(left, right);
                case CN_BINOP_DIV:
                  return cn_bits_u8_divide(left, right);
                case CN_BINOP_REM:
                  return cn_bits_u8_rem(left, right);
                case CN_BINOP_MOD:
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
                  return cn_bits_u16_multiply(left, right);
                case CN_BINOP_DIV:
                  return cn_bits_u16_divide(left, right);
                case CN_BINOP_REM:
                  return cn_bits_u16_rem(left, right);
                case CN_BINOP_MOD:
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
                  return cn_bits_u32_multiply(left, right);
                case CN_BINOP_DIV:
                  return cn_bits_u32_divide(left, right);
                case CN_BINOP_REM:
                  return cn_bits_u32_rem(left, right);
                case CN_BINOP_MOD:
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
                  return cn_bits_u64_multiply(left, right);
                case CN_BINOP_DIV:
                  return cn_bits_u64_divide(left, right);
                case CN_BINOP_REM:
                  return cn_bits_u64_rem(left, right);
                case CN_BINOP_MOD:
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
            return cn_integer_multiply((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_DIV:
            return cn_integer_divide((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_REM:
            return cn_integer_rem((cn_integer*)left_val, (cn_integer*)right_val);
          case CN_BINOP_MOD:
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
          case CN_BINOP_MULNOSMT:
          case CN_BINOP_DIVNOSMT:
          case CN_BINOP_REMNOSMT:
          case CN_BINOP_MODNOSMT:
          case CN_BINOP_EXP:
          case CN_BINOP_EXPNOSMT:
            assert(false);
          default:
            assert(false);
        }
      }
    }

    case CN_TERM_ITE: {
      cn_bool* cond_val = (cn_bool*)EVAL(term->data.ite.cond);
      if (!cond_val)
        return NULL;

      if (convert_from_cn_bool(cond_val)) {
        return EVAL(term->data.ite.then_term);
      } else {
        return EVAL(term->data.ite.else_term);
      }
    }

    case CN_TERM_CAST: {
      void* value_val = EVAL(term->data.cast.value);
      if (!value_val)
        return NULL;

      cn_base_type source_type = term->data.cast.value->base_type;
      cn_base_type target_type = term->data.cast.target_type;

      // If source and target types are the same, no cast needed
      if (source_type.tag == target_type.tag) {
        return value_val;
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
              // Fallback to 64-bit signed for unsupported sizes
              return cast_cn_pointer_to_cn_bits_i64((cn_pointer*)value_val);
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
              // Fallback to 64-bit unsigned for unsupported sizes
              return cast_cn_pointer_to_cn_bits_u64((cn_pointer*)value_val);
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
        // For bitvector-to-bitvector casts, we'll implement same-size casts
        // and return the value unchanged (reinterpret cast)
        return value_val;  // For now, just return the same value (no size/sign conversion)
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
              return convert_to_cn_bits_i8(int_val->val);
            case 16:
              return convert_to_cn_bits_i16(int_val->val);
            case 32:
              return convert_to_cn_bits_i32(int_val->val);
            case 64:
              return convert_to_cn_bits_i64(int_val->val);
            default:
              // Fallback to 64-bit signed for unsupported sizes
              return convert_to_cn_bits_i64(int_val->val);
          }
        } else {
          switch (target_bits.size_bits) {
            case 8:
              return convert_to_cn_bits_u8(int_val->val);
            case 16:
              return convert_to_cn_bits_u16(int_val->val);
            case 32:
              return convert_to_cn_bits_u32(int_val->val);
            case 64:
              return convert_to_cn_bits_u64(int_val->val);
            default:
              // Fallback to 64-bit unsigned for unsupported sizes
              return convert_to_cn_bits_u64(int_val->val);
          }
        }
      }

      assert(false);
    }

    case CN_TERM_MEMBER_SHIFT: {
      void* base_val = EVAL(term->data.member_shift.base);
      if (!base_val)
        return NULL;

      cn_pointer* base_ptr = (cn_pointer*)base_val;
      size_t offset = term->data.member_shift.offset;

      // Calculate new pointer: base + offset
      void* new_ptr = (char*)base_ptr->ptr + offset;
      return convert_to_cn_pointer(new_ptr);
    }

    case CN_TERM_ARRAY_SHIFT: {
      void* base_val = EVAL(term->data.array_shift.base);
      void* index_val = EVAL(term->data.array_shift.index);
      if (!base_val || !index_val)
        return NULL;

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
                return NULL;
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
                return NULL;
            }
          }
          break;
        }
        default:
          return NULL;
      }

      // Unreachable
      assert(false);
      return NULL;
    }

    case CN_TERM_MAP_SET: {
      void* map_val = EVAL(term->data.map_set.map);
      void* key_val = EVAL(term->data.map_set.key);
      void* value_val = EVAL(term->data.map_set.value);
      if (!map_val || !key_val || !value_val)
        return NULL;

      cn_map* map = (cn_map*)map_val;
      cn_integer* key = (cn_integer*)key_val;

      // Create a new map with the updated value
      cn_map* new_map = cn_map_deep_copy(map);
      return cn_map_set(new_map, key, value_val);
    }

    case CN_TERM_MAP_GET: {
      void* map_val = EVAL(term->data.map_get.map);
      void* key_val = EVAL(term->data.map_get.key);
      if (!map_val || !key_val)
        return NULL;

      // Use appropriate map get function based on result type
      // For now, assume we're getting a generic pointer
      assert(false);
    }

    case CN_TERM_WRAPI: {
      void* value_val = EVAL(term->data.wrapi.value);
      if (!value_val)
        return NULL;

      // Wrap integer to given integer type
      // For now, just return the value unchanged
      return value_val;
    }

    case CN_TERM_SYM: {
      // Symbol evaluation - would need symbol table
      // For now, just document that we have name and unique ID available
      // printf("Evaluating symbol: %s (ID: %" PRIu64 ")\n", term->data.sym.name, term->data.sym.id);
      assert(false);
    }

    case CN_TERM_LET: {
      // Let binding - would need variable substitution or environment
      assert(false);
    }

    case CN_TERM_STRUCT: {
      // Create struct from members
      assert(false);
    }

    case CN_TERM_STRUCT_MEMBER: {
      void* struct_val = EVAL(term->data.struct_member.struct_term);
      if (!struct_val)
        return NULL;

      // Extract struct member
      assert(false);
    }

    case CN_TERM_STRUCT_UPDATE: {
      void* struct_val = EVAL(term->data.struct_update.struct_term);
      void* new_val = EVAL(term->data.struct_update.new_value);
      if (!struct_val || !new_val)
        return NULL;

      // Update struct member
      assert(false);
    }

    case CN_TERM_RECORD: {
      // Create record from members
      assert(false);
    }

    case CN_TERM_RECORD_MEMBER: {
      void* record_val = EVAL(term->data.record_member.record_term);
      if (!record_val)
        return NULL;

      // Extract record member
      assert(false);
    }

    case CN_TERM_CONSTRUCTOR: {
      // Apply data constructor
      assert(false);
    }

    case CN_TERM_APPLY: {
      // Function application - evaluate arguments and apply function
      assert(false);
    }

    case CN_TERM_EACHI: {
      // Universal quantification - for SMT solving, not direct evaluation
      assert(false);
    }

    case CN_TERM_MATCH: {
      // Pattern matching - complex operation
      assert(false);
    }

    default:
      assert(false);
  }
}

static cn_bits_info get_bits_info(cn_term* term) {
  switch (term->type) {
    case CN_TERM_CONST:
      if (term->data.const_val.type == CN_CONST_BITS) {
        return term->data.const_val.data.bits.info;
      }
      break;
    case CN_TERM_BINOP:
      return get_bits_info(term->data.binop.left);
    case CN_TERM_UNOP:
      return get_bits_info(term->data.unop.operand);
    default:
      break;
  }
  // Return a default/error value
  cn_bits_info info = {true, -1};
  return info;
}

bool cn_eval_context(cn_constraint_context* ctx) {
  if (!ctx) {
    return false;
  }

  // Evaluate all logical constraints
  const cn_logical_constraint* logical = cn_context_first_logical(ctx);
  while (logical) {
    switch (logical->type) {
      case CN_LOGICAL_TERM: {
        // Evaluate the term and check if it's true
        void* result = cn_eval_term(logical->data.term);
        if (!result) {
          return false;  // Failed to evaluate term
        }

        // For logical constraints, the term should evaluate to a boolean
        if (cn_base_type_is(logical->data.term->base_type, CN_BASE_BOOL)) {
          cn_bool* bool_result = (cn_bool*)result;
          if (!bool_result->val) {
            return false;  // Constraint is false
          }
        } else {
          // Non-boolean logical constraints are treated as "truthy"
          // This is a simplification; in practice might need more sophisticated handling
        }
        break;
      }

      case CN_LOGICAL_FORALL: {
        // For now, we can't evaluate quantified constraints without a full solver
        // This is a limitation of the current implementation
        // In practice, forall constraints would require instantiation or theorem proving
        assert(false);
        break;
      }
    }
    logical = logical->next;
  }

  // Check resource constraints for consistency
  const cn_resource_constraint* resource = cn_context_first_resource(ctx);
  while (resource) {
    // Evaluate the pointer term
    void* pointer_result = cn_eval_term(resource->pointer);
    if (!pointer_result) {
      return false;  // Failed to evaluate pointer
    }

    // Basic consistency check: pointer should not be null for non-zero bytes
    if (resource->bytes > 0) {
      cn_pointer* ptr = (cn_pointer*)pointer_result;
      if (ptr->ptr == NULL) {
        return false;  // Can't own bytes at null pointer
      }
    }

    resource = resource->next;
  }

  // If all constraints are satisfied, return true
  return true;
}
