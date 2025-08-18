#include <stdlib.h>
#include <string.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/vector.h>
#include <cn-smt/subst.h>
#include <cn-smt/terms.h>

// Generate hash table implementation for uint64_t -> cn_term_ptr
BENNET_HASH_TABLE_IMPL(uint64_t, cn_term_ptr)

// Generate vector implementation for cn_term_ptr
BENNET_VECTOR_IMPL(cn_term_ptr)

// Generate hash table implementation for const_char_ptr -> cn_term_ptr
BENNET_HASH_TABLE_IMPL(const_char_ptr, cn_term_ptr)

// Helper function to deep copy a term
static cn_term* deep_copy_term(cn_term* term) {
  if (!term) {
    return NULL;
  }

  cn_term* copy = malloc(sizeof(cn_term));
  if (!copy) {
    return NULL;
  }

  // Copy non-pointer data
  *copy = *term;

  switch (term->type) {
    case CN_TERM_CONST:
      // For string constants, we need to duplicate the string
      if (term->data.const_val.type == CN_CONST_CTYPE_CONST &&
          term->data.const_val.data.ctype_name) {
        size_t len = strlen(term->data.const_val.data.ctype_name);
        char* dup = malloc(len + 1);
        if (!dup) {
          free(copy);
          return NULL;
        }
        strcpy(dup, term->data.const_val.data.ctype_name);
        copy->data.const_val.data.ctype_name = dup;
      }
      break;

    case CN_TERM_SYM:
      // No pointers to copy
      break;

    case CN_TERM_UNOP:
      copy->data.unop.operand = deep_copy_term(term->data.unop.operand);
      break;

    case CN_TERM_BINOP:
      copy->data.binop.left = deep_copy_term(term->data.binop.left);
      copy->data.binop.right = deep_copy_term(term->data.binop.right);
      break;

    case CN_TERM_ITE:
      copy->data.ite.cond = deep_copy_term(term->data.ite.cond);
      copy->data.ite.then_term = deep_copy_term(term->data.ite.then_term);
      copy->data.ite.else_term = deep_copy_term(term->data.ite.else_term);
      break;

    case CN_TERM_EACHI:
      copy->data.eachi.body = deep_copy_term(term->data.eachi.body);
      break;

    case CN_TERM_STRUCT:
      // Deep copy the members hash table
      bennet_hash_table_init(const_char_ptr, cn_term_ptr)(&copy->data.struct_val.members,
          bennet_hash_const_char_ptr,
          bennet_eq_const_char_ptr);
      // Deep copy all entries in the hash table
      for (size_t i = 0; i < term->data.struct_val.members.capacity; ++i) {
        if (term->data.struct_val.members.entries[i].occupied) {
          const char* key = term->data.struct_val.members.entries[i].key;
          cn_term* value = term->data.struct_val.members.entries[i].value;
          cn_term* copied_value = deep_copy_term(value);
          if (copied_value) {
            bennet_hash_table_set(const_char_ptr, cn_term_ptr)(
                &copy->data.struct_val.members, key, copied_value);
          }
        }
      }
      break;

    case CN_TERM_STRUCT_MEMBER:
      copy->data.struct_member.struct_term =
          deep_copy_term(term->data.struct_member.struct_term);
      break;

    case CN_TERM_STRUCT_UPDATE:
      copy->data.struct_update.struct_term =
          deep_copy_term(term->data.struct_update.struct_term);
      copy->data.struct_update.new_value =
          deep_copy_term(term->data.struct_update.new_value);
      break;

    case CN_TERM_RECORD:
      // Deep copy the members hash table
      bennet_hash_table_init(const_char_ptr, cn_term_ptr)(&copy->data.record.members,
          bennet_hash_const_char_ptr,
          bennet_eq_const_char_ptr);
      // Deep copy all entries in the hash table
      for (size_t i = 0; i < term->data.record.members.capacity; ++i) {
        if (term->data.record.members.entries[i].occupied) {
          const char* key = term->data.record.members.entries[i].key;
          cn_term* value = term->data.record.members.entries[i].value;
          cn_term* copied_value = deep_copy_term(value);
          if (copied_value) {
            bennet_hash_table_set(const_char_ptr, cn_term_ptr)(
                &copy->data.record.members, key, copied_value);
          }
        }
      }
      break;

    case CN_TERM_RECORD_MEMBER:
      copy->data.record_member.record_term =
          deep_copy_term(term->data.record_member.record_term);
      break;

    case CN_TERM_CONSTRUCTOR:
      // Deep copy the args hash table
      bennet_hash_table_init(const_char_ptr, cn_term_ptr)(&copy->data.constructor.args,
          bennet_hash_const_char_ptr,
          bennet_eq_const_char_ptr);
      // Deep copy all entries in the hash table
      for (size_t i = 0; i < term->data.constructor.args.capacity; ++i) {
        if (term->data.constructor.args.entries[i].occupied) {
          const char* key = term->data.constructor.args.entries[i].key;
          cn_term* value = term->data.constructor.args.entries[i].value;
          cn_term* copied_value = deep_copy_term(value);
          if (copied_value) {
            bennet_hash_table_set(const_char_ptr, cn_term_ptr)(
                &copy->data.constructor.args, key, copied_value);
          }
        }
      }
      break;

    case CN_TERM_MEMBER_SHIFT:
      copy->data.member_shift.base = deep_copy_term(term->data.member_shift.base);
      break;

    case CN_TERM_ARRAY_SHIFT:
      copy->data.array_shift.base = deep_copy_term(term->data.array_shift.base);
      copy->data.array_shift.index = deep_copy_term(term->data.array_shift.index);
      break;

    case CN_TERM_WRAPI:
      copy->data.wrapi.value = deep_copy_term(term->data.wrapi.value);
      break;

    case CN_TERM_MAP_SET:
      copy->data.map_set.map = deep_copy_term(term->data.map_set.map);
      copy->data.map_set.key = deep_copy_term(term->data.map_set.key);
      copy->data.map_set.value = deep_copy_term(term->data.map_set.value);
      break;

    case CN_TERM_MAP_GET:
      copy->data.map_get.map = deep_copy_term(term->data.map_get.map);
      copy->data.map_get.key = deep_copy_term(term->data.map_get.key);
      break;

    case CN_TERM_APPLY:
      copy->data.apply.function_name =
          term->data.apply.function_name;  // String is not copied
      // Deep copy the args vector
      bennet_vector_init(cn_term_ptr)(&copy->data.apply.args);
      // Deep copy all entries in the vector
      for (size_t i = 0; i < term->data.apply.args.size; ++i) {
        cn_term* arg = term->data.apply.args.data[i];
        cn_term* copied_arg = deep_copy_term(arg);
        if (copied_arg) {
          bennet_vector_push(cn_term_ptr)(&copy->data.apply.args, copied_arg);
        }
      }
      break;

    case CN_TERM_LET:
      copy->data.let.value = deep_copy_term(term->data.let.value);
      copy->data.let.body = deep_copy_term(term->data.let.body);
      break;

    case CN_TERM_CAST:
      copy->data.cast.value = deep_copy_term(term->data.cast.value);
      break;

    default:
      // Assuming no other term types have pointers that need deep copying
      break;
  }

  return copy;
}

// Forward declaration for recursive substitution
static cn_term* cn_subst_term_impl(
    cn_term* term, bennet_hash_table(uint64_t, cn_term_ptr) * subst_table);

// Substitute symbols in a term recursively
cn_term* cn_subst_term(
    cn_term* term, bennet_hash_table(uint64_t, cn_term_ptr) * subst_table) {
  if (!term || !subst_table) {
    return term;
  }

  return cn_subst_term_impl(term, subst_table);
}

static cn_term* cn_subst_term_impl(
    cn_term* term, bennet_hash_table(uint64_t, cn_term_ptr) * subst_table) {
  if (!term) {
    return NULL;
  }

  switch (term->type) {
    case CN_TERM_CONST:
      // Constants don't contain symbols, return as-is
      return term;

    case CN_TERM_SYM: {
      // Look up symbol ID in substitution table
      bennet_optional(cn_term_ptr) result =
          bennet_hash_table_get(uint64_t, cn_term_ptr)(subst_table, term->data.sym.id);
      if (bennet_optional_is_some(result)) {
        // Return a copy of the term from the substitution table
        cn_term* subst_term = bennet_optional_unwrap(result);
        return deep_copy_term(subst_term);
      }
      // Symbol not found in substitution table, return original
      return term;
    }

    case CN_TERM_UNOP: {
      // Recursively substitute in operand
      cn_term* new_operand = cn_subst_term_impl(term->data.unop.operand, subst_table);
      if (new_operand == term->data.unop.operand) {
        // No substitution occurred, return original
        return term;
      }

      // Create new unop term with substituted operand
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_UNOP;
      new_term->base_type = term->base_type;
      new_term->data.unop.op = term->data.unop.op;
      new_term->data.unop.operand = new_operand;
      return new_term;
    }

    case CN_TERM_BINOP: {
      // Recursively substitute in both operands
      cn_term* new_left = cn_subst_term_impl(term->data.binop.left, subst_table);
      cn_term* new_right = cn_subst_term_impl(term->data.binop.right, subst_table);

      if (new_left == term->data.binop.left && new_right == term->data.binop.right) {
        // No substitution occurred, return original
        return term;
      }

      // Create new binop term with substituted operands
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_BINOP;
      new_term->base_type = term->base_type;
      new_term->data.binop.op = term->data.binop.op;
      new_term->data.binop.left = new_left;
      new_term->data.binop.right = new_right;
      return new_term;
    }

    case CN_TERM_ITE: {
      // Recursively substitute in condition and branches
      cn_term* new_cond = cn_subst_term_impl(term->data.ite.cond, subst_table);
      cn_term* new_then = cn_subst_term_impl(term->data.ite.then_term, subst_table);
      cn_term* new_else = cn_subst_term_impl(term->data.ite.else_term, subst_table);

      if (new_cond == term->data.ite.cond && new_then == term->data.ite.then_term &&
          new_else == term->data.ite.else_term) {
        // No substitution occurred, return original
        return term;
      }

      // Create new ITE term with substituted parts
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_ITE;
      new_term->base_type = term->base_type;
      new_term->data.ite.cond = new_cond;
      new_term->data.ite.then_term = new_then;
      new_term->data.ite.else_term = new_else;
      return new_term;
    }

    case CN_TERM_EACHI: {
      // Recursively substitute in the body
      cn_term* new_body = cn_subst_term_impl(term->data.eachi.body, subst_table);
      if (new_body == term->data.eachi.body) {
        // No substitution occurred, return original
        return term;
      }

      // Create new EACHI term with substituted body
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_EACHI;
      new_term->base_type = term->base_type;
      new_term->data.eachi.start = term->data.eachi.start;
      new_term->data.eachi.var_name = term->data.eachi.var_name;
      new_term->data.eachi.var_type = term->data.eachi.var_type;
      new_term->data.eachi.end = term->data.eachi.end;
      new_term->data.eachi.body = new_body;
      return new_term;
    }

    case CN_TERM_STRUCT: {
      // Create a new hash table for the copied members
      bennet_hash_table(const_char_ptr, cn_term_ptr) new_members;
      bennet_hash_table_init(const_char_ptr, cn_term_ptr)(
          &new_members, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

      bool changed = false;
      // Iterate through all entries in the original hash table
      for (size_t i = 0; i < term->data.struct_val.members.capacity; ++i) {
        if (term->data.struct_val.members.entries[i].occupied) {
          const char* key = term->data.struct_val.members.entries[i].key;
          cn_term* value = term->data.struct_val.members.entries[i].value;
          cn_term* new_value = cn_subst_term_impl(value, subst_table);

          // Add the (possibly substituted) value to the new hash table
          bennet_hash_table_set(const_char_ptr, cn_term_ptr)(
              &new_members, key, new_value);

          // Check if any substitution occurred
          if (new_value != value) {
            changed = true;
          }
        }
      }

      if (!changed) {
        // No substitution occurred, free the new hash table and return original
        bennet_hash_table_free(const_char_ptr, cn_term_ptr)(&new_members);
        return term;
      }

      // Create new STRUCT term with substituted members
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        bennet_hash_table_free(const_char_ptr, cn_term_ptr)(&new_members);
        return term;
      }

      new_term->type = CN_TERM_STRUCT;
      new_term->base_type = term->base_type;
      new_term->data.struct_val.tag = term->data.struct_val.tag;
      new_term->data.struct_val.members = new_members;
      return new_term;
    }

    case CN_TERM_STRUCT_MEMBER: {
      // Recursively substitute in the struct term
      cn_term* new_struct_term =
          cn_subst_term_impl(term->data.struct_member.struct_term, subst_table);
      if (new_struct_term == term->data.struct_member.struct_term) {
        // No substitution occurred, return original
        return term;
      }

      // Create new STRUCT_MEMBER term with substituted struct
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_STRUCT_MEMBER;
      new_term->base_type = term->base_type;
      new_term->data.struct_member.struct_term = new_struct_term;
      new_term->data.struct_member.member_name = term->data.struct_member.member_name;
      return new_term;
    }

    case CN_TERM_STRUCT_UPDATE: {
      // Recursively substitute in the struct term and new value
      cn_term* new_struct_term =
          cn_subst_term_impl(term->data.struct_update.struct_term, subst_table);
      cn_term* new_value =
          cn_subst_term_impl(term->data.struct_update.new_value, subst_table);

      if (new_struct_term == term->data.struct_update.struct_term &&
          new_value == term->data.struct_update.new_value) {
        // No substitution occurred, return original
        return term;
      }

      // Create new STRUCT_UPDATE term with substituted parts
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_STRUCT_UPDATE;
      new_term->base_type = term->base_type;
      new_term->data.struct_update.struct_term = new_struct_term;
      new_term->data.struct_update.member_name = term->data.struct_update.member_name;
      new_term->data.struct_update.new_value = new_value;
      return new_term;
    }

    case CN_TERM_RECORD: {
      // Create a new hash table for the copied members
      bennet_hash_table(const_char_ptr, cn_term_ptr) new_members;
      bennet_hash_table_init(const_char_ptr, cn_term_ptr)(
          &new_members, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

      bool changed = false;
      // Iterate through all entries in the original hash table
      for (size_t i = 0; i < term->data.record.members.capacity; ++i) {
        if (term->data.record.members.entries[i].occupied) {
          const char* key = term->data.record.members.entries[i].key;
          cn_term* value = term->data.record.members.entries[i].value;
          cn_term* new_value = cn_subst_term_impl(value, subst_table);

          // Add the (possibly substituted) value to the new hash table
          bennet_hash_table_set(const_char_ptr, cn_term_ptr)(
              &new_members, key, new_value);

          // Check if any substitution occurred
          if (new_value != value) {
            changed = true;
          }
        }
      }

      if (!changed) {
        // No substitution occurred, free the new hash table and return original
        bennet_hash_table_free(const_char_ptr, cn_term_ptr)(&new_members);
        return term;
      }

      // Create new RECORD term with substituted members
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        bennet_hash_table_free(const_char_ptr, cn_term_ptr)(&new_members);
        return term;
      }

      new_term->type = CN_TERM_RECORD;
      new_term->base_type = term->base_type;
      new_term->data.record.members = new_members;
      return new_term;
    }

    case CN_TERM_RECORD_MEMBER: {
      // Recursively substitute in the record term
      cn_term* new_record_term =
          cn_subst_term_impl(term->data.record_member.record_term, subst_table);
      if (new_record_term == term->data.record_member.record_term) {
        // No substitution occurred, return original
        return term;
      }

      // Create new RECORD_MEMBER term with substituted record
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_RECORD_MEMBER;
      new_term->base_type = term->base_type;
      new_term->data.record_member.record_term = new_record_term;
      new_term->data.record_member.member_name = term->data.record_member.member_name;
      return new_term;
    }

    case CN_TERM_CONSTRUCTOR: {
      // Create a new hash table for the copied args
      bennet_hash_table(const_char_ptr, cn_term_ptr) new_args;
      bennet_hash_table_init(const_char_ptr, cn_term_ptr)(
          &new_args, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

      bool changed = false;
      // Iterate through all entries in the original hash table
      for (size_t i = 0; i < term->data.constructor.args.capacity; ++i) {
        if (term->data.constructor.args.entries[i].occupied) {
          const char* key = term->data.constructor.args.entries[i].key;
          cn_term* value = term->data.constructor.args.entries[i].value;
          cn_term* new_value = cn_subst_term_impl(value, subst_table);

          // Add the (possibly substituted) value to the new hash table
          bennet_hash_table_set(const_char_ptr, cn_term_ptr)(&new_args, key, new_value);

          // Check if any substitution occurred
          if (new_value != value) {
            changed = true;
          }
        }
      }

      if (!changed) {
        // No substitution occurred, free the new hash table and return original
        bennet_hash_table_free(const_char_ptr, cn_term_ptr)(&new_args);
        return term;
      }

      // Create new CONSTRUCTOR term with substituted args
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        bennet_hash_table_free(const_char_ptr, cn_term_ptr)(&new_args);
        return term;
      }

      new_term->type = CN_TERM_CONSTRUCTOR;
      new_term->base_type = term->base_type;
      new_term->data.constructor.constructor_name =
          term->data.constructor.constructor_name;
      new_term->data.constructor.args = new_args;
      return new_term;
    }

    case CN_TERM_MEMBER_SHIFT: {
      // Recursively substitute in the base pointer
      cn_term* new_base = cn_subst_term_impl(term->data.member_shift.base, subst_table);
      if (new_base == term->data.member_shift.base) {
        // No substitution occurred, return original
        return term;
      }

      // Create new member shift term with substituted base
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_MEMBER_SHIFT;
      new_term->base_type = term->base_type;
      new_term->data.member_shift.base = new_base;
      new_term->data.member_shift.offset = term->data.member_shift.offset;
      return new_term;
    }

    case CN_TERM_ARRAY_SHIFT: {
      // Recursively substitute in base and index
      cn_term* new_base = cn_subst_term_impl(term->data.array_shift.base, subst_table);
      cn_term* new_index = cn_subst_term_impl(term->data.array_shift.index, subst_table);

      if (new_base == term->data.array_shift.base &&
          new_index == term->data.array_shift.index) {
        // No substitution occurred, return original
        return term;
      }

      // Create new array shift term with substituted parts
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_ARRAY_SHIFT;
      new_term->base_type = term->base_type;
      new_term->data.array_shift.base = new_base;
      new_term->data.array_shift.element_size = term->data.array_shift.element_size;
      new_term->data.array_shift.index = new_index;
      return new_term;
    }

    case CN_TERM_WRAPI: {
      // Recursively substitute in the value
      cn_term* new_value = cn_subst_term_impl(term->data.wrapi.value, subst_table);
      if (new_value == term->data.wrapi.value) {
        // No substitution occurred, return original
        return term;
      }

      // Create new WRAPI term with substituted value
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_WRAPI;
      new_term->base_type = term->base_type;
      new_term->data.wrapi.int_type = term->data.wrapi.int_type;
      new_term->data.wrapi.value = new_value;
      return new_term;
    }

    case CN_TERM_MAP_SET: {
      // Recursively substitute in map, key, and value
      cn_term* new_map = cn_subst_term_impl(term->data.map_set.map, subst_table);
      cn_term* new_key = cn_subst_term_impl(term->data.map_set.key, subst_table);
      cn_term* new_value = cn_subst_term_impl(term->data.map_set.value, subst_table);

      if (new_map == term->data.map_set.map && new_key == term->data.map_set.key &&
          new_value == term->data.map_set.value) {
        // No substitution occurred, return original
        return term;
      }

      // Create new MAP_SET term with substituted parts
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_MAP_SET;
      new_term->base_type = term->base_type;
      new_term->data.map_set.map = new_map;
      new_term->data.map_set.key = new_key;
      new_term->data.map_set.value = new_value;
      return new_term;
    }

    case CN_TERM_MAP_GET: {
      // Recursively substitute in map and key
      cn_term* new_map = cn_subst_term_impl(term->data.map_get.map, subst_table);
      cn_term* new_key = cn_subst_term_impl(term->data.map_get.key, subst_table);

      if (new_map == term->data.map_get.map && new_key == term->data.map_get.key) {
        // No substitution occurred, return original
        return term;
      }

      // Create new MAP_GET term with substituted parts
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_MAP_GET;
      new_term->base_type = term->base_type;
      new_term->data.map_get.map = new_map;
      new_term->data.map_get.key = new_key;
      return new_term;
    }

    case CN_TERM_APPLY: {
      // Create a new vector for the copied args
      bennet_vector(cn_term_ptr) new_args;
      bennet_vector_init(cn_term_ptr)(&new_args);

      bool changed = false;
      // Iterate through all entries in the original vector
      for (size_t i = 0; i < term->data.apply.args.size; ++i) {
        cn_term* arg = term->data.apply.args.data[i];
        cn_term* new_arg = cn_subst_term_impl(arg, subst_table);

        // Add the (possibly substituted) arg to the new vector
        bennet_vector_push(cn_term_ptr)(&new_args, new_arg);

        // Check if any substitution occurred
        if (new_arg != arg) {
          changed = true;
        }
      }

      if (!changed) {
        // No substitution occurred, free the new vector and return original
        bennet_vector_free(cn_term_ptr)(&new_args);
        return term;
      }

      // Create new APPLY term with substituted args
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        bennet_vector_free(cn_term_ptr)(&new_args);
        return term;
      }

      new_term->type = CN_TERM_APPLY;
      new_term->base_type = term->base_type;
      new_term->data.apply.function_name = term->data.apply.function_name;
      new_term->data.apply.args = new_args;
      return new_term;
    }

    case CN_TERM_LET: {
      // Recursively substitute in value and body
      cn_term* new_value = cn_subst_term_impl(term->data.let.value, subst_table);
      cn_term* new_body = cn_subst_term_impl(term->data.let.body, subst_table);

      if (new_value == term->data.let.value && new_body == term->data.let.body) {
        // No substitution occurred, return original
        return term;
      }

      // Create new LET term with substituted parts
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_LET;
      new_term->base_type = term->base_type;
      new_term->data.let.var_name = term->data.let.var_name;
      new_term->data.let.value = new_value;
      new_term->data.let.body = new_body;
      return new_term;
    }

    case CN_TERM_CAST: {
      // Recursively substitute in the value being cast
      cn_term* new_value = cn_subst_term_impl(term->data.cast.value, subst_table);
      if (new_value == term->data.cast.value) {
        // No substitution occurred, return original
        return term;
      }

      // Create new cast term with substituted value
      cn_term* new_term = malloc(sizeof(cn_term));
      if (!new_term) {
        return term;
      }

      new_term->type = CN_TERM_CAST;
      new_term->base_type = term->base_type;
      new_term->data.cast.target_type = term->data.cast.target_type;
      new_term->data.cast.value = new_value;
      return new_term;
    }

    // For other term types, we don't substitute (yet)
    // These would need more complex handling for nested terms
    default:
      return term;
  }
}

// Create a substitution table with the proper hash and equality functions
bennet_hash_table(uint64_t, cn_term_ptr) * cn_create_subst_table(void) {
  bennet_hash_table(uint64_t, cn_term_ptr)* table = malloc(sizeof(*table));
  if (!table) {
    return NULL;
  }

  bennet_hash_table_init(uint64_t, cn_term_ptr)(
      table, bennet_hash_uint64_t, bennet_eq_uint64_t);
  return table;
}

// Free a substitution table
void cn_free_subst_table(bennet_hash_table(uint64_t, cn_term_ptr) * table) {
  if (table) {
    bennet_hash_table_free(uint64_t, cn_term_ptr)(table);
    free(table);
  }
}

// Add a substitution to the table
void cn_add_substitution(
    bennet_hash_table(uint64_t, cn_term_ptr) * table, uint64_t symbol_id, cn_term* term) {
  if (table && term) {
    bennet_hash_table_set(uint64_t, cn_term_ptr)(table, symbol_id, term);
  }
}
