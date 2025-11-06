#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <bennet/internals/domains/sized.h>
#include <bennet/state/rand_alloc.h>
#include <cn-smt/gather.h>
#include <cn-smt/memory/test_alloc.h>
#include <cn-smt/solver.h>
#include <cn-smt/to_smt.h>

// Vector implementation for cn_term_ptr
BENNET_VECTOR_IMPL(cn_term_ptr)

// Global gather constraint context
cn_constraint_context* cn_gather_context = NULL;

// Optional explicit initialization function
void cn_smt_gather_init(void) {
  if (!cn_gather_context) {
    cn_gather_context = cn_context_create();
  } else {
    cn_smt_gather_reset();
  }
}

// Check if gather context is initialized
bool cn_smt_gather_is_initialized(void) {
  return cn_gather_context != NULL;
}

// Get current gather context (may return NULL if not initialized)
cn_constraint_context* cn_smt_gather_get_context(void) {
  return cn_gather_context;
}

// Force cleanup of gather context
void cn_smt_gather_cleanup(void) {
  if (cn_gather_context) {
    cn_context_destroy(cn_gather_context);
    cn_gather_context = NULL;
  }
}

// Reset gather context (clear constraints but keep context alive)
void cn_smt_gather_reset(void) {
  if (cn_gather_context) {
    cn_context_clear(cn_gather_context);
  }
}

// Get constraint counts
size_t cn_smt_gather_resource_count(void) {
  return cn_gather_context ? cn_context_resource_count(cn_gather_context) : 0;
}

size_t cn_smt_gather_logical_count(void) {
  return cn_gather_context ? cn_context_logical_count(cn_gather_context) : 0;
}

// Add constraints using the function interface (alternative to macros)
void cn_smt_gather_add_logical_term(cn_term* term) {
  assert(term);
  if (!cn_gather_context) {
    cn_smt_gather_init();
  }
  cn_logical_constraint* constraint = cn_logical_constraint_create_term(term);
  assert(constraint);

  cn_context_add_logical_constraint(cn_gather_context, constraint);
}

void cn_smt_gather_add_logical_forall(
    cn_sym var_name, cn_base_type var_type, cn_term* body) {
  assert(body);
  if (!cn_gather_context) {
    cn_smt_gather_init();
  }
  cn_logical_constraint* constraint =
      cn_logical_constraint_create_forall(var_name, var_type, body);
  assert(constraint);

  cn_context_add_logical_constraint(cn_gather_context, constraint);
}

void cn_smt_gather_add_assignment(cn_term* pointer, size_t bytes, size_t alignment) {
  assert(pointer);
  assert(alignment);

  if (!cn_gather_context) {
    cn_smt_gather_init();
  }

  // Create end address: pointer + bytes - 1
  cn_term* end_addr;

  // Handle edge case: if bytes is 0, create an invalid range (start > end)
  assert(bytes);

  cn_term* bytes_term = cn_smt_bits(false, 64, (intmax_t)bytes);
  cn_term* one_term = cn_smt_bits(false, 64, 1);
  cn_term* bytes_minus_one = cn_smt_sub(bytes_term, one_term);
  end_addr = cn_smt_array_shift(pointer, 1, bytes_minus_one);

  cn_resource_constraint* constraint =
      cn_resource_constraint_create_predicate(pointer, end_addr, alignment);
  assert(constraint);

  cn_context_add_resource_constraint(cn_gather_context, constraint);
}

void cn_smt_gather_add_array_assignment(
    cn_term* start_addr, cn_term* end_addr, size_t alignment) {
  assert(start_addr);
  assert(end_addr);
  assert(alignment);

  if (!cn_gather_context) {
    cn_smt_gather_init();
  }

  cn_resource_constraint* constraint =
      cn_resource_constraint_create_predicate(start_addr, end_addr, alignment);
  assert(constraint);

  cn_context_add_resource_constraint(cn_gather_context, constraint);
}

// Create a fresh symbolic variable of the given type
cn_term* cn_smt_gather_create_symbolic_var(const char* name, cn_base_type type) {
  assert(name);
  if (!cn_gather_context) {
    cn_smt_gather_init();
  }

  // Increment the counter in the context
  cn_gather_context->counter++;

  cn_sym sym = {.name = cn_test_strdup(name), .id = cn_gather_context->counter};
  cn_term* res = cn_smt_sym(sym, type);

  // Generate skew value based on type
  cn_term* skew = NULL;

  // For pointers and bitvectors, generate skew values using bennet_arbitrary
  switch (type.tag) {
    case CN_BASE_LOC: {
      // Generate an arbitrary pointer value as skew
      uintptr_t generated_ptr = bennet_arbitrary_sized_uintptr_t_top();
      skew = cn_smt_pointer(generated_ptr);
      break;
    }

    case CN_BASE_BITS: {
      cn_bits_info bits_info = cn_base_type_get_bits_info(type);

      if (bits_info.is_signed) {
        // Handle signed bitvectors
        switch (bits_info.size_bits) {
          case 8: {
            int8_t generated_val = bennet_arbitrary_sized_int8_t_top();
            skew = cn_smt_bits(true, 8, generated_val);
            break;
          }
          case 16: {
            int16_t generated_val = bennet_arbitrary_sized_int16_t_top();
            skew = cn_smt_bits(true, 16, generated_val);
            break;
          }
          case 32: {
            int32_t generated_val = bennet_arbitrary_sized_int32_t_top();
            skew = cn_smt_bits(true, 32, generated_val);
            break;
          }
          case 64: {
            int64_t generated_val = bennet_arbitrary_sized_int64_t_top();
            skew = cn_smt_bits(true, 64, generated_val);
            break;
          }

          default:
            assert(false);  // Unreachable
        }
      } else {
        // Handle unsigned bitvectors
        switch (bits_info.size_bits) {
          case 8: {
            uint8_t generated_val = bennet_arbitrary_sized_uint8_t_top();
            skew = cn_smt_bits(false, 8, generated_val);
            break;
          }
          case 16: {
            uint16_t generated_val = bennet_arbitrary_sized_uint16_t_top();
            skew = cn_smt_bits(false, 16, generated_val);
            break;
          }
          case 32: {
            uint32_t generated_val = bennet_arbitrary_sized_uint32_t_top();
            skew = cn_smt_bits(false, 32, generated_val);
            break;
          }
          case 64: {
            uint64_t generated_val = bennet_arbitrary_sized_uint64_t_top();
            skew = cn_smt_bits(false, 64, generated_val);
            break;
          }

          default:
            assert(false);  // Unreachable
        }
      }
      break;
    }

    default:
      printf("DEBUG: Unhandled type tag in cn_smt_gather_create_symbolic_var: %d\n",
          type.tag);
      fflush(stdout);
      assert(false);  // Unreachable
  }

  // Add variable with its skew to the context
  cn_context_add_variable(cn_gather_context, res->data.sym, type, skew);

  return res;
}

// Call a defined generator function with arguments
cn_term* cn_smt_gather_call_function(
    cn_sym function_symbol, cn_term** args, size_t arg_count) {
  // Create a function application term
  // Note: We assume the result type is CN_BASE_INTEGER for now
  // In a real implementation, this would need proper type inference
  return cn_smt_apply(
      function_symbol.name, cn_base_type_simple(CN_BASE_INTEGER), args, arg_count);
}

// Conditional branching with probabilistic choice
bool cn_smt_gather_conditional_branch(cn_term* condition, uint8_t x, uint8_t y) {
  // Get a random number in range [0, x + y)
  uint8_t random_value = bennet_uniform_uint8_t(x + y);

  // Return true if random_value < x (then branch)
  // Return false if random_value >= x (else branch)
  return random_value < x;
}

enum cn_smt_solver_result cn_smt_gather_model(struct cn_smt_solver* smt_solver) {
  return cn_smt_context_model(smt_solver, cn_gather_context);
}
