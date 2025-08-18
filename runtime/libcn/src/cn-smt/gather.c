#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include <bennet/state/rand_alloc.h>
#include <cn-smt/gather.h>
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
bool cn_smt_gather_add_logical_term(cn_term* term) {
  if (!cn_gather_context) {
    cn_smt_gather_init();
  }
  cn_logical_constraint* constraint = cn_logical_constraint_create_term(term);
  if (constraint) {
    return cn_context_add_logical_constraint(cn_gather_context, constraint);
  }
  return false;
}

bool cn_smt_gather_add_logical_forall(
    cn_sym var_name, cn_base_type var_type, cn_term* body) {
  if (!cn_gather_context) {
    cn_smt_gather_init();
  }
  cn_logical_constraint* constraint =
      cn_logical_constraint_create_forall(var_name, var_type, body);
  if (constraint) {
    return cn_context_add_logical_constraint(cn_gather_context, constraint);
  }
  return false;
}

bool cn_smt_gather_add_assignment(size_t bytes, cn_term* pointer, cn_term* value) {
  if (!cn_gather_context) {
    cn_smt_gather_init();
  }
  cn_resource_constraint* constraint =
      cn_resource_constraint_create_predicate(bytes, pointer);
  if (constraint) {
    return cn_context_add_resource_constraint(cn_gather_context, constraint);
  }
  return false;
}

// Create a fresh symbolic variable of the given type
cn_term* cn_smt_gather_create_symbolic_var(const char* name, cn_base_type type) {
  if (!cn_gather_context) {
    cn_smt_gather_init();
  }
  // Increment the counter in the context
  cn_gather_context->counter++;

  cn_sym sym = {.name = strdup(name), .id = cn_gather_context->counter};
  cn_term* res = cn_smt_sym(sym, type);
  cn_context_add_variable(cn_gather_context, res->data.sym, type);

  return res;
}

// Call a defined generator function with arguments
cn_term* cn_smt_gather_call_function(
    cn_sym function_symbol, cn_term** args, size_t arg_count) {
  // Create a vector to hold the arguments
  bennet_vector(cn_term_ptr) arg_vector;
  bennet_vector_init(cn_term_ptr)(&arg_vector);

  // Add all arguments to the vector
  for (size_t i = 0; i < arg_count; i++) {
    bennet_vector_push(cn_term_ptr)(&arg_vector, args[i]);
  }

  // Create a function application term
  // Note: We assume the result type is CN_BASE_INTEGER for now
  // In a real implementation, this would need proper type inference
  cn_term* result = cn_smt_apply(
      function_symbol.name, cn_base_type_simple(CN_BASE_INTEGER), &arg_vector);

  bennet_vector_free(cn_term_ptr)(&arg_vector);
  return result;
}

// Conditional branching with probabilistic choice
bool cn_smt_gather_conditional_branch(cn_term* condition, uint8_t x, uint8_t y) {
  // Get a random number in range [0, x + y)
  uint8_t random_value = bennet_uniform_uint8_t(x + y);

  // Return true if random_value < x (then branch)
  // Return false if random_value >= x (else branch)
  return random_value < x;
}

// Weighted choice implementation
uint64_t cn_smt_gather_weighted_choice(uint64_t* choices, size_t num_choices) {
  // Simple implementation - for now just return the first choice
  // In a real implementation, this would use the weights for probabilistic selection
  return choices[0];
}

// Helper callback for declaring variables in SMT solver
static void declare_variable_callback(cn_sym var, cn_base_type type, void* user_data) {
  struct cn_smt_solver* solver = (struct cn_smt_solver*)user_data;
  cn_declare_variable(solver, var, type);
}

enum cn_smt_solver_result cn_smt_gather_model(struct cn_smt_solver* smt_solver) {
  if (!cn_gather_context) {
    // No constraints to check, return satisfiable
    stop_solver(smt_solver);
    return true;
  }

  // Declare all variables in the context first
  cn_context_foreach_variable(cn_gather_context, declare_variable_callback, smt_solver);

  // Add all logical constraints as assertions to the solver
  const cn_logical_constraint* logical_constraint =
      cn_context_first_logical(cn_gather_context);
  while (logical_constraint != NULL) {
    sexp_t* smt_expr = NULL;

    switch (logical_constraint->type) {
      case CN_LOGICAL_TERM: {
        // Convert the term to SMT and add as assertion
        smt_expr = translate_term(smt_solver, logical_constraint->data.term);
        break;
      }

      case CN_LOGICAL_FORALL: {
        // For forall constraints, we need to create a quantified formula
        // Note: This is simplified - in practice you might need more sophisticated handling
        sexp_t* body_smt =
            translate_term(smt_solver, logical_constraint->data.forall.body);

        // Create variable binding for the quantifier
        sexp_t* var_smt = symbol(logical_constraint->data.forall.var_name.name);
        sexp_t* var_type_smt = NULL;

        // Convert base type to SMT type
        switch (logical_constraint->data.forall.var_type.tag) {
          case CN_BASE_INTEGER:
            var_type_smt = t_int();
            break;
          case CN_BASE_BOOL:
            var_type_smt = t_bool();
            break;
          case CN_BASE_BITS: {
            int width = logical_constraint->data.forall.var_type.data.bits.size_bits;
            var_type_smt = t_bits(width);
            break;
          }
          default:
            var_type_smt = t_int();  // fallback
            break;
        }

        sexp_t* binding = sexp_list((sexp_t*[]){var_smt, var_type_smt}, 2);
        sexp_t* bindings[] = {binding};
        smt_expr = forall(bindings, 1, body_smt);
        break;
      }
    }

    if (smt_expr) {
      // Add the constraint as an assertion
      sexp_t* assert_cmd = assume(smt_expr);
      ack_command(smt_solver, assert_cmd);
      sexp_free(assert_cmd);
    }

    logical_constraint = logical_constraint->next;
  }

  // Handle resource constraints
  const cn_resource_constraint* resource_constraint =
      cn_context_first_resource(cn_gather_context);
  while (resource_constraint != NULL) {
    // Get min and max pointer bounds
    void* min_ptr = bennet_rand_alloc_min_ptr();
    void* max_ptr = bennet_rand_alloc_max_ptr();

    // Convert pointers to SMT terms
    sexp_t* pointer_smt = translate_term(smt_solver, resource_constraint->pointer);
    sexp_t* min_ptr_smt = loc_k((uintptr_t)min_ptr);
    sexp_t* max_ptr_smt = loc_k((uintptr_t)max_ptr);

    // Create assertion: `min_ptr <= pointer`
    sexp_t* min_bound_expr = bv_uleq(min_ptr_smt, pointer_smt);
    sexp_t* min_bound_assert = assume(min_bound_expr);
    ack_command(smt_solver, min_bound_assert);
    sexp_free(min_bound_assert);

    // Create assertion: `pointer + bytes <= max_ptr`
    sexp_t* bytes_smt = loc_k(resource_constraint->bytes);
    sexp_t* pointer_plus_bytes = bv_add(pointer_smt, bytes_smt);
    sexp_t* max_bound_expr = bv_uleq(pointer_plus_bytes, max_ptr_smt);
    sexp_t* max_bound_assert = assume(max_bound_expr);
    ack_command(smt_solver, max_bound_assert);
    sexp_free(max_bound_assert);

    // No overflow on `pointer + bytes`
    sexp_t* no_overflow_expr = bv_uleq(pointer_smt, pointer_plus_bytes);
    sexp_t* no_overflow_assert = assume(no_overflow_expr);
    ack_command(smt_solver, no_overflow_assert);
    sexp_free(no_overflow_assert);

    // Ensure exclusive ownership (non-overlapping)
    // For each other resource constraint, assert they don't overlap
    const cn_resource_constraint* other_resource =
        cn_context_first_resource(cn_gather_context);
    while (other_resource != NULL) {
      // Skip comparison with itself
      if (other_resource == resource_constraint) {
        other_resource = other_resource->next;
        continue;
      }

      {
        // For two resources (pointer_a, bytes_a) and (pointer_b, bytes_b),
        // assert: pointer_a + bytes_a <= pointer_b || pointer_b + bytes_b <= pointer_a

        sexp_t* other_pointer_smt = translate_term(smt_solver, other_resource->pointer);
        sexp_t* other_bytes_smt = loc_k(other_resource->bytes);
        sexp_t* other_pointer_plus_bytes = bv_add(other_pointer_smt, other_bytes_smt);

        // Condition 1: current_pointer + current_bytes <= other_pointer
        sexp_t* cond1 = bv_uleq(pointer_plus_bytes, other_pointer_smt);

        // Condition 2: other_pointer + other_bytes <= current_pointer
        sexp_t* cond2 = bv_uleq(other_pointer_plus_bytes, pointer_smt);

        // Non-overlap constraint: cond1 || cond2
        sexp_t* non_overlap_expr = bool_or(cond1, cond2);
        sexp_t* non_overlap_assert = assume(non_overlap_expr);
        ack_command(smt_solver, non_overlap_assert);
        sexp_free(non_overlap_assert);
      }

      other_resource = other_resource->next;
    }

    resource_constraint = resource_constraint->next;
  }

  // Check satisfiability
  enum cn_smt_solver_result result = check(smt_solver);

  return result;
}
