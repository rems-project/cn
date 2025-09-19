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
  end_addr = cn_smt_add(pointer, bytes_minus_one);

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
    return CN_SOLVER_SAT;
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

  // Setup resource bounds
  if (resource_constraint != NULL) {
    sexp_t* define_min_ptr = define_const(
        "bennet_min_ptr", t_loc(), loc_k((uintptr_t)bennet_rand_alloc_min_ptr()));
    ack_command(smt_solver, define_min_ptr);
    sexp_free(define_min_ptr);

    sexp_t* define_max_ptr = define_const(
        "bennet_max_ptr", t_loc(), loc_k((uintptr_t)bennet_rand_alloc_max_ptr()));
    ack_command(smt_solver, define_max_ptr);
    sexp_free(define_max_ptr);

    sexp_t* min_ptr_smt = sexp_atom("bennet_min_ptr");
    sexp_t* max_ptr_smt = sexp_atom("bennet_max_ptr");

    while (resource_constraint != NULL) {
      // Convert start and end addresses to SMT terms
      sexp_t* start_addr_smt =
          translate_term(smt_solver, resource_constraint->start_addr);
      sexp_t* end_addr_smt = translate_term(smt_solver, resource_constraint->end_addr);

      // Create assertion: `min_ptr <= start_addr`
      sexp_t* min_bound_expr = bv_uleq(min_ptr_smt, start_addr_smt);
      sexp_t* min_bound_assert = assume(min_bound_expr);
      ack_command(smt_solver, min_bound_assert);
      sexp_free(min_bound_assert);

      // Create assertion: `end_addr <= max_ptr`
      sexp_t* max_bound_expr = bv_uleq(end_addr_smt, max_ptr_smt);
      sexp_t* max_bound_assert = assume(max_bound_expr);
      ack_command(smt_solver, max_bound_assert);
      sexp_free(max_bound_assert);

      // CRITICAL: Prevent bitvector overflow by ensuring start_addr doesn't get too close to max
      // Add constraint: start_addr + 65536 <= max_ptr (leave safe margin)
      sexp_t* safety_margin = loc_k(65536);
      sexp_t* start_plus_margin = bv_add(start_addr_smt, safety_margin);
      sexp_t* overflow_guard_expr = bv_uleq(start_plus_margin, max_ptr_smt);
      sexp_t* overflow_guard_assert = assume(overflow_guard_expr);
      ack_command(smt_solver, overflow_guard_assert);
      sexp_free(overflow_guard_assert);
      sexp_free(safety_margin);
      sexp_free(start_plus_margin);

      // Ensure start_addr <= end_addr (prevents overflow and ensures validity)
      sexp_t* validity_expr = bv_uleq(start_addr_smt, end_addr_smt);
      sexp_t* validity_assert = assume(validity_expr);
      ack_command(smt_solver, validity_assert);
      sexp_free(validity_assert);

      // Alignment
      if (resource_constraint->alignment > 1) {
        sexp_t* alignment_smt = loc_k(resource_constraint->alignment);
        sexp_t* start_mask_align =
            bv_and(start_addr_smt, bv_sub(alignment_smt, loc_k(1)));
        sexp_t* zero_smt = loc_k(0);
        sexp_t* alignment_expr =
            sexp_list((sexp_t*[]){sexp_atom("="), start_mask_align, zero_smt}, 3);
        sexp_t* alignment_assert = assume(alignment_expr);
        ack_command(smt_solver, alignment_assert);
        sexp_free(alignment_assert);
        sexp_free(alignment_smt);
        sexp_free(zero_smt);
      }

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
          // For two resources (start_a, end_a) and (start_b, end_b),
          // assert: end_a < start_b || end_b < start_a

          sexp_t* other_start_smt =
              translate_term(smt_solver, other_resource->start_addr);
          sexp_t* other_end_smt = translate_term(smt_solver, other_resource->end_addr);

          // Condition 1: current_end < other_start
          sexp_t* cond1 = bv_ult(end_addr_smt, other_start_smt);

          // Condition 2: other_end < current_start
          sexp_t* cond2 = bv_ult(other_end_smt, start_addr_smt);

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
  }

  // Check satisfiability
  return check(smt_solver);
}
