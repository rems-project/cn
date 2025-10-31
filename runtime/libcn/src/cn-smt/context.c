#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/state/rand_alloc.h>
#include <cn-smt/context.h>
#include <cn-smt/subst.h>
#include <cn-smt/to_smt.h>

// Global flag to control whether SMT skewing is enabled
static bool smt_skewing_enabled = true;

void cn_set_smt_skewing_enabled(bool enabled) {
  smt_skewing_enabled = enabled;
}

// Generate hash table implementation for uint64_t -> cn_term_ptr
BENNET_HASH_TABLE_IMPL(uint64_t, cn_term_ptr)

// Generate hash table implementation for cn_sym -> cn_variable_entry
BENNET_HASH_TABLE_IMPL(cn_sym, cn_variable_entry)

// Generate vector implementation for cn_term_ptr
BENNET_VECTOR_IMPL(cn_term_ptr)

// Generate vector implementation for cn_sym
BENNET_VECTOR_IMPL(cn_sym)

// Context management functions
cn_constraint_context* cn_context_create(void) {
  cn_constraint_context* ctx = malloc(sizeof(cn_constraint_context));
  assert(ctx);

  ctx->resource_constraints = NULL;
  ctx->logical_constraints = NULL;
  ctx->resource_count = 0;
  ctx->logical_count = 0;
  ctx->counter = 0;

  // Initialize variables hash table
  ctx->variables = malloc(sizeof(bennet_hash_table(cn_sym, cn_variable_entry)));
  assert(ctx->variables);
  bennet_hash_table_init(cn_sym, cn_variable_entry)(
      ctx->variables, bennet_hash_cn_sym, bennet_eq_cn_sym);

  return ctx;
}

void cn_context_destroy(cn_constraint_context* ctx) {
  if (!ctx)
    return;

  // Free all resource constraints
  cn_resource_constraint* resource = ctx->resource_constraints;
  while (resource) {
    cn_resource_constraint* next = resource->next;
    cn_resource_constraint_destroy(resource);
    resource = next;
  }

  // Free all logical constraints
  cn_logical_constraint* logical = ctx->logical_constraints;
  while (logical) {
    cn_logical_constraint* next = logical->next;
    cn_logical_constraint_destroy(logical);
    logical = next;
  }

  // Free variables hash table
  bennet_hash_table_free(cn_sym, cn_variable_entry)(ctx->variables);
  free(ctx->variables);

  free(ctx);
}

void cn_context_clear(cn_constraint_context* ctx) {
  if (!ctx)
    return;

  // Free all resource constraints
  cn_resource_constraint* resource = ctx->resource_constraints;
  while (resource) {
    cn_resource_constraint* next = resource->next;
    cn_resource_constraint_destroy(resource);
    resource = next;
  }
  ctx->resource_constraints = NULL;
  ctx->resource_count = 0;

  // Free all logical constraints
  cn_logical_constraint* logical = ctx->logical_constraints;
  while (logical) {
    cn_logical_constraint* next = logical->next;
    cn_logical_constraint_destroy(logical);
    logical = next;
  }
  ctx->logical_constraints = NULL;
  ctx->logical_count = 0;

  // Clear variables hash table
  bennet_hash_table_clear(cn_sym, cn_variable_entry)(ctx->variables);

  // Reset counter
  ctx->counter = 0;
}

// Resource constraint management
cn_resource_constraint* cn_resource_constraint_create_predicate(
    cn_term* start_addr, cn_term* end_addr, size_t alignment) {
  assert(start_addr);
  assert(end_addr);
  assert(alignment);

  cn_resource_constraint* constraint = malloc(sizeof(cn_resource_constraint));
  assert(constraint);

  constraint->start_addr = start_addr;
  constraint->end_addr = end_addr;
  constraint->alignment = alignment;
  constraint->next = NULL;

  return constraint;
}

void cn_context_add_resource_constraint(
    cn_constraint_context* ctx, cn_resource_constraint* constraint) {
  assert(ctx && constraint);

  assert(constraint->start_addr->base_type.tag == CN_BASE_LOC);
  assert(constraint->end_addr->base_type.tag == CN_BASE_LOC);

  // Add to front of linked list
  constraint->next = ctx->resource_constraints;
  ctx->resource_constraints = constraint;
  ctx->resource_count++;
}

void cn_resource_constraint_destroy(cn_resource_constraint* constraint) {
  if (!constraint) {
    return;
  }

  free(constraint);
}

// Logical constraint management
cn_logical_constraint* cn_logical_constraint_create_term(cn_term* term) {
  assert(term);

  cn_logical_constraint* constraint = malloc(sizeof(cn_logical_constraint));
  assert(constraint);

  constraint->type = CN_LOGICAL_TERM;
  constraint->data.term = term;
  constraint->next = NULL;

  return constraint;
}

cn_logical_constraint* cn_logical_constraint_create_forall(
    cn_sym var_name, cn_base_type var_type, cn_term* body) {
  assert(var_name.name && body);

  cn_logical_constraint* constraint = malloc(sizeof(cn_logical_constraint));
  assert(constraint);

  constraint->type = CN_LOGICAL_FORALL;
  constraint->data.forall.var_name = var_name;
  constraint->data.forall.var_type = var_type;
  constraint->data.forall.body = body;
  constraint->next = NULL;

  return constraint;
}

void cn_context_add_logical_constraint(
    cn_constraint_context* ctx, cn_logical_constraint* constraint) {
  assert(ctx && constraint);

  // Add to front of linked list
  constraint->next = ctx->logical_constraints;
  ctx->logical_constraints = constraint;
  ctx->logical_count++;
}

void cn_logical_constraint_destroy(cn_logical_constraint* constraint) {
  if (!constraint)
    return;

  if (constraint->type == CN_LOGICAL_FORALL) {
  }

  free(constraint);
}

// Variable context management
void cn_context_add_variable(
    cn_constraint_context* ctx, cn_sym var, cn_base_type type, cn_term* skew) {
  assert(ctx && ctx->variables);

  cn_variable_entry entry = {.type = type, .skew = skew};
  bennet_hash_table_set(cn_sym, cn_variable_entry)(ctx->variables, var, entry);
}

bennet_optional(cn_base_type)
    cn_context_get_variable_type(const cn_constraint_context* ctx, cn_sym var) {
  assert(ctx && ctx->variables);

  bennet_optional(cn_variable_entry) entry =
      bennet_hash_table_get(cn_sym, cn_variable_entry)(ctx->variables, var);
  if (entry.is_some) {
    return bennet_optional_some(cn_base_type, entry.body.type);
  } else {
    return bennet_optional_none(cn_base_type);
  }
}

cn_term* cn_context_get_variable_skew(const cn_constraint_context* ctx, cn_sym var) {
  assert(ctx && ctx->variables);

  bennet_optional(cn_variable_entry) entry =
      bennet_hash_table_get(cn_sym, cn_variable_entry)(ctx->variables, var);
  if (entry.is_some) {
    return entry.body.skew;
  } else {
    return NULL;
  }
}

bool cn_context_has_variable(const cn_constraint_context* ctx, cn_sym var) {
  assert(ctx && ctx->variables);

  return bennet_hash_table_contains(cn_sym, cn_variable_entry)(ctx->variables, var);
}

size_t cn_context_variable_count(const cn_constraint_context* ctx) {
  assert(ctx && ctx->variables);

  return bennet_hash_table_size(cn_sym, cn_variable_entry)(ctx->variables);
}

void cn_context_foreach_variable(
    const cn_constraint_context* ctx, cn_variable_callback callback, void* user_data) {
  assert(ctx && ctx->variables && callback);

  // Iterate through all entries in the hash table
  for (size_t i = 0; i < ctx->variables->capacity; i++) {
    if (ctx->variables->entries[i].occupied) {
      callback(
          ctx->variables->entries[i].key, ctx->variables->entries[i].value, user_data);
    }
  }
}

// Query functions
size_t cn_context_resource_count(const cn_constraint_context* ctx) {
  return ctx ? ctx->resource_count : 0;
}

size_t cn_context_logical_count(const cn_constraint_context* ctx) {
  return ctx ? ctx->logical_count : 0;
}

// Iterator functions
const cn_resource_constraint* cn_context_first_resource(
    const cn_constraint_context* ctx) {
  return ctx ? ctx->resource_constraints : NULL;
}

const cn_logical_constraint* cn_context_first_logical(const cn_constraint_context* ctx) {
  return ctx ? ctx->logical_constraints : NULL;
}

// Debug/printing functions
void cn_context_print_summary(const cn_constraint_context* ctx) {
  if (!ctx) {
    printf("Context: NULL\n");
    return;
  }

  printf("Constraint Context Summary:\n");
  printf("  Resource constraints: %zu\n", ctx->resource_count);
  printf("  Logical constraints: %zu\n", ctx->logical_count);

  // Print resource constraints
  if (ctx->resource_count > 0) {
    printf("  Resource constraints:\n");
    const cn_resource_constraint* resource = ctx->resource_constraints;
    int i = 0;
    while (resource && i < 5) {  // Limit to first 5 for summary
      printf("    [%d] Start: %s\n", i, cn_term_to_string(resource->start_addr));
      printf("         End: %s\n", cn_term_to_string(resource->end_addr));

      resource = resource->next;
      i++;
    }
    if (ctx->resource_count > 5) {
      printf("    ... and %zu more\n", ctx->resource_count - 5);
    }
  }

  // Print logical constraints
  if (ctx->logical_count > 0) {
    printf("  Logical constraints:\n");
    const cn_logical_constraint* logical = ctx->logical_constraints;
    int i = 0;
    while (logical && i < 5) {  // Limit to first 5 for summary
      printf(
          "    [%d] Type: %s\n", i, logical->type == CN_LOGICAL_TERM ? "Term" : "Forall");
      if (logical->type == CN_LOGICAL_FORALL) {
        printf("         Quantified var: %s\n", logical->data.forall.var_name.name);
      }
      logical = logical->next;
      i++;
    }
    if (ctx->logical_count > 5) {
      printf("    ... and %zu more\n", ctx->logical_count - 5);
    }
  }
}

enum cn_smt_solver_result cn_smt_context_model(
    struct cn_smt_solver* smt_solver, const cn_constraint_context* ctx) {
  assert(ctx != NULL);
  assert(ctx->variables != NULL);

  // Declare all variables in the context first
  for (size_t i = 0; i < ctx->variables->capacity; i++) {
    if (ctx->variables->entries[i].occupied) {
      cn_sym var = ctx->variables->entries[i].key;
      cn_variable_entry entry = ctx->variables->entries[i].value;
      cn_declare_variable(smt_solver, var, entry.type);

      if (smt_skewing_enabled) {
        cn_term* eq_term = cn_smt_eq(cn_smt_sym(var, entry.type), entry.skew);
        sexp_t* eq_smt = translate_term(smt_solver, eq_term);
        sexp_t* eq_assert = assume_soft(eq_smt);
        ack_command(smt_solver, eq_assert);
      }
    }
  }

  // Add all logical constraints as assertions to the solver
  const cn_logical_constraint* logical_constraint = ctx->logical_constraints;
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
    }

    logical_constraint = logical_constraint->next;
  }

  // Handle resource constraints
  const cn_resource_constraint* resource_constraint = ctx->resource_constraints;

  // Setup resource bounds
  if (resource_constraint != NULL) {
    sexp_t* define_min_ptr = define_const(
        "bennet_min_ptr", t_loc(), loc_k((uintptr_t)bennet_rand_alloc_min_ptr()));
    ack_command(smt_solver, define_min_ptr);

    sexp_t* define_max_ptr = define_const(
        "bennet_max_ptr", t_loc(), loc_k((uintptr_t)bennet_rand_alloc_max_ptr()));
    ack_command(smt_solver, define_max_ptr);

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

      // Create assertion: `end_addr <= max_ptr`
      sexp_t* max_bound_expr = bv_uleq(end_addr_smt, max_ptr_smt);
      sexp_t* max_bound_assert = assume(max_bound_expr);
      ack_command(smt_solver, max_bound_assert);

      // CRITICAL: Prevent bitvector overflow by ensuring start_addr doesn't get too close to max
      // Add constraint: start_addr + 65536 <= max_ptr (leave safe margin)
      sexp_t* safety_margin = loc_k(65536);
      sexp_t* start_plus_margin = bv_add(start_addr_smt, safety_margin);
      sexp_t* overflow_guard_expr = bv_uleq(start_plus_margin, max_ptr_smt);
      sexp_t* overflow_guard_assert = assume(overflow_guard_expr);
      ack_command(smt_solver, overflow_guard_assert);

      // Ensure start_addr <= end_addr (prevents overflow and ensures validity)
      sexp_t* validity_expr = bv_uleq(start_addr_smt, end_addr_smt);
      sexp_t* validity_assert = assume(validity_expr);
      ack_command(smt_solver, validity_assert);

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
      }

      // Ensure exclusive ownership (non-overlapping)
      // For each other resource constraint, assert they don't overlap
      const cn_resource_constraint* other_resource = ctx->resource_constraints;
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
        }

        other_resource = other_resource->next;
      }

      resource_constraint = resource_constraint->next;
    }
  }

  // Check satisfiability
  return check(smt_solver);
}
