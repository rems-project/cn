#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/context.h>
#include <cn-smt/subst.h>

// Generate hash table implementation for uint64_t -> cn_term_ptr
BENNET_HASH_TABLE_IMPL(uint64_t, cn_term_ptr)

// Generate hash table implementation for cn_sym -> cn_base_type
BENNET_HASH_TABLE_IMPL(cn_sym, cn_base_type)

// Generate vector implementation for cn_term_ptr
BENNET_VECTOR_IMPL(cn_term_ptr)

// Generate vector implementation for cn_sym
BENNET_VECTOR_IMPL(cn_sym)

// Context management functions
cn_constraint_context* cn_context_create(void) {
  cn_constraint_context* ctx = malloc(sizeof(cn_constraint_context));
  if (!ctx)
    return NULL;

  ctx->resource_constraints = NULL;
  ctx->logical_constraints = NULL;
  ctx->resource_count = 0;
  ctx->logical_count = 0;
  ctx->counter = 0;

  // Initialize variables hash table
  ctx->variables = malloc(sizeof(bennet_hash_table(cn_sym, cn_base_type)));
  if (!ctx->variables) {
    free(ctx);
    return NULL;
  }
  bennet_hash_table_init(cn_sym, cn_base_type)(
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
  bennet_hash_table_free(cn_sym, cn_base_type)(ctx->variables);
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
  bennet_hash_table_clear(cn_sym, cn_base_type)(ctx->variables);

  // Reset counter
  ctx->counter = 0;
}

// Resource constraint management
cn_resource_constraint* cn_resource_constraint_create_predicate(
    size_t bytes, cn_term* pointer) {
  assert(pointer);

  cn_resource_constraint* constraint = malloc(sizeof(cn_resource_constraint));
  assert(constraint);

  constraint->bytes = bytes;
  constraint->pointer = pointer;
  constraint->next = NULL;

  return constraint;
}

bool cn_context_add_resource_constraint(
    cn_constraint_context* ctx, cn_resource_constraint* constraint) {
  if (!ctx || !constraint) {
    return false;
  }

  assert(constraint->pointer->base_type.tag == CN_BASE_LOC);

  // Add to front of linked list
  constraint->next = ctx->resource_constraints;
  ctx->resource_constraints = constraint;
  ctx->resource_count++;

  return true;
}

void cn_resource_constraint_destroy(cn_resource_constraint* constraint) {
  if (!constraint) {
    return;
  }

  free(constraint);
}

// Logical constraint management
cn_logical_constraint* cn_logical_constraint_create_term(cn_term* term) {
  if (!term)
    return NULL;

  cn_logical_constraint* constraint = malloc(sizeof(cn_logical_constraint));
  if (!constraint)
    return NULL;

  constraint->type = CN_LOGICAL_TERM;
  constraint->data.term = term;
  constraint->next = NULL;

  return constraint;
}

cn_logical_constraint* cn_logical_constraint_create_forall(
    cn_sym var_name, cn_base_type var_type, cn_term* body) {
  if (!var_name.name || !body)
    return NULL;

  cn_logical_constraint* constraint = malloc(sizeof(cn_logical_constraint));
  if (!constraint)
    return NULL;

  constraint->type = CN_LOGICAL_FORALL;
  constraint->data.forall.var_name = var_name;
  constraint->data.forall.var_type = var_type;
  constraint->data.forall.body = body;
  constraint->next = NULL;

  return constraint;
}

bool cn_context_add_logical_constraint(
    cn_constraint_context* ctx, cn_logical_constraint* constraint) {
  if (!ctx || !constraint)
    return false;

  // Add to front of linked list
  constraint->next = ctx->logical_constraints;
  ctx->logical_constraints = constraint;
  ctx->logical_count++;

  return true;
}

void cn_logical_constraint_destroy(cn_logical_constraint* constraint) {
  if (!constraint)
    return;

  if (constraint->type == CN_LOGICAL_FORALL) {
  }

  free(constraint);
}

// Variable context management
bool cn_context_add_variable(cn_constraint_context* ctx, cn_sym var, cn_base_type type) {
  if (!ctx || !ctx->variables)
    return false;

  bennet_hash_table_set(cn_sym, cn_base_type)(ctx->variables, var, type);
  return true;
}

bennet_optional(cn_base_type)
    cn_context_get_variable_type(const cn_constraint_context* ctx, cn_sym var) {
  if (!ctx || !ctx->variables)
    return bennet_optional_none(cn_base_type);

  return bennet_hash_table_get(cn_sym, cn_base_type)(ctx->variables, var);
}

bool cn_context_has_variable(const cn_constraint_context* ctx, cn_sym var) {
  if (!ctx || !ctx->variables)
    return false;

  return bennet_hash_table_contains(cn_sym, cn_base_type)(ctx->variables, var);
}

size_t cn_context_variable_count(const cn_constraint_context* ctx) {
  if (!ctx || !ctx->variables)
    return 0;

  return bennet_hash_table_size(cn_sym, cn_base_type)(ctx->variables);
}

void cn_context_foreach_variable(
    const cn_constraint_context* ctx, cn_variable_callback callback, void* user_data) {
  if (!ctx || !ctx->variables || !callback)
    return;

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
      printf("    [%d] Bytes: %zu\n", i, resource->bytes);
      printf("         Pointer: %s\n", cn_term_to_string(resource->pointer));

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
