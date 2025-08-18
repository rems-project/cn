#ifndef CN_SMT_CONTEXT_H
#define CN_SMT_CONTEXT_H

#include <stdbool.h>
#include <stddef.h>

#include <bennet/utils/vector.h>
#include <cn-smt/subst.h>
#include <cn-smt/terms.h>

// Declare vector type for cn_sym
BENNET_VECTOR_DECL(cn_sym)

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations
typedef struct cn_constraint_context cn_constraint_context;
typedef struct cn_resource_constraint cn_resource_constraint;
typedef struct cn_logical_constraint cn_logical_constraint;

// Resource constraint
typedef struct cn_resource_constraint {
  cn_term* pointer;                     // Address
  size_t bytes;                         // Number of bytes owned
  struct cn_resource_constraint* next;  // Linked list
} cn_resource_constraint;

// Logical constraint types
typedef enum {
  CN_LOGICAL_TERM,   // T constructor - simple index term
  CN_LOGICAL_FORALL  // Forall constructor - quantified constraint
} cn_logical_type;

// Logical constraint structure
typedef struct cn_logical_constraint {
  cn_logical_type type;
  union {
    cn_term* term;  // For CN_LOGICAL_TERM
    struct {        // For CN_LOGICAL_FORALL
      cn_sym var_name;
      cn_base_type var_type;
      cn_term* body;
    } forall;
  } data;
  struct cn_logical_constraint* next;  // Linked list
} cn_logical_constraint;

// Main constraint context structure
typedef struct cn_constraint_context {
  // Variable context - maps cn_sym to cn_base_type
  bennet_hash_table(cn_sym, cn_base_type) * variables;

  cn_resource_constraint* resource_constraints;
  cn_logical_constraint* logical_constraints;
  size_t resource_count;
  size_t logical_count;

  // Counter for generating unique symbolic variable names
  uint64_t counter;
} cn_constraint_context;

// Context management functions
cn_constraint_context* cn_context_create(void);
void cn_context_destroy(cn_constraint_context* ctx);
void cn_context_clear(cn_constraint_context* ctx);

// Variable context management
bool cn_context_add_variable(cn_constraint_context* ctx, cn_sym var, cn_base_type type);
bennet_optional(cn_base_type)
    cn_context_get_variable_type(const cn_constraint_context* ctx, cn_sym var);
bool cn_context_has_variable(const cn_constraint_context* ctx, cn_sym var);
size_t cn_context_variable_count(const cn_constraint_context* ctx);

// Resource constraint management
cn_resource_constraint* cn_resource_constraint_create_predicate(
    size_t bytes, cn_term* pointer);

bool cn_context_add_resource_constraint(
    cn_constraint_context* ctx, cn_resource_constraint* constraint);

void cn_resource_constraint_destroy(cn_resource_constraint* constraint);

// Logical constraint management
cn_logical_constraint* cn_logical_constraint_create_term(cn_term* term);

cn_logical_constraint* cn_logical_constraint_create_forall(
    cn_sym var_name, cn_base_type var_type, cn_term* body);

bool cn_context_add_logical_constraint(
    cn_constraint_context* ctx, cn_logical_constraint* constraint);

void cn_logical_constraint_destroy(cn_logical_constraint* constraint);

// Query functions
size_t cn_context_resource_count(const cn_constraint_context* ctx);
size_t cn_context_logical_count(const cn_constraint_context* ctx);

// Iterator functions
const cn_resource_constraint* cn_context_first_resource(const cn_constraint_context* ctx);
const cn_logical_constraint* cn_context_first_logical(const cn_constraint_context* ctx);

// Variable iteration function
typedef void (*cn_variable_callback)(cn_sym var, cn_base_type type, void* user_data);
void cn_context_foreach_variable(
    const cn_constraint_context* ctx, cn_variable_callback callback, void* user_data);

// Debug/printing functions (optional)
void cn_context_print_summary(const cn_constraint_context* ctx);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_CONTEXT_H
