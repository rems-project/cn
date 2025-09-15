#ifndef CN_SMT_EVAL_H
#define CN_SMT_EVAL_H

#include <stdbool.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <cn-smt/context.h>
#include <cn-smt/terms.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declare pointer types for struct operations
typedef void* void_ptr;

// Optional and hash table declarations for member mapping (const_char_ptr to void_ptr)
BENNET_OPTIONAL_DECL(void_ptr);
BENNET_HASH_TABLE_DECL(const_char_ptr, void_ptr);
BENNET_HASH_TABLE_IMPL(const_char_ptr, void_ptr);

/**
 * @brief Struct handler containing function pointers for struct operations.
 *
 * This structure defines the interface for handling struct creation, member access,
 * and member updates in the CN evaluation system. Each struct type can register
 * its own implementation of these operations.
 */
typedef struct {
  /**
   * @brief Function pointer for creating structs from member hash table.
   * @param members Hash table mapping member names to their values.
   * @return Pointer to the created struct.
   */
  void* (*create_struct)(bennet_hash_table(const_char_ptr, void_ptr) * members);

  /**
   * @brief Function pointer for getting struct member value.
   * @param struct_val Pointer to the struct.
   * @param member_name Name of the member to retrieve.
   * @return Pointer to the member value.
   */
  void* (*get_member)(void* struct_val, const char* member_name);

  /**
   * @brief Function pointer for updating struct member value.
   * @param struct_val Pointer to the struct.
   * @param member_name Name of the member to update.
   * @param new_value New value for the member.
   * @return Pointer to the updated struct (may be a new struct instance).
   */
  void* (*update_member)(void* struct_val, const char* member_name, void* new_value);
} cn_struct_handler;

// Hash table and optional declarations for cn_struct_handler
BENNET_HASH_TABLE_DECL(const_char_ptr, cn_struct_handler)
BENNET_OPTIONAL_DECL(cn_struct_handler);

/**
 * @brief Registers a struct handler for a given struct type name.
 *
 * This function allows registering custom handlers for struct operations.
 * Once registered, the evaluator will use these handlers when evaluating
 * CN_TERM_STRUCT, CN_TERM_STRUCT_MEMBER, and CN_TERM_STRUCT_UPDATE terms.
 *
 * @param struct_name The name/tag of the struct type.
 * @param handler The struct handler containing function pointers.
 */
void cn_register_struct_handler(const char* struct_name, cn_struct_handler handler);

/**
 * @brief Evaluates a cn_term and returns the result as a Fulminate value.
 *
 * This function walks the cn_term AST and computes a concrete value using the
 * Fulminate runtime functions. The caller is responsible for knowing the
 * expected return type and casting the void* pointer accordingly.
 *
 * @param term The cn_term to evaluate.
 * @return A pointer to a Fulminate value (e.g., cn_integer*, cn_bool*), or NULL
 *         if the term cannot be evaluated.
 */
void* cn_eval_term(cn_term* term);

/**
 * @brief Evaluates a constraint context and returns whether it is satisfiable.
 *
 * This function evaluates all logical constraints in the context and determines
 * if they are collectively satisfied. For resource constraints, it checks that
 * the constraints are consistent (e.g., no overlapping memory regions).
 *
 * @param ctx The constraint context to evaluate.
 * @return true if the context is satisfiable (all constraints are satisfied),
 *         false otherwise.
 */
bool cn_eval_context(cn_constraint_context* ctx);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_EVAL_H
