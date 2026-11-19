#ifndef CN_SMT_STRUCTS_H
#define CN_SMT_STRUCTS_H

#include <bennet/utils/hash_table.h>
#include <bennet/utils/vector.h>
#include <cn-smt/eval.h>
#include <cn-smt/solver.h>
#include <cn-smt/terms.h>

// Forward declare pointer types for struct operations
typedef void* void_ptr;

// Optional and hash table declarations for member mapping (const_char_ptr to void_ptr)
BENNET_OPTIONAL_DECL(void_ptr);
BENNET_HASH_TABLE_DECL(const_char_ptr, void_ptr);
BENNET_HASH_TABLE_IMPL(const_char_ptr, void_ptr);

// Vector type declarations for struct member information
BENNET_VECTOR_DECL(struct_member_t)
BENNET_VECTOR_IMPL(struct_member_t)

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

  /**
   * @brief Function pointer for creating default struct values.
   * @return Pointer to a struct with default member values.
   */
  void* (*default_struct)(void);

  // Member information
  bennet_vector(struct_member_t) members;
} cn_struct_data;

// Hash table and optional declarations for unified struct data are already in eval.h

void cn_register_struct(const char* struct_name, const cn_struct_data* struct_data);

// Create a default struct value for a given struct type name
void* cn_smt_struct_default(const char* struct_tag);

// Create struct from member values
void* cn_smt_struct_create(
    const char* struct_tag, bennet_hash_table(const_char_ptr, void_ptr) * member_values);

// Get struct member value
void* cn_smt_struct_get_member(
    const char* struct_tag, void* struct_val, const char* member_name);

// Update struct member value
void* cn_smt_struct_update_member(
    const char* struct_tag, void* struct_val, const char* member_name, void* new_val);

// Get struct data for a given struct type name
cn_struct_data* cn_get_struct_data(const char* struct_name);

// Check if struct type is registered
bool cn_struct_type_exists(const char* struct_name);

// Reset the struct registry
void cn_smt_struct_registry_reset(void);

#endif  // CN_SMT_STRUCTS_H
