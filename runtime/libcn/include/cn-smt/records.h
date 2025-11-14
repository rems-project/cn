#ifndef CN_SMT_RECORDS_H
#define CN_SMT_RECORDS_H

#include <bennet/utils/hash_table.h>
#include <bennet/utils/vector.h>
#include <cn-smt/eval.h>
#include <cn-smt/solver.h>
#include <cn-smt/terms.h>

// Forward declare pointer types for record operations
typedef void* void_ptr;
typedef size_t record_hash_t;

// Hash table declarations for record registry (record_hash_t to void_ptr)
BENNET_OPTIONAL_DECL(record_hash_t);
BENNET_HASH_TABLE_DECL(record_hash_t, void_ptr);
BENNET_HASH_TABLE_IMPL(record_hash_t, void_ptr);

/**
 * @brief Record handler containing function pointers for record operations.
 *
 * This structure defines the interface for handling record creation, member access,
 * and member updates in the CN evaluation system. Each record type can register
 * its own implementation of these operations.
 */
typedef struct {
  /**
   * @brief Function pointer for creating records from member hash table.
   * @param members Hash table mapping member names to their values.
   * @return Pointer to the created record.
   */
  void* (*create_record)(bennet_hash_table(const_char_ptr, void_ptr) * members);

  /**
   * @brief Function pointer for getting record member value.
   * @param record_val Pointer to the record.
   * @param member_name Name of the member to retrieve.
   * @return Pointer to the member value.
   */
  void* (*get_member)(void* record_val, const char* member_name);

  /**
   * @brief Function pointer for updating record member value.
   * @param record_val Pointer to the record.
   * @param member_name Name of the member to update.
   * @param new_value New value for the member.
   * @return Pointer to the updated record (may be a new record instance).
   */
  void* (*update_member)(void* record_val, const char* member_name, void* new_value);

  /**
   * @brief Function pointer for creating default record values.
   * @return Pointer to a record with default member values.
   */
  void* (*default_record)(void);

  // Member information
  bennet_vector(struct_member_t) members;
} cn_record_data;

/**
 * @brief Generate hash for record type based on member composition.
 * @param count Number of members.
 * @param member_names Array of member names.
 * @param member_types Array of member base types.
 * @return Hash value uniquely identifying this record type.
 */
record_hash_t cn_record_member_hash(
    size_t count, const char** member_names, cn_base_type* member_types);

/**
 * @brief Register a record type with its handler functions.
 * @param record_hash Hash identifying the record type.
 * @param record_data Pointer to the record handler data.
 */
void cn_register_record(record_hash_t record_hash, const cn_record_data* record_data);

/**
 * @brief Create a default record value for a given record type hash.
 * @param record_hash Hash identifying the record type.
 * @return Pointer to a record with default member values.
 */
void* cn_smt_record_default(record_hash_t record_hash);

/**
 * @brief Create record from member values.
 * @param record_hash Hash identifying the record type.
 * @param member_values Hash table mapping member names to values.
 * @return Pointer to the created record.
 */
void* cn_smt_record_create(record_hash_t record_hash,
    bennet_hash_table(const_char_ptr, void_ptr) * member_values);

/**
 * @brief Get record member value.
 * @param record_hash Hash identifying the record type.
 * @param record_val Pointer to the record.
 * @param member_name Name of the member to retrieve.
 * @return Pointer to the member value.
 */
void* cn_smt_record_get_member(
    record_hash_t record_hash, void* record_val, const char* member_name);

/**
 * @brief Update record member value.
 * @param record_hash Hash identifying the record type.
 * @param record_val Pointer to the record.
 * @param member_name Name of the member to update.
 * @param new_val New value for the member.
 * @return Pointer to the updated record.
 */
void* cn_smt_record_update_member(
    record_hash_t record_hash, void* record_val, const char* member_name, void* new_val);

/**
 * @brief Get record data for a given record type hash.
 * @param record_hash Hash identifying the record type.
 * @return Pointer to the record handler data.
 */
cn_record_data* cn_get_record_data(record_hash_t record_hash);

/**
 * @brief Check if record type is registered.
 * @param record_hash Hash identifying the record type.
 * @return True if the record type is registered, false otherwise.
 */
bool cn_record_type_exists(record_hash_t record_hash);

/**
 * @brief Reset the record registry.
 */
void cn_smt_record_registry_reset(void);

#endif  // CN_SMT_RECORDS_H
