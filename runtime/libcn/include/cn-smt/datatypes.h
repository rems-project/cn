#ifndef CN_SMT_DATATYPES_H
#define CN_SMT_DATATYPES_H

#include <bennet/utils/hash_table.h>
#include <bennet/utils/vector.h>
#include <cn-smt/structs.h>
#include <cn-smt/terms.h>

// Forward declare pointer types for datatype constructor operations
// (void_ptr is already declared in structs.h)
// (bennet_vector(void_ptr) is declared in solver.h)
// (cn_datatype_constructor_fn is declared in solver.h)

/**
 * @brief Function pointer type for datatype destructor.
 *
 * Given a constructor name and a datatype value, returns an array of member pointers
 * if the value matches the constructor, or NULL if it doesn't match.
 *
 * @param constructor_name Name of the constructor to check (e.g., "Nil", "Cons")
 * @param value Pointer to the datatype value
 * @return Array of void* pointers to members if match, NULL otherwise
 */
typedef void** (*cn_datatype_destructor_fn)(const char*, void*);

// Hash table and optional declarations for (const_char_ptr, void_ptr) are in structs.h
// We use them here without redeclaring

/**
 * @brief Metadata for a single field in a datatype constructor.
 */
typedef struct {
  const char* label;       // Field label/name
  cn_base_type base_type;  // Field base type
} cn_datatype_field_info;

/**
 * @brief Complete data for a datatype constructor.
 *
 * Contains both the constructor function pointer and metadata about its fields.
 */
typedef struct {
  cn_datatype_constructor_fn fn;   // Constructor function
  cn_datatype_field_info* fields;  // Field metadata for parsing
  size_t field_count;              // Number of fields
} cn_datatype_constructor_data;

/**
 * @brief Datatype data structure containing all constructors for a datatype.
 *
 * This structure holds a hash table mapping constructor names to their
 * corresponding constructor data (function + metadata) for a specific datatype.
 */
typedef struct {
  /**
   * @brief Hash table mapping constructor names to constructor data.
   * Key: const char* (constructor name, e.g., "Nil", "Cons")
   * Value: void_ptr (actually cn_datatype_constructor_data* cast to void*)
   */
  bennet_hash_table(const_char_ptr, void_ptr) constructors;

  /**
   * @brief Destructor function for pattern matching.
   * Takes a constructor name and value, returns member array or NULL.
   */
  cn_datatype_destructor_fn destructor;
} cn_datatype_data;

/**
 * @brief Register a constructor function for a datatype.
 *
 * @param datatype_name Name of the datatype (e.g., "IntList")
 * @param constructor_name Name of the constructor (e.g., "Nil", "Cons")
 * @param constructor_fn Function pointer to the constructor implementation
 * @param fields Array of field metadata (can be NULL if field_count is 0)
 * @param field_count Number of fields in the constructor
 */
void cn_register_datatype_constructor(const char* datatype_name,
    const char* constructor_name,
    cn_datatype_constructor_fn constructor_fn,
    cn_datatype_field_info* fields,
    size_t field_count);

/**
 * @brief Apply a datatype constructor with given arguments.
 *
 * Looks up the constructor function for the given datatype and constructor name,
 * and calls it with the provided arguments.
 *
 * @param datatype_name Name of the datatype (e.g., "IntList")
 * @param constructor_name Name of the constructor (e.g., "Nil", "Cons")
 * @param args Vector of constructor arguments (already evaluated to void*)
 * @return Pointer to the constructed datatype instance
 */
void* cn_smt_datatype_apply_constructor(const char* datatype_name,
    const char* constructor_name,
    bennet_vector(void_ptr) * args);

/**
 * @brief Get datatype data for a given datatype name.
 *
 * @param datatype_name Name of the datatype
 * @return Pointer to the datatype data structure
 */
cn_datatype_data* cn_get_datatype_data(const char* datatype_name);

/**
 * @brief Get constructor data for a specific constructor.
 *
 * @param datatype_name Name of the datatype
 * @param constructor_name Name of the constructor
 * @return Pointer to the constructor data structure (function + field metadata)
 */
cn_datatype_constructor_data* cn_get_datatype_constructor_data(
    const char* datatype_name, const char* constructor_name);

/**
 * @brief Check if a datatype is registered.
 *
 * @param datatype_name Name of the datatype
 * @return True if the datatype is registered, false otherwise
 */
bool cn_datatype_exists(const char* datatype_name);

/**
 * @brief Register a destructor function for a datatype.
 *
 * @param datatype_name Name of the datatype
 * @param destructor_fn Destructor function pointer
 */
void cn_register_datatype_destructor(
    const char* datatype_name, cn_datatype_destructor_fn destructor_fn);

/**
 * @brief Get the destructor function for a datatype.
 *
 * @param datatype_name Name of the datatype
 * @return Destructor function pointer
 */
cn_datatype_destructor_fn cn_get_datatype_destructor(const char* datatype_name);

/**
 * @brief Reset the datatype registry.
 */
void cn_smt_datatype_registry_reset(void);

#endif  // CN_SMT_DATATYPES_H
