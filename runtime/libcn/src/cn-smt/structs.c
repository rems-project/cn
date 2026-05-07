#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <cn-smt/eval.h>
#include <cn-smt/memory/test_alloc.h>
#include <cn-smt/solver.h>
#include <cn-smt/structs.h>

// Hash and equality functions for void_ptr (pointer comparison)
__attribute__((unused)) static inline size_t bennet_hash_void_ptr(void_ptr ptr) {
  return (size_t)ptr;
}

__attribute__((unused)) static inline bool bennet_eq_void_ptr(void_ptr a, void_ptr b) {
  return a == b;
}

// String hash and equality functions are already defined in terms.h
// Hash table implementation for (const_char_ptr, void_ptr) is already in eval.h

// Global registry of all struct data (unified, storing pointers)
static bennet_hash_table(const_char_ptr, void_ptr) g_struct_data;
static bool g_struct_data_initialized = false;

// Initialize the global struct data registry
static void init_struct_data(void) {
  if (!g_struct_data_initialized) {
    bennet_hash_table_init(const_char_ptr, void_ptr)(
        &g_struct_data, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);
    g_struct_data_initialized = true;
  }
}

// Unified struct registration function
void cn_register_struct(const char* struct_name, const cn_struct_data* struct_data) {
  assert(struct_name);
  assert(struct_data);
  assert(struct_data->create_struct);
  assert(struct_data->default_struct);
  assert(struct_data->get_member);
  assert(struct_data->update_member);

  init_struct_data();

  // Allocate struct data on heap to get stable pointer and copy the input data
  cn_struct_data* data = cn_test_malloc(sizeof(cn_struct_data));
  *data = *struct_data;  // Copy all function pointers

  // Initialize and copy the members vector (deep copy)
  bennet_vector_init(struct_member_t)(&data->members);
  // Cast away const for vector access (we're only reading the data)
  bennet_vector(struct_member_t)* src_members =
      (bennet_vector(struct_member_t)*)&struct_data->members;
  for (size_t i = 0; i < bennet_vector_size(struct_member_t)(src_members); i++) {
    struct_member_t* member = bennet_vector_get(struct_member_t)(src_members, i);
    bennet_vector_push(struct_member_t)(&data->members, *member);
  }

  bennet_hash_table_set(const_char_ptr, void_ptr)(
      &g_struct_data, struct_name, (void_ptr)data);
}

// Get struct data for a given struct type name
cn_struct_data* cn_get_struct_data(const char* struct_name) {
  init_struct_data();
  bennet_optional(void_ptr) opt =
      bennet_hash_table_get(const_char_ptr, void_ptr)(&g_struct_data, struct_name);
  if (bennet_optional_is_none(opt)) {
    fprintf(stderr, "\nInvalid struct lookup: %s\n", struct_name);
    assert(bennet_optional_is_some(opt));
  }

  return (cn_struct_data*)bennet_optional_unwrap(opt);
}

// Create a default struct value for a given struct type name
void* cn_smt_struct_default(const char* struct_tag) {
  assert(struct_tag);
  cn_struct_data* data = cn_get_struct_data(struct_tag);
  assert(data);
  assert(data->default_struct);
  return data->default_struct();
}

// Create struct from member values
void* cn_smt_struct_create(
    const char* struct_tag, bennet_hash_table(const_char_ptr, void_ptr) * member_values) {
  assert(struct_tag);
  assert(member_values);
  cn_struct_data* data = cn_get_struct_data(struct_tag);
  assert(data);
  assert(data->create_struct);
  return data->create_struct(member_values);
}

// Get struct member value
void* cn_smt_struct_get_member(
    const char* struct_tag, void* struct_val, const char* member_name) {
  assert(struct_tag);
  assert(struct_val);
  assert(member_name);
  cn_struct_data* data = cn_get_struct_data(struct_tag);
  assert(data);
  assert(data->get_member);
  return data->get_member(struct_val, member_name);
}

// Update struct member value
void* cn_smt_struct_update_member(
    const char* struct_tag, void* struct_val, const char* member_name, void* new_val) {
  assert(struct_tag);
  assert(struct_val);
  assert(member_name);
  assert(new_val);
  cn_struct_data* data = cn_get_struct_data(struct_tag);
  assert(data);
  assert(data->update_member);
  return data->update_member(struct_val, member_name, new_val);
}

// Check if struct type is registered
bool cn_struct_type_exists(const char* struct_name) {
  assert(struct_name);
  init_struct_data();
  return bennet_hash_table_contains(const_char_ptr, void_ptr)(
      &g_struct_data, struct_name);
}

// Reset the struct registry
void cn_smt_struct_registry_reset(void) {
  bennet_hash_table_free(const_char_ptr, void_ptr)(&g_struct_data);
}
