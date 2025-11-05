#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <cn-smt/eval.h>
#include <cn-smt/memory/test_alloc.h>
#include <cn-smt/records.h>
#include <cn-smt/solver.h>

// Hash and equality functions for record_hash_t
static inline size_t bennet_hash_record_hash_t(record_hash_t hash) {
  return (size_t)hash;
}

static inline bool bennet_eq_record_hash_t(record_hash_t a, record_hash_t b) {
  return a == b;
}

// Global registry of all record data (hash-based, storing pointers)
static bennet_hash_table(record_hash_t, void_ptr) g_record_data;
static bool g_record_data_initialized = false;

// Initialize the global record data registry
static void init_record_data(void) {
  if (!g_record_data_initialized) {
    bennet_hash_table_init(record_hash_t, void_ptr)(
        &g_record_data, bennet_hash_record_hash_t, bennet_eq_record_hash_t);
    g_record_data_initialized = true;
  }
}

// Simple hash function for member composition
// This creates a hash based on member names and types
record_hash_t cn_record_member_hash(
    size_t count, const char** member_names, cn_base_type* member_types) {
  record_hash_t hash = 0;

  // Hash based on member count
  hash = hash * 31 + count;

  // Hash each member name and type
  for (size_t i = 0; i < count; i++) {
    // Hash member name
    const char* name = member_names[i];
    while (*name) {
      hash = hash * 31 + (record_hash_t)*name;
      name++;
    }

    // Hash member type (simplified - just use the tag)
    hash = hash * 31 + (record_hash_t)member_types[i].tag;

    // For bits types, also hash the signedness and size
    if (member_types[i].tag == CN_BASE_BITS) {
      hash = hash * 31 + (record_hash_t)member_types[i].data.bits.is_signed;
      hash = hash * 31 + (record_hash_t)member_types[i].data.bits.size_bits;
    }
  }

  return hash;
}

// Register record with hash-based lookup
void cn_register_record(record_hash_t record_hash, const cn_record_data* record_data) {
  assert(record_data);
  assert(record_data->create_record);
  assert(record_data->default_record);
  assert(record_data->get_member);
  assert(record_data->update_member);

  init_record_data();

  // Allocate record data on heap to get stable pointer and copy the input data
  cn_record_data* data = cn_test_malloc(sizeof(cn_record_data));
  *data = *record_data;  // Copy all function pointers

  // Initialize and copy the members vector (deep copy)
  bennet_vector_init(struct_member_t)(&data->members);
  // Cast away const for vector access (we're only reading the data)
  bennet_vector(struct_member_t)* src_members =
      (bennet_vector(struct_member_t)*)&record_data->members;
  for (size_t i = 0; i < bennet_vector_size(struct_member_t)(src_members); i++) {
    struct_member_t* member = bennet_vector_get(struct_member_t)(src_members, i);
    bennet_vector_push(struct_member_t)(&data->members, *member);
  }

  bennet_hash_table_set(record_hash_t, void_ptr)(
      &g_record_data, record_hash, (void_ptr)data);
}

// Get record data for a given record type hash
cn_record_data* cn_get_record_data(record_hash_t record_hash) {
  init_record_data();
  bennet_optional(void_ptr) opt =
      bennet_hash_table_get(record_hash_t, void_ptr)(&g_record_data, record_hash);
  if (bennet_optional_is_none(opt)) {
    fprintf(stderr, "\nInvalid record lookup: hash %zu\n", record_hash);
    assert(bennet_optional_is_some(opt));
  }

  return (cn_record_data*)bennet_optional_unwrap(opt);
}

// Create a default record value for a given record type hash
void* cn_smt_record_default(record_hash_t record_hash) {
  cn_record_data* data = cn_get_record_data(record_hash);
  assert(data);
  assert(data->default_record);
  return data->default_record();
}

// Create record from member values
void* cn_smt_record_create(record_hash_t record_hash,
    bennet_hash_table(const_char_ptr, void_ptr) * member_values) {
  assert(member_values);
  cn_record_data* data = cn_get_record_data(record_hash);
  assert(data);
  assert(data->create_record);
  return data->create_record(member_values);
}

// Get record member value
void* cn_smt_record_get_member(
    record_hash_t record_hash, void* record_val, const char* member_name) {
  assert(record_val);
  assert(member_name);
  cn_record_data* data = cn_get_record_data(record_hash);
  assert(data);
  assert(data->get_member);
  return data->get_member(record_val, member_name);
}

// Update record member value
void* cn_smt_record_update_member(
    record_hash_t record_hash, void* record_val, const char* member_name, void* new_val) {
  assert(record_val);
  assert(member_name);
  assert(new_val);
  cn_record_data* data = cn_get_record_data(record_hash);
  assert(data);
  assert(data->update_member);
  return data->update_member(record_val, member_name, new_val);
}

// Check if record type is registered
bool cn_record_type_exists(record_hash_t record_hash) {
  init_record_data();
  return bennet_hash_table_contains(record_hash_t, void_ptr)(&g_record_data, record_hash);
}

// Reset the record registry
void cn_smt_record_registry_reset(void) {
  g_record_data_initialized = false;
}
