#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <bennet/utils/vector.h>
#include <cn-smt/datatypes.h>
#include <cn-smt/memory/test_alloc.h>
#include <cn-smt/terms.h>

// Global registry of all datatype data (mapping datatype name to datatype data)
static bennet_hash_table(const_char_ptr, void_ptr) g_datatype_registry;
static bool g_datatype_registry_initialized = false;

// Initialize the global datatype registry
static void init_datatype_registry(void) {
  if (!g_datatype_registry_initialized) {
    bennet_hash_table_init(const_char_ptr, void_ptr)(
        &g_datatype_registry, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);
    g_datatype_registry_initialized = true;
  }
}

// Get or create datatype data for a given datatype name
static cn_datatype_data* get_or_create_datatype_data(const char* datatype_name) {
  assert(datatype_name);
  init_datatype_registry();

  // Check if datatype already exists
  bennet_optional(void_ptr) opt = bennet_hash_table_get(const_char_ptr, void_ptr)(
      &g_datatype_registry, datatype_name);

  if (bennet_optional_is_some(opt)) {
    return (cn_datatype_data*)bennet_optional_unwrap(opt);
  }

  // Create new datatype data
  cn_datatype_data* data = cn_test_malloc(sizeof(cn_datatype_data));
  assert(data);

  // Initialize the constructors hash table
  bennet_hash_table_init(const_char_ptr, void_ptr)(
      &data->constructors, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

  // Initialize destructor to NULL
  data->destructor = NULL;

  // Register the datatype
  bennet_hash_table_set(const_char_ptr, void_ptr)(
      &g_datatype_registry, datatype_name, (void_ptr)data);

  return data;
}

// Register a constructor function for a datatype
void cn_register_datatype_constructor(const char* datatype_name,
    const char* constructor_name,
    cn_datatype_constructor_fn constructor_fn,
    cn_datatype_field_info* fields,
    size_t field_count) {
  assert(datatype_name);
  assert(constructor_name);
  assert(constructor_fn);

  cn_datatype_data* datatype_data = get_or_create_datatype_data(datatype_name);

  // Allocate constructor data
  cn_datatype_constructor_data* ctor_data =
      cn_test_malloc(sizeof(cn_datatype_constructor_data));
  assert(ctor_data);

  ctor_data->fn = constructor_fn;
  ctor_data->field_count = field_count;

  // Copy field metadata if provided
  if (field_count > 0 && fields) {
    ctor_data->fields = cn_test_malloc(sizeof(cn_datatype_field_info) * field_count);
    assert(ctor_data->fields);
    memcpy(ctor_data->fields, fields, sizeof(cn_datatype_field_info) * field_count);
  } else {
    ctor_data->fields = NULL;
  }

  // Add constructor data to the datatype's constructor table
  bennet_hash_table_set(const_char_ptr, void_ptr)(
      &datatype_data->constructors, constructor_name, (void_ptr)ctor_data);
}

// Get datatype data for a given datatype name
cn_datatype_data* cn_get_datatype_data(const char* datatype_name) {
  assert(datatype_name);
  init_datatype_registry();

  bennet_optional(void_ptr) opt = bennet_hash_table_get(const_char_ptr, void_ptr)(
      &g_datatype_registry, datatype_name);

  if (bennet_optional_is_none(opt)) {
    fprintf(stderr, "\nInvalid datatype lookup: %s\n", datatype_name);
    assert(bennet_optional_is_some(opt));
  }

  return (cn_datatype_data*)bennet_optional_unwrap(opt);
}

// Get constructor data for a specific constructor
cn_datatype_constructor_data* cn_get_datatype_constructor_data(
    const char* datatype_name, const char* constructor_name) {
  assert(datatype_name);
  assert(constructor_name);

  // Get datatype data
  cn_datatype_data* datatype_data = cn_get_datatype_data(datatype_name);
  assert(datatype_data);

  // Look up constructor data
  bennet_optional(void_ptr) ctor_opt = bennet_hash_table_get(const_char_ptr, void_ptr)(
      &datatype_data->constructors, constructor_name);

  if (bennet_optional_is_none(ctor_opt)) {
    fprintf(stderr,
        "\nInvalid constructor lookup: %s::%s\n",
        datatype_name,
        constructor_name);
    assert(bennet_optional_is_some(ctor_opt));
  }

  return (cn_datatype_constructor_data*)bennet_optional_unwrap(ctor_opt);
}

// Apply a datatype constructor with given arguments
void* cn_smt_datatype_apply_constructor(const char* datatype_name,
    const char* constructor_name,
    bennet_vector(void_ptr) * args) {
  assert(datatype_name);
  assert(constructor_name);
  assert(args);

  // Get constructor data (includes function and field metadata)
  cn_datatype_constructor_data* ctor_data =
      cn_get_datatype_constructor_data(datatype_name, constructor_name);
  assert(ctor_data);
  assert(ctor_data->fn);

  // Call the constructor function with arguments
  return ctor_data->fn(args);
}

// Check if a datatype is registered
bool cn_datatype_exists(const char* datatype_name) {
  assert(datatype_name);
  init_datatype_registry();
  return bennet_hash_table_contains(const_char_ptr, void_ptr)(
      &g_datatype_registry, datatype_name);
}

// Register a destructor function for a datatype
void cn_register_datatype_destructor(
    const char* datatype_name, cn_datatype_destructor_fn destructor_fn) {
  assert(datatype_name);
  assert(destructor_fn);

  // Get or create the datatype data
  cn_datatype_data* datatype_data = get_or_create_datatype_data(datatype_name);
  assert(datatype_data);

  // Set the destructor function
  datatype_data->destructor = destructor_fn;
}

// Get the destructor function for a datatype
cn_datatype_destructor_fn cn_get_datatype_destructor(const char* datatype_name) {
  assert(datatype_name);

  // Get the datatype data
  cn_datatype_data* datatype_data = cn_get_datatype_data(datatype_name);
  assert(datatype_data);
  assert(datatype_data->destructor);  // Destructor must be registered

  return datatype_data->destructor;
}

// Reset the datatype registry
void cn_smt_datatype_registry_reset(void) {
  bennet_hash_table_free(const_char_ptr, void_ptr)(&g_datatype_registry);
}
