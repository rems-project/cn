#ifndef CN_SMT_FUNCTIONS_H
#define CN_SMT_FUNCTIONS_H

#include <stdbool.h>
#include <stddef.h>

#include <cn-smt/solver.h>
#include <cn-smt/terms.h>
#include <cn-smt/to_smt.h>

#ifdef __cplusplus
extern "C" {
#endif

// Function handler type: takes array of void* arguments, returns void* result
typedef void* (*cn_func_handler)(void** args);

// Register a CN logical function with both its execution handler and SMT definition
// For uninterpreted functions (body == NULL), this will print an error and exit
void cn_register_func(cn_sym name,
    cn_func_handler handler,
    cn_arg_binder* arg_binders,
    size_t arg_count,
    cn_base_type res_bt,
    bool recursive,
    cn_term* body,
    struct cn_smt_solver* s);

// Get the execution handler for a registered function
cn_func_handler cn_get_func_handler(const char* func_name);

// Check if a function is registered
bool cn_func_exists(const char* func_name);

// Reset the function registry
void cn_smt_func_registry_reset(void);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_FUNCTIONS_H
