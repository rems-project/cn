#ifndef BENNET_DSL_H
#define BENNET_DSL_H

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <bennet/dsl/arbitrary.h>
#include <bennet/dsl/assert.h>
#include <bennet/dsl/assign.h>
#include <bennet/dsl/backtrack.h>
#include <bennet/dsl/specialized.h>
#include <bennet/internals/domain.h>
#include <bennet/state/checkpoint.h>
#include <bennet/state/failure.h>
#include <cn-smt/memory/std_alloc.h>
#include <cn-smt/terms.h>

#define BENNET_CHECK_TIMEOUT()                                                           \
  if (bennet_get_input_timeout() != 0 &&                                                 \
      bennet_get_milliseconds() - bennet_get_input_timer() >                             \
          bennet_get_input_timeout()) {                                                  \
    bennet_failure_reset();                                                              \
    bennet_failure_set_failure_type(BENNET_FAILURE_TIMEOUT);                             \
    goto bennet_label_bennet_backtrack;                                                  \
  }

#define BENNET_INIT()                                                                    \
  size_t bennet_rec_size = bennet_get_size();                                            \
  BENNET_INIT_SIZED();

#define BENNET_INIT_SIZED()                                                              \
  if (0) {                                                                               \
  bennet_label_bennet_backtrack:                                                         \
    bennet_decrement_depth();                                                            \
    return NULL;                                                                         \
  }                                                                                      \
  BENNET_CHECK_TIMEOUT();                                                                \
  bennet_increment_depth();                                                              \
  if (bennet_rec_size <= 0 || bennet_get_depth() == bennet_max_depth()) {                \
    if (has_depth_failures()) {                                                          \
      bennet_failure_set_failure_type(BENNET_FAILURE_DEPTH);                             \
      add_depth_failure();                                                               \
    } else {                                                                             \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
    }                                                                                    \
    goto bennet_label_bennet_backtrack;                                                  \
  }

#define BENNET_ARBITRARY(cn_ty, c_ty)                                                    \
  ({                                                                                     \
    bennet_domain(c_ty)* domain = bennet_domain_top(c_ty);                               \
    bennet_arbitrary_##cn_ty(domain);                                                    \
  })

#define BENNET_ARBITRARY_POINTER() BENNET_ARBITRARY(cn_pointer, uintptr_t)

#define BENNET_ARBITRARY_UNSIGNED(bits) BENNET_ARBITRARY(cn_bits_u##bits, uint##bits##_t)

#define BENNET_ARBITRARY_SIGNED(bits) BENNET_ARBITRARY(cn_bits_i##bits, int##bits##_t)

#define BENNET_SPECIALIZED(cn_ty,                                                        \
    lower_bound_ex,                                                                      \
    lower_bound_inc,                                                                     \
    upper_bound_inc,                                                                     \
    upper_bound_ex,                                                                      \
    last_var,                                                                            \
    ...)                                                                                 \
  ({                                                                                     \
    const char* vars = {__VA_ARGS__};                                                    \
    bennet_specialized_##cn_ty(                                                          \
        lower_bound_ex, lower_bound_inc, upper_bound_inc, upper_bound_ex, vars);         \
    if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                      \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  })

#define BENNET_CALL(ty, last_var, ...)                                                   \
  ({                                                                                     \
    ty* var = __VA_ARGS__;                                                               \
    if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                      \
      BENNET_CHECK_TIMEOUT();                                                            \
                                                                                         \
      if (bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH) {                   \
        bennet_failure_blame_many(path_vars);                                            \
      }                                                                                  \
                                                                                         \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
    var;                                                                                 \
  })

#define BENNET_ASSIGN(id,                                                                \
    ptr,                                                                                 \
    ptr_ty,                                                                              \
    addr,                                                                                \
    val_ty,                                                                              \
    value,                                                                               \
    last_var,                                                                            \
    addr_term,                                                                           \
    num_other_vars,                                                                      \
    other_var_ids,                                                                       \
    other_var_syms,                                                                      \
    ...)                                                                                 \
  {                                                                                      \
    val_ty value_redir = value;                                                          \
    const void* vars[] = {__VA_ARGS__};                                                  \
    if (bennet_assign(ptr_ty,                                                            \
            id,                                                                          \
            ptr,                                                                         \
            addr,                                                                        \
            &value_redir,                                                                \
            sizeof(val_ty),                                                              \
            vars,                                                                        \
            addr_term,                                                                   \
            num_other_vars,                                                              \
            other_var_ids,                                                               \
            other_var_syms)) {                                                           \
      bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                      \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
                                                                                         \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, false);                              \
  }

/*=============================================================================
 * BEGIN/REFINE/END macros for dynamic abstract interpretation
 *
 * These macros split the domain generation process into three phases:
 * 1. BEGIN: Initialize domain from static analysis
 * 2. REFINE: Apply constraint refinements (zero or more)
 * 3. END: Sample from refined domain, handle backtracking
 *===========================================================================*/

/* Initialize domain from static analysis result */
#define BENNET_LET_ARBITRARY_DOMAIN_BEGIN(cn_ty, c_ty, var, last_var, initial_domain)    \
  bool var##_restore_randomness = false;                                                 \
  bennet_checkpoint var##_checkpoint = bennet_checkpoint_save();                         \
  bennet_rand_checkpoint var##_rand_checkpoint_before = bennet_rand_save();              \
  bennet_rand_checkpoint var##_rand_checkpoint_after = NULL;                             \
                                                                                         \
  bennet_domain(c_ty)* var##_cs = initial_domain;                                        \
  bennet_domain(c_ty)* var##_cs_tmp = var##_cs;                                          \
  bennet_absint_state* var##_absint_state = NULL;                                        \
  (void)var##_absint_state; /* May be unused if no REFINE calls */                       \
                                                                                         \
  bennet_label_##var##_refine :;

#define BENNET_LET_ARBITRARY_DOMAIN_BEGIN_SIGNED(bits, var, last_var, domain)            \
  BENNET_LET_ARBITRARY_DOMAIN_BEGIN(cn_bits_i##bits, int##bits##_t, var, last_var, domain)

#define BENNET_LET_ARBITRARY_DOMAIN_BEGIN_UNSIGNED(bits, var, last_var, domain)          \
  BENNET_LET_ARBITRARY_DOMAIN_BEGIN(                                                     \
      cn_bits_u##bits, uint##bits##_t, var, last_var, domain)

#define BENNET_LET_ARBITRARY_DOMAIN_BEGIN_POINTER(var, last_var, domain)                 \
  BENNET_LET_ARBITRARY_DOMAIN_BEGIN(cn_pointer, uintptr_t, var, last_var, domain)

/* Refine domain using backward abstract interpretation
 * Parameters:
 *   c_ty: C type (e.g., int32_t)
 *   var: variable name being generated
 *   x_sym: cn_sym for the target variable
 *   x_bt: cn_base_type for the target variable
 *   constraint_expr: cn_term* boolean expression (constraint to satisfy)
 *   last_var: backtrack target if domain becomes bottom
 *
 * Uses bennet_domain_refine which operates at the product domain level:
 * extracts the wint component, runs backward assume, rebuilds the product.
 */
#define BENNET_REFINE_CONSTRAINT(c_ty, var, x_sym, x_bt, constraint_expr, last_var, ...) \
  {                                                                                      \
    BENNET_CHECK_TIMEOUT();                                                              \
    cn_base_type _refine_bt = (x_bt);                                                    \
    bennet_absint_sym _refine_sym = {.name = (x_sym).name, .id = (x_sym).id};            \
    bool _refine_is_bottom = false;                                                      \
    bennet_domain(c_ty)* _refine_result = bennet_domain_refine(                          \
        c_ty, var##_cs, _refine_sym, &_refine_bt, constraint_expr, &_refine_is_bottom);  \
    if (_refine_is_bottom) {                                                             \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      const void* _refine_vars[] = {__VA_ARGS__};                                        \
      bennet_failure_blame_many(_refine_vars);                                           \
                                                                                         \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                             \
    } else {                                                                             \
      var##_cs = _refine_result;                                                         \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, false);                            \
    }                                                                                    \
  }

#define BENNET_REFINE_CONSTRAINT_BEGIN(                                                  \
    c_ty, var, x_sym, x_bt, constraint_expr, last_var, ...)                              \
  {                                                                                      \
    BENNET_CHECK_TIMEOUT();                                                              \
    cn_base_type _refine_bt = (x_bt);                                                    \
    bennet_absint_sym _refine_sym = {.name = (x_sym).name, .id = (x_sym).id};            \
    bool _refine_is_bottom = false;                                                      \
    bennet_domain(c_ty)* _refine_result = bennet_domain_refine(                          \
        c_ty, var##_cs, _refine_sym, &_refine_bt, constraint_expr, &_refine_is_bottom);  \
    if (_refine_is_bottom) {                                                             \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      const void* _refine_vars[] = {__VA_ARGS__};                                        \
      bennet_failure_blame_many(_refine_vars);                                           \
                                                                                         \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, true);

#define BENNET_REFINE_CONSTRAINT_END(c_ty, var, last_var)                                \
  }                                                                                      \
  else {                                                                                 \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, false);                              \
    var##_cs = _refine_result;                                                           \
  }                                                                                      \
  }

/* Backward refinement for a single free variable within a REFINE_CONSTRAINT block.
 * Must be called between BENNET_REFINE_CONSTRAINT_BEGIN and _END.
 * Depends on _refine_sym and _refine_bt being in scope from BEGIN.
 *
 * Parameters:
 *   v_c_ty:           C type of the free variable (e.g., int32_t)
 *   v:                free variable identifier (for blame)
 *   v_sym:            bennet_absint_sym for the free variable
 *   v_bt:             cn_base_type for the free variable
 *   constraint_expr:  cn_term* constraint with two symbolic vars
 *   var_cs:           domain of the primary variable (e.g., x_cs)
 */
#define BENNET_REFINE_CONSTRAINT_BACKWARD(                                               \
    v_c_ty, v, v_sym, v_bt, constraint_expr, var_cs)                                     \
  {                                                                                      \
    BENNET_CHECK_TIMEOUT();                                                              \
    bennet_absint_sym _rc_sym_v = {.name = (v_sym).name, .id = (v_sym).id};              \
    cn_base_type _rc_bt_v = (v_bt);                                                      \
    cn_term* _rc_expr = (constraint_expr);                                               \
    bool _rc_is_bottom = false;                                                          \
    bennet_domain(v_c_ty)* _rc_D_v = bennet_domain_refine_with_state(v_c_ty,             \
        bennet_domain_top(v_c_ty),                                                       \
        _rc_sym_v,                                                                       \
        &_rc_bt_v,                                                                       \
        _rc_expr,                                                                        \
        &_rc_is_bottom,                                                                  \
        _refine_sym,                                                                     \
        bennet_tagged_domain_create(&_refine_bt, var_cs));                               \
    if (!_rc_is_bottom && !bennet_domain_is_top(v_c_ty, _rc_D_v)) {                      \
      bennet_failure_blame_domain(v_c_ty, v, _rc_D_v);                                   \
    }                                                                                    \
  }

#define BENNET_REFINE_CONSTRAINT_SIGNED(                                                 \
    bits, var, x_sym, constraint_expr, last_var, ...)                                    \
  BENNET_REFINE_CONSTRAINT(int##bits##_t,                                                \
      var,                                                                               \
      x_sym,                                                                             \
      cn_base_type_bits(true, bits),                                                     \
      constraint_expr,                                                                   \
      last_var,                                                                          \
      __VA_ARGS__)

#define BENNET_REFINE_CONSTRAINT_UNSIGNED(                                               \
    bits, var, x_sym, constraint_expr, last_var, ...)                                    \
  BENNET_REFINE_CONSTRAINT(uint##bits##_t,                                               \
      var,                                                                               \
      x_sym,                                                                             \
      cn_base_type_bits(false, bits),                                                    \
      constraint_expr,                                                                   \
      last_var,                                                                          \
      __VA_ARGS__)

#define BENNET_REFINE_CONSTRAINT_POINTER(var, x_sym, constraint_expr, last_var, ...)     \
  BENNET_REFINE_CONSTRAINT(uintptr_t,                                                    \
      var,                                                                               \
      x_sym,                                                                             \
      cn_base_type_simple(CN_BASE_LOC),                                                  \
      constraint_expr,                                                                   \
      last_var,                                                                          \
      __VA_ARGS__)

/**
 * Refine domain using a pointer assignment.
 * Used between ARBITRARY_DOMAIN_BEGIN and _END to narrow the pointer
 * domain based on a statically-computed byte offset from the base pointer.
 *
 * @param c_ty    C type (uintptr_t for pointers)
 * @param var     Variable being refined (has var##_cs and var##_cs_tmp)
 * @param offset  Static byte offset from the base pointer
 * @param bytes   sizeof the type being written
 * @param last_var Backtrack label if domain becomes bottom
 */
#define BENNET_REFINE_ASSIGNMENT(c_ty,                                                   \
    var,                                                                                 \
    offset,                                                                              \
    bytes,                                                                               \
    last_var,                                                                            \
    addr_term,                                                                           \
    num_other_vars,                                                                      \
    other_var_ids,                                                                       \
    other_var_syms,                                                                      \
    ...)                                                                                 \
  {                                                                                      \
    BENNET_CHECK_TIMEOUT();                                                              \
    bennet_domain(c_ty)* _asgn_domain =                                                  \
        bennet_domain_from_assignment_##c_ty(0, (void*)(offset), bytes);                 \
    bennet_domain(c_ty)* _refine_result =                                                \
        bennet_domain_meet(c_ty, var##_cs, _asgn_domain);                                \
    if (bennet_domain_is_bottom(c_ty, _refine_result)) {                                 \
      if (bennet_get_dynamic_absint_assign() == BENNET_DYNAMIC_ABSINT_ASSIGN_ALSO) {     \
        bennet_assign_backward_blame(                                                    \
            addr_term, num_other_vars, other_var_ids, other_var_syms, bytes);            \
      } else {                                                                           \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        const void* _asgn_vars[] = {__VA_ARGS__};                                        \
        bennet_failure_blame_many(_asgn_vars);                                           \
      }                                                                                  \
                                                                                         \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                             \
    } else {                                                                             \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, false);                            \
      var##_cs = _refine_result;                                                         \
    }                                                                                    \
  }

/* Sample from refined domain, handle backtracking */
#define BENNET_LET_ARBITRARY_DOMAIN_END(backtracks, cn_ty, c_ty, var, last_var)          \
  if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                        \
    bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                        \
    goto bennet_label_##last_var##_backtrack;                                            \
  }                                                                                      \
                                                                                         \
  var##_cs_tmp = var##_cs;                                                               \
                                                                                         \
  int var##_backtracks = backtracks;                                                     \
                                                                                         \
  bennet_label_##var##_gen :;                                                            \
  cn_ty* var = bennet_arbitrary_##cn_ty(var##_cs_tmp);                                   \
                                                                                         \
  var##_cs_tmp = var##_cs;                                                               \
                                                                                         \
  if (var##_restore_randomness) {                                                        \
    bennet_rand_restore(var##_rand_checkpoint_after);                                    \
    var##_restore_randomness = false;                                                    \
  }                                                                                      \
  var##_rand_checkpoint_after = bennet_rand_save();                                      \
                                                                                         \
  if (0) {                                                                               \
    bennet_label_##var##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    bool var##_should_restore_randomness =                                               \
        bennet_failure_get_failure_type() == BENNET_FAILURE_ASSIGN;                      \
    bool var##_is_young = bennet_failure_is_young();                                     \
    if (bennet_backtrack_arbitrary_##cn_ty(                                              \
            &var##_backtracks, &var##_cs, &var##_cs_tmp, &var##_checkpoint, var)) {      \
      var##_restore_randomness = var##_should_restore_randomness;                        \
      if (!var##_restore_randomness) {                                                   \
        var##_restore_randomness =                                                       \
            !var##_is_young && !bennet_domain_equal(c_ty, var##_cs, var##_cs_tmp);       \
      }                                                                                  \
                                                                                         \
      goto bennet_label_##var##_gen;                                                     \
    } else {                                                                             \
      if (var##_is_young && bennet_failure_is_blamed(var)) {                             \
        bennet_failure_mark_old();                                                       \
      }                                                                                  \
                                                                                         \
      if (bennet_failure_is_blamed(var)) {                                               \
        bennet_domain(c_ty)* var##_failure_domain =                                      \
            bennet_failure_get_domain(c_ty, var);                                        \
        if (var##_failure_domain != NULL) {                                              \
          var##_failure_domain = bennet_domain_copy(c_ty, var##_failure_domain);         \
        }                                                                                \
        bennet_failure_remove_blame(var);                                                \
                                                                                         \
        bennet_domain(c_ty)* refine_result =                                             \
            bennet_domain_meet(c_ty, var##_cs, var##_failure_domain);                    \
        bool refine_is_bottom = bennet_domain_is_bottom(c_ty, refine_result);            \
        std_free(refine_result);                                                         \
                                                                                         \
        if (!bennet_failure_has_blame() && refine_is_bottom) {                           \
          bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                        \
          bennet_failure_mark_old(); /* necessary due to set_type impl */                \
                                                                                         \
          if (var##_failure_domain != NULL) {                                            \
            var##_cs = var##_failure_domain;                                             \
          }                                                                              \
                                                                                         \
          goto bennet_label_##var##_refine;                                              \
        }                                                                                \
      }                                                                                  \
                                                                                         \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_LET_ARBITRARY_DOMAIN_END_SIGNED(backtracks, bits, var, last_var)          \
  BENNET_LET_ARBITRARY_DOMAIN_END(                                                       \
      backtracks, cn_bits_i##bits, int##bits##_t, var, last_var)

#define BENNET_LET_ARBITRARY_DOMAIN_END_UNSIGNED(backtracks, bits, var, last_var)        \
  BENNET_LET_ARBITRARY_DOMAIN_END(                                                       \
      backtracks, cn_bits_u##bits, uint##bits##_t, var, last_var)

#define BENNET_LET_ARBITRARY_DOMAIN_END_POINTER(backtracks, var, last_var)               \
  BENNET_LET_ARBITRARY_DOMAIN_END(backtracks, cn_pointer, uintptr_t, var, last_var)

/*=============================================================================
 * Original combined macro (for backward compatibility)
 *===========================================================================*/

#define BENNET_LET_ARBITRARY_DOMAIN(backtracks, cn_ty, c_ty, var, last_var, ...)         \
  bool var##_restore_randomness = false;                                                 \
  int var##_backtracks = backtracks;                                                     \
  bennet_checkpoint var##_checkpoint = bennet_checkpoint_save();                         \
  bennet_rand_checkpoint var##_rand_checkpoint_before = bennet_rand_save();              \
  bennet_rand_checkpoint var##_rand_checkpoint_after = NULL;                             \
                                                                                         \
  bennet_domain(c_ty)* var##_cs = __VA_ARGS__;                                           \
  bennet_domain(c_ty)* var##_cs_tmp = var##_cs;                                          \
                                                                                         \
  bennet_label_##var##_gen :;                                                            \
  cn_ty* var = bennet_arbitrary_##cn_ty(var##_cs_tmp);                                   \
                                                                                         \
  var##_cs_tmp = var##_cs;                                                               \
                                                                                         \
  if (var##_restore_randomness) {                                                        \
    bennet_rand_restore(var##_rand_checkpoint_after);                                    \
    var##_restore_randomness = false;                                                    \
  }                                                                                      \
  var##_rand_checkpoint_after = bennet_rand_save();                                      \
                                                                                         \
  if (0) {                                                                               \
    bennet_label_##var##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    bool var##_should_restore_randomness =                                               \
        bennet_failure_get_failure_type() == BENNET_FAILURE_ASSIGN;                      \
    bool var##_is_young = bennet_failure_is_young();                                     \
    if (bennet_backtrack_arbitrary_##cn_ty(                                              \
            &var##_backtracks, &var##_cs, &var##_cs_tmp, &var##_checkpoint, var)) {      \
      var##_restore_randomness = var##_should_restore_randomness;                        \
      if (!var##_restore_randomness) {                                                   \
        var##_restore_randomness =                                                       \
            !var##_is_young && !bennet_domain_equal(c_ty, var##_cs, var##_cs_tmp);       \
      }                                                                                  \
                                                                                         \
      goto bennet_label_##var##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_LET_ARBITRARY_DOMAIN_POINTER(backtracks, var, last_var, ...)              \
  BENNET_LET_ARBITRARY_DOMAIN(                                                           \
      backtracks, cn_pointer, uintptr_t, var, last_var, __VA_ARGS__)

#define BENNET_LET_ARBITRARY_DOMAIN_UNSIGNED(backtracks, bits, var, last_var, ...)       \
  BENNET_LET_ARBITRARY_DOMAIN(                                                           \
      backtracks, cn_bits_u##bits, uint##bits##_t, var, last_var, __VA_ARGS__)

#define BENNET_LET_ARBITRARY_DOMAIN_SIGNED(backtracks, bits, var, last_var, ...)         \
  BENNET_LET_ARBITRARY_DOMAIN(                                                           \
      backtracks, cn_bits_i##bits, int##bits##_t, var, last_var, __VA_ARGS__)

#define BENNET_LET_ARBITRARY(backtracks, cn_ty, c_ty, var, last_var)                     \
  BENNET_LET_ARBITRARY_DOMAIN(                                                           \
      backtracks, cn_ty, c_ty, var, last_var, bennet_domain_top(c_ty))

#define BENNET_LET_ARBITRARY_POINTER(backtracks, var, last_var)                          \
  BENNET_LET_ARBITRARY(backtracks, cn_pointer, uintptr_t, var, last_var)

#define BENNET_LET_ARBITRARY_UNSIGNED(backtracks, bits, var, last_var)                   \
  BENNET_LET_ARBITRARY(backtracks, cn_bits_u##bits, uint##bits##_t, var, last_var)

#define BENNET_LET_ARBITRARY_SIGNED(backtracks, bits, var, last_var)                     \
  BENNET_LET_ARBITRARY(backtracks, cn_bits_i##bits, int##bits##_t, var, last_var)

#define BENNET_LET_SPECIALIZED(backtracks,                                               \
    cn_ty,                                                                               \
    c_ty,                                                                                \
    var,                                                                                 \
    last_var,                                                                            \
    lower_bound_ex,                                                                      \
    lower_bound_inc,                                                                     \
    upper_bound_inc,                                                                     \
    upper_bound_ex,                                                                      \
    ...)                                                                                 \
  bool var##_restore_randomness = false;                                                 \
  int var##_backtracks = backtracks;                                                     \
  bennet_checkpoint var##_checkpoint = bennet_checkpoint_save();                         \
  bennet_rand_checkpoint var##_rand_checkpoint_before = bennet_rand_save();              \
  bennet_rand_checkpoint var##_rand_checkpoint_after = NULL;                             \
                                                                                         \
  const void* var##_vars[] = {__VA_ARGS__};                                              \
                                                                                         \
  bennet_label_##var##_gen :;                                                            \
  cn_ty* var = bennet_specialized_##cn_ty(                                               \
      lower_bound_ex, lower_bound_inc, upper_bound_inc, upper_bound_ex, var##_vars);     \
  if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                        \
    goto bennet_label_##last_var##_backtrack;                                            \
  }                                                                                      \
                                                                                         \
  if (var##_restore_randomness) {                                                        \
    bennet_rand_restore(var##_rand_checkpoint_after);                                    \
    var##_restore_randomness = false;                                                    \
  }                                                                                      \
  var##_rand_checkpoint_after = bennet_rand_save();                                      \
                                                                                         \
  if (0) {                                                                               \
    bennet_label_##var##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    bool var##_should_restore_randomness =                                               \
        bennet_failure_get_failure_type() == BENNET_FAILURE_ASSIGN;                      \
    bennet_checkpoint_restore(&var##_checkpoint);                                        \
    bennet_failure_mark_old();                                                           \
    if (var##_backtracks > 0) {                                                          \
      var##_backtracks--;                                                                \
      var##_restore_randomness = var##_should_restore_randomness;                        \
      bennet_failure_reset();                                                            \
      goto bennet_label_##var##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_LET_SPECIALIZED_UNSIGNED(backtracks,                                      \
    bits,                                                                                \
    var,                                                                                 \
    last_var,                                                                            \
    lower_bound_ex,                                                                      \
    lower_bound_inc,                                                                     \
    upper_bound_inc,                                                                     \
    upper_bound_ex,                                                                      \
    ...)                                                                                 \
  BENNET_LET_SPECIALIZED(backtracks,                                                     \
      cn_bits_u##bits,                                                                   \
      uint##bits##_t,                                                                    \
      var,                                                                               \
      last_var,                                                                          \
      lower_bound_ex,                                                                    \
      lower_bound_inc,                                                                   \
      upper_bound_inc,                                                                   \
      upper_bound_ex,                                                                    \
      __VA_ARGS__)

#define BENNET_LET_SPECIALIZED_SIGNED(backtracks,                                        \
    bits,                                                                                \
    var,                                                                                 \
    last_var,                                                                            \
    lower_bound_ex,                                                                      \
    lower_bound_inc,                                                                     \
    upper_bound_inc,                                                                     \
    upper_bound_ex,                                                                      \
    ...)                                                                                 \
  BENNET_LET_SPECIALIZED(backtracks,                                                     \
      cn_bits_i##bits,                                                                   \
      int##bits##_t,                                                                     \
      var,                                                                               \
      last_var,                                                                          \
      lower_bound_ex,                                                                    \
      lower_bound_inc,                                                                   \
      upper_bound_inc,                                                                   \
      upper_bound_ex,                                                                    \
      __VA_ARGS__)

#define BENNET_LET_SPECIALIZED_POINTER(backtracks,                                       \
    var,                                                                                 \
    last_var,                                                                            \
    lower_bound_ex,                                                                      \
    lower_bound_inc,                                                                     \
    upper_bound_inc,                                                                     \
    upper_bound_ex,                                                                      \
    ...)                                                                                 \
  BENNET_LET_SPECIALIZED(backtracks,                                                     \
      cn_pointer,                                                                        \
      cn_pointer,                                                                        \
      var,                                                                               \
      last_var,                                                                          \
      lower_bound_ex,                                                                    \
      lower_bound_inc,                                                                   \
      upper_bound_inc,                                                                   \
      upper_bound_ex,                                                                    \
      __VA_ARGS__)

#define BENNET_LET_RETURN_BEGIN(ty, var, expr)                                           \
  ty* var = expr;                                                                        \
  if (0) {                                                                               \
    bennet_label_##var##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    if (bennet_failure_is_blamed(var)) {
#define BENNET_LET_RETURN_END(var, last_var, ...)                                        \
  const void* toAdd[] = {__VA_ARGS__};                                                   \
  bool is_young = bennet_failure_is_young();                                             \
  bennet_failure_remove_blame(var);                                                      \
  bennet_failure_blame_many(toAdd);                                                      \
  if (is_young) {                                                                        \
    bennet_failure_mark_young();                                                         \
  }                                                                                      \
  }                                                                                      \
                                                                                         \
  goto bennet_label_##last_var##_backtrack;                                              \
  }

#define BENNET_LET_RETURN(ty, var, expr, last_var, ...)                                  \
  BENNET_LET_RETURN_BEGIN(ty, var, expr)                                                 \
  BENNET_LET_RETURN_END(var, last_var, __VA_ARGS__)

#define BENNET_LET(backtracks, cn_ty, var, last_var, ...)                                \
  int var##_backtracks = backtracks;                                                     \
  bennet_checkpoint var##_checkpoint = bennet_checkpoint_save();                         \
  bennet_label_##var##_gen :;                                                            \
  cn_ty* var = __VA_ARGS__;                                                              \
                                                                                         \
  if (0) {                                                                               \
    bennet_label_##var##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
                                                                                         \
    if (bennet_backtrack(&var##_backtracks, &var##_checkpoint, var)) {                   \
      goto bennet_label_##var##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_MAP_BEGIN(map, i, i_ty, perm, max, last_var, ...)                         \
  cn_map* map = map_create();                                                            \
  {                                                                                      \
    i_ty* i = max;                                                                       \
                                                                                         \
    if (0) {                                                                             \
      bennet_label_##i##_backtrack :;                                                    \
      BENNET_CHECK_TIMEOUT();                                                            \
      if (bennet_failure_is_blamed(i)) {                                                 \
        const void* toAdd[] = {__VA_ARGS__};                                             \
        bennet_failure_remove_blame(i);                                                  \
        bennet_failure_blame_many(toAdd);                                                \
      }                                                                                  \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
                                                                                         \
    while (convert_from_cn_bool(perm)) {                                                 \
      /* Poll the input timeout per iteration: the loop body's success path has no */    \
      /* other timeout check, so an unbounded `each` would otherwise build the whole */  \
      /* array uninterrupted, defeating --input-timeout and risking OOM. */              \
      BENNET_CHECK_TIMEOUT();                                                            \
    /* Generate each item */
#define BENNET_MAP_END(map, i, i_ty, min, val)                                           \
  cn_map_set(map, cast_##i_ty##_to_cn_integer(i), val);                                  \
                                                                                         \
  if (convert_from_cn_bool(i_ty##_equality(i, min))) {                                   \
    break;                                                                               \
  }                                                                                      \
                                                                                         \
  i = i_ty##_sub(i, convert_to_##i_ty(1));                                               \
  }                                                                                      \
  }

#define BENNET_PICK_BEGIN(ty, var, tmp, last_var, ...)                                   \
  ty* var = NULL;                                                                        \
  uint64_t tmp##_choices[] = {__VA_ARGS__, UINT64_MAX};                                  \
  uint8_t tmp##_num_choices = 0;                                                         \
  while (tmp##_choices[tmp##_num_choices] != UINT64_MAX) {                               \
    tmp##_num_choices += 2;                                                              \
  }                                                                                      \
  tmp##_num_choices /= 2;                                                                \
  struct bennet_int_urn* tmp##_urn = urn_from_array(tmp##_choices, tmp##_num_choices);   \
  bennet_checkpoint tmp##_checkpoint = bennet_checkpoint_save();                         \
  bennet_label_##tmp##_gen :;                                                            \
  cn_bits_u64* tmp = convert_to_cn_bits_u64(urn_remove(tmp##_urn));                      \
  if (0) {                                                                               \
    bennet_label_##tmp##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    bennet_checkpoint_restore(&tmp##_checkpoint);                                        \
    bennet_failure_mark_old();                                                           \
    if ((bennet_failure_get_failure_type() == BENNET_FAILURE_ASSERT ||                   \
            bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH) &&                \
        tmp##_urn->size != 0) {                                                          \
      bennet_failure_reset();                                                            \
      goto bennet_label_##tmp##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }                                                                                      \
  switch (convert_from_cn_bits_u64(tmp)) {                                               \
  /* Case per choice */

#define BENNET_PICK_CASE_BEGIN(index) case index: {
#define BENNET_PICK_CASE_END(var, e)                                                     \
  var = e;                                                                               \
  }                                                                                      \
  break;

#define BENNET_PICK_END(tmp)                                                             \
  default:                                                                               \
    printf("Invalid generated value");                                                   \
    assert(false);                                                                       \
    }                                                                                    \
    urn_free(tmp##_urn);

#define BENNET_SPLIT_BEGIN(tmp, ...)                                                     \
  void* tmp = malloc(1);                                                                 \
  int tmp##_backtracks = bennet_get_size_split_backtracks_allowed();                     \
  bennet_checkpoint tmp##_checkpoint = bennet_checkpoint_save();                         \
  bennet_label_##tmp##_gen : {                                                           \
    size_t* vars[] = {__VA_ARGS__};                                                      \
    int count = 0;                                                                       \
    for (int i = 0; vars[i] != NULL; i++) {                                              \
      count += 1;                                                                        \
    }

#define BENNET_SPLIT_END(tmp, last_var, ...)                                             \
  if (count >= bennet_rec_size) {                                                        \
    bennet_failure_set_failure_type(BENNET_FAILURE_DEPTH);                               \
    add_depth_failure();                                                                 \
    const void* toAdd[] = {__VA_ARGS__};                                                 \
    bennet_failure_blame_many(toAdd);                                                    \
    goto bennet_label_##last_var##_backtrack;                                            \
  }                                                                                      \
  bennet_split(bennet_rec_size - count - 1, vars, count);                                \
  for (int i = 0; i < count; i++) {                                                      \
    *(vars[i]) = *(vars[i]) + 1;                                                         \
  }                                                                                      \
  }                                                                                      \
  if (0) {                                                                               \
    bennet_label_##tmp##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    if (bennet_failure_is_blamed(tmp)) {                                                 \
      bennet_checkpoint_restore(&tmp##_checkpoint);                                      \
      bennet_failure_remove_blame(tmp);                                                  \
      free(tmp);                                                                         \
                                                                                         \
      const void* toAdd[] = {__VA_ARGS__};                                               \
      bennet_failure_blame_many(toAdd);                                                  \
      if (tmp##_backtracks <= 0) {                                                       \
        goto bennet_label_##last_var##_backtrack;                                        \
      }                                                                                  \
      tmp##_backtracks--;                                                                \
      bennet_failure_reset();                                                            \
      goto bennet_label_##tmp##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#endif  // BENNET_DSL_H
