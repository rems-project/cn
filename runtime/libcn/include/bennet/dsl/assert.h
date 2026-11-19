#ifndef BENNET_ASSERT_H
#define BENNET_ASSERT_H

#include <stddef.h>
#include <stdint.h>

#include <bennet/dsl/assign.h>
#include <bennet/info/backtracks.h>
#include <bennet/info/unsatisfied.h>
#include <bennet/internals/domain.h>
#include <bennet/state/failure.h>
#include <bennet/utils/optional.h>
#include <cn-executable/utils.h>

#ifdef __cplusplus
extern "C" {
#endif

#define BENNET_ASSERT(cond, last_var, ...)                                               \
  if (!convert_from_cn_bool(cond)) {                                                     \
    bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                        \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                               \
                                                                                         \
    bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                              \
    const void* vars[] = {__VA_ARGS__};                                                  \
    bennet_failure_blame_many(vars);                                                     \
    goto bennet_label_##last_var##_backtrack;                                            \
  }                                                                                      \
                                                                                         \
  bennet_info_unsatisfied_log(__FILE__, __LINE__, false);

/**
 * BEGIN macro for assert_domain.
 *
 * @param backtrack_var The variable for this assert_domain's backtrack label
 * @param N The number of free variables in the assert_domain
 */
#define BENNET_ASSERT_DOMAIN_BEGIN(backtrack_var, N) ((void)0)

/**
 * EVAL_CONSTRAINT macro: Evaluate a boolean constraint; if false, blame
 * the free variables using the failure system.
 *
 * @param cond The boolean constraint expression (cn_bool*)
 * @param ... NULL-terminated list of variable pointers to blame
 */
#define BENNET_ASSERT_DOMAIN_EVAL_CONSTRAINT(cond, ...)                                  \
  if (!convert_from_cn_bool(cond)) {                                                     \
    if (bennet_failure_get_failure_type() != BENNET_FAILURE_ASSIGN) {                    \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
    }                                                                                    \
    const void* _eval_vars[] = {__VA_ARGS__};                                            \
    bennet_failure_blame_many(_eval_vars);                                               \
  }

/**
 * EVAL_ASSIGNMENT macro: Check if a pointer assignment is consistent with
 * the current domain. If not (bottom after meet), blame the variable.
 *
 * @param cty The C type (uintptr_t)
 * @param var The pointer variable to blame if inconsistent
 * @param base_ptr void* value of the pointer variable
 * @param addr void* address of the assignment
 * @param bytes sizeof the type being written
 * @param domain_expr The current domain expression for this variable
 */
#define BENNET_ASSERT_DOMAIN_EVAL_ASSIGNMENT(cty,                                        \
    var,                                                                                 \
    base_ptr,                                                                            \
    addr,                                                                                \
    bytes,                                                                               \
    domain_expr,                                                                         \
    addr_term,                                                                           \
    num_other_vars,                                                                      \
    other_var_ids,                                                                       \
    other_var_syms)                                                                      \
  {                                                                                      \
    bennet_domain(cty)* _eval_asgn_domain =                                              \
        bennet_domain_from_assignment_##cty(base_ptr, addr, bytes);                      \
    bennet_domain(cty)* _eval_meet =                                                     \
        bennet_domain_meet(cty, (domain_expr), _eval_asgn_domain);                       \
    if (bennet_domain_is_bottom(cty, _eval_meet)) {                                      \
      bennet_dynamic_absint_assign_mode _mode = bennet_get_dynamic_absint_assign();      \
      if (_mode != BENNET_DYNAMIC_ABSINT_ASSIGN_ONLY) {                                  \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSIGN);                          \
        bennet_failure_blame(var);                                                       \
      }                                                                                  \
      if (_mode != BENNET_DYNAMIC_ABSINT_ASSIGN_DISABLED) {                              \
        bennet_assign_backward_blame(                                                    \
            addr_term, num_other_vars, other_var_ids, other_var_syms, bytes);            \
      }                                                                                  \
    }                                                                                    \
  }

/**
 * VAR_BEGIN macro: Declares a per-variable domain on the stack.
 * Use between BEGIN and END when constraint refinements are needed.
 *
 * @param cty The C type of the variable
 * @param backtrack_var The backtrack variable for this assert_domain
 * @param var The variable name (for domain naming)
 * @param domain_expr The initial domain expression
 */
#define BENNET_ASSERT_DOMAIN_VAR_BEGIN(cty, backtrack_var, var, domain_expr)             \
  bennet_domain(cty)* backtrack_var##_##var##_domain = (domain_expr);

/**
 * REFINE_CONSTRAINT macro for assert_domain: Refines the per-variable domain
 * using a constraint expression. Use between VAR_BEGIN and VAR_END.
 *
 * @param c_ty The C type of the variable
 * @param backtrack_var The backtrack variable for this assert_domain
 * @param var The variable (must match VAR_BEGIN)
 * @param x_sym The cn_sym for the variable
 * @param x_bt The base type expression for the variable
 * @param constraint_expr The constraint expression to refine against
 */
#define BENNET_ASSERT_DOMAIN_REFINE_CONSTRAINT(                                          \
    c_ty, backtrack_var, var, x_sym, x_bt, constraint_expr, ...)                         \
  {                                                                                      \
    BENNET_CHECK_TIMEOUT();                                                              \
    cn_base_type _refine_bt = (x_bt);                                                    \
    bennet_absint_sym _refine_sym = {.name = (x_sym).name, .id = (x_sym).id};            \
    bool _refine_is_bottom = false;                                                      \
    backtrack_var##_##var##_domain = bennet_domain_refine(c_ty,                          \
        backtrack_var##_##var##_domain,                                                  \
        _refine_sym,                                                                     \
        &_refine_bt,                                                                     \
        constraint_expr,                                                                 \
        &_refine_is_bottom);                                                             \
    if (_refine_is_bottom) {                                                             \
      if (bennet_failure_get_failure_type() != BENNET_FAILURE_ASSIGN) {                  \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
      }                                                                                  \
      const void* _refine_vars[] = {__VA_ARGS__};                                        \
      bennet_failure_blame_many(_refine_vars);                                           \
    }                                                                                    \
  }

/**
 * Refine a per-variable domain using a pointer assignment.
 * Used between ASSERT_DOMAIN_VAR_BEGIN and ASSERT_DOMAIN_VAR_END.
 *
 * @param c_ty         C type (uintptr_t)
 * @param backtrack_var The assert_domain's backtrack variable
 * @param var           The variable (must match VAR_BEGIN)
 * @param base_ptr     void* value of the pointer variable
 * @param addr         void* address of the assignment
 * @param bytes        sizeof the type being written
 */
#define BENNET_ASSERT_DOMAIN_REFINE_ASSIGNMENT(c_ty,                                     \
    backtrack_var,                                                                       \
    var,                                                                                 \
    base_ptr,                                                                            \
    addr,                                                                                \
    bytes,                                                                               \
    addr_term,                                                                           \
    num_other_vars,                                                                      \
    other_var_ids,                                                                       \
    other_var_syms)                                                                      \
  {                                                                                      \
    BENNET_CHECK_TIMEOUT();                                                              \
    bennet_domain(c_ty)* _asgn_domain =                                                  \
        bennet_domain_from_assignment_##c_ty(base_ptr, addr, bytes);                     \
    backtrack_var##_##var##_domain =                                                     \
        bennet_domain_meet(c_ty, backtrack_var##_##var##_domain, _asgn_domain);          \
    if (bennet_domain_is_bottom(c_ty, backtrack_var##_##var##_domain)) {                 \
      bennet_dynamic_absint_assign_mode _mode = bennet_get_dynamic_absint_assign();      \
      if (_mode != BENNET_DYNAMIC_ABSINT_ASSIGN_ONLY) {                                  \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSIGN);                          \
        bennet_failure_blame(var);                                                       \
      }                                                                                  \
      if (_mode != BENNET_DYNAMIC_ABSINT_ASSIGN_DISABLED) {                              \
        bennet_assign_backward_blame(                                                    \
            addr_term, num_other_vars, other_var_ids, other_var_syms, bytes);            \
      }                                                                                  \
    }                                                                                    \
  }

/**
 * VAR_END macro: Checks the (possibly refined) domain and handles backtracking.
 * Use after VAR_BEGIN and any REFINE_CONSTRAINT calls.
 *
 * @param cty The C type of the variable (e.g., uintptr_t, uint64_t)
 * @param cn_ty The CN wrapper type (e.g., cn_pointer, cn_bits_u64)
 * @param var The variable name (must match VAR_BEGIN), blamed on failure
 * @param backtrack_var The backtrack variable for this assert_domain
 */
#define BENNET_ASSERT_DOMAIN_VAR_END(cty, cn_ty, var, backtrack_var)                     \
  if (!bennet_domain_check(cty,                                                          \
          (cty)convert_from_##cn_ty((cn_ty*)(var)),                                      \
          backtrack_var##_##var##_domain)) {                                             \
    if (!bennet_domain_check_ownership(cty,                                              \
            (cty)convert_from_##cn_ty((cn_ty*)(var)),                                    \
            backtrack_var##_##var##_domain)) {                                           \
      /* ASSIGN failure - ownership check failed */                                      \
      if (bennet_failure_get_failure_type() != BENNET_FAILURE_ASSIGN) {                  \
        bennet_failure_reset();                                                          \
      }                                                                                  \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSIGN);                            \
      bennet_failure_blame_domain(cty,                                                   \
          var,                                                                           \
          bennet_domain_top_except_ownership(cty, backtrack_var##_##var##_domain));      \
    } else if (bennet_failure_get_failure_type() != BENNET_FAILURE_ASSIGN) {             \
      /* ASSERT failure - but only if no prior ASSIGN */                                 \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_domain(cty, var, backtrack_var##_##var##_domain);             \
    }                                                                                    \
  }

/**
 * END macro for assert_domain.
 * Logs diagnostics, and if a failure occurred, jumps to the backtrack label.
 *
 * @param backtrack_var The variable to use for this assert_domain's backtrack label
 * @param last_var The outer backtrack variable to jump to on failure
 */
#define BENNET_ASSERT_DOMAIN_END(backtrack_var, last_var)                                \
  if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                        \
    bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                        \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                               \
                                                                                         \
    bennet_label_##backtrack_var##_backtrack : BENNET_CHECK_TIMEOUT();                   \
                                                                                         \
    goto bennet_label_##last_var##_backtrack;                                            \
  }                                                                                      \
                                                                                         \
  bennet_info_unsatisfied_log(__FILE__, __LINE__, false);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_ASSERT_H
