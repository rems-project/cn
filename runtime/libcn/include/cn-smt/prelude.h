#ifndef CN_SMT_PRELUDE_H
#define CN_SMT_PRELUDE_H

#include <cn-smt/branch_history.h>
#include <cn-smt/concretize.h>
#include <cn-smt/context.h>
#include <cn-smt/datatypes.h>
#include <cn-smt/eval.h>
#include <cn-smt/from_smt.h>
#include <cn-smt/functions.h>
#include <cn-smt/gather.h>
#include <cn-smt/memory/intern.h>
#include <cn-smt/path_selector.h>
#include <cn-smt/records.h>
#include <cn-smt/sexp.h>
#include <cn-smt/solver.h>
#include <cn-smt/structs.h>
#include <cn-smt/subst.h>
#include <cn-smt/terms.h>
#include <cn-smt/to_smt.h>
#include <cn-smt/trie.h>

/**
 * @brief Destroy all CN-SMT registries and free associated memory.
 *
 * This function frees all CN-SMT registries and sets the initialization
 * flag to false. Safe to call multiple times (idempotent).
 */
void cn_smt_destroy(void);

/**
 * @brief Initialize CN-SMT subsystem.
 *
 * This function marks CN-SMT as initialized. The actual registry initialization
 * is lazy and happens on first use. Asserts if already initialized.
 */
void cn_smt_init(void);

#endif  // CN_SMT_PRELUDE_H
