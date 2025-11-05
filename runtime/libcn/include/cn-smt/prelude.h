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
 * @brief Reset all CN-SMT registries.
 *
 * This function resets all CN-SMT registries by setting their initialization
 * flags to false, allowing them to be re-initialized on next use.
 */
void cn_smt_reset(void);

#endif  // CN_SMT_PRELUDE_H
