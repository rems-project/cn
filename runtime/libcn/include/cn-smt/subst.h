#ifndef CN_SMT_SUBST_H
#define CN_SMT_SUBST_H

#include <stdint.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <cn-smt/terms.h>

#ifdef __cplusplus
extern "C" {
#endif

// Typedef for cn_term pointer to work with hash table macros
typedef cn_term* cn_term_ptr;

// Declare optional type for cn_term_ptr
BENNET_OPTIONAL_DECL(cn_term_ptr);

// Hash table type for symbol ID to constant term substitutions
BENNET_HASH_TABLE_DECL(uint64_t, cn_term_ptr)

/**
 * Substitute symbols in a CN term with constants from a substitution table.
 * 
 * @param term The term to perform substitution on
 * @param subst_table Hash table mapping symbol IDs (uint64_t) to constant terms (cn_term*)
 * @return A new term with symbols substituted, or the original term if no substitutions occurred
 * 
 * The function recursively traverses the term structure and replaces any CN_TERM_SYM
 * nodes whose symbol IDs are found in the substitution table with copies of the
 * corresponding constant terms.
 * 
 * Only constant terms (CN_TERM_CONST) should be used as substitution values.
 */
cn_term* cn_subst_term(
    cn_term* term, bennet_hash_table(uint64_t, cn_term_ptr) * subst_table);

/**
 * Create a new substitution table initialized with proper hash and equality functions.
 * 
 * @return A newly allocated substitution table, or NULL on allocation failure
 */
bennet_hash_table(uint64_t, cn_term_ptr) * cn_create_subst_table(void);

/**
 * Free a substitution table and its resources.
 * 
 * @param table The substitution table to free
 */
void cn_free_subst_table(bennet_hash_table(uint64_t, cn_term_ptr) * table);

/**
 * Add a substitution mapping from symbol ID to constant term.
 * 
 * @param table The substitution table
 * @param symbol_id The symbol ID to substitute
 * @param constant The constant term to substitute with (must be CN_TERM_CONST)
 */
void cn_add_substitution(bennet_hash_table(uint64_t, cn_term_ptr) * table,
    uint64_t symbol_id,
    cn_term* constant);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_SUBST_H
