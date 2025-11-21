/* Type aliases needed by cbindgen-generated function signatures
 * These must be defined before the function declarations that use them.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

/* Type alias to map cbindgen-generated type to actual Bennet type */
typedef enum bennet_sizing_strategy cn_bennet_sizing_strategy;

/* Test generation progress level */
enum cn_test_gen_progress {
  CN_TEST_GEN_PROGRESS_NONE = 0,
  CN_TEST_GEN_PROGRESS_FINAL = 1,
  CN_TEST_GEN_PROGRESS_ALL = 2
};

/* Test input configuration structure */
struct cn_test_input {
  bool replay;
  enum cn_test_gen_progress progress_level;
  enum bennet_sizing_strategy sizing_strategy;
  bool trap;
  bool replicas;
  bool log_all_backtracks;
  bool output_tyche;
  FILE* tyche_output_stream;
  uint64_t begin_time;
};

/* Function pointer type for test cases - replaces cbindgen's incomplete forward declaration */
typedef enum cn_test_result (*cn_Option_CnTestCaseFn)(struct cn_test_input);
