#include <assert.h>
#include <inttypes.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/prelude.h>
#include <bennet/state/rand_alloc.h>
#include <bennet/utils.h>
#include <bennet/utils/hash_table.h>
#include <cn-executable/utils.h>
#include <cn-testing/result.h>
#include <cn-testing/test.h>
#include <cn-replicate/shape.h>
#include <cn-smt/branch_history.h>
#include <cn-smt/concretize.h>
#include <cn-smt/context.h>
#include <cn-smt/solver.h>

#define cn_printf(level, ...)                                                            \
  if (get_cn_logging_level() >= level) {                                                 \
    printf(__VA_ARGS__);                                                                 \
  }

struct cn_test_case {
  const char* suite;
  const char* name;
  cn_test_case_fn* func;
};

#define CN_TEST_MAX_TEST_CASES 1000

static struct cn_test_case test_cases[CN_TEST_MAX_TEST_CASES];
static uint16_t num_test_cases = 0;

/**
 * Registers a test.
 *
 * @param suite The name of the test suite.
 * @param name The name of the test.
 * @param func The function pointer to the test.
 */
void cn_register_test_case(const char* suite, const char* name, cn_test_case_fn* func) {
  if (num_test_cases == CN_TEST_MAX_TEST_CASES) {
    fprintf(stderr, "Error: Tried to register too many tests.\n");
    exit(1);
  }

  test_cases[num_test_cases++] =
      (struct cn_test_case){.suite = suite, .name = name, .func = func};
}

/**
 * Prints information about a test.
 *
 * @param suite The name of the test suite.
 * @param name The name of the test.
 * @param tests The number of test runs.
 * @param discards The number of discarded test cases.
 */
void print_test_info(const char* suite, const char* name, int tests, int discards) {
  if (tests == 0 && discards == 0) {
    printf("Testing %s::%s:", suite, name);
  } else if (discards == 0) {
    printf("Testing %s::%s: %d runs", suite, name, tests);
  } else {
    printf("Testing %s::%s: %d runs, %d discards", suite, name, tests, discards);
  }

  fflush(stdout);
}

#if defined(__has_builtin) && !defined(__ibmxl__) && __has_builtin(__builtin_debugtrap)
  #define _cn_trap() __builtin_debugtrap()
#elif defined(__has_builtin) && !defined(__ibmxl__) && __has_builtin(__debugbreak)
  #define _cn_trap() __debugbreak()
#elif defined(_MSC_VER) || defined(__INTEL_COMPILER)
  #define _cn_trap() __debugbreak()
#elif defined(__ARMCC_VERSION)
  #define _cn_trap() __breakpoint(42)
#elif defined(__ibmxl__) || defined(__xlC__)
  #include <builtins.h>
  #define _cn_trap() __trap(42)
#elif defined(__DMC__) && defined(_M_IX86)
static inline void _cn_trap(void) {
  __asm int 3h;
}
#elif defined(__i386__) || defined(__x86_64__)
static inline void _cn_trap(void) {
  __asm__ __volatile__("int3");
}
#elif defined(__thumb__)
static inline void _cn_trap(void) {
  __asm__ __volatile__(".inst 0xde01");
}
#elif defined(__aarch64__)
static inline void _cn_trap(void) {
  __asm__ __volatile__(".inst 0xd4200000");
}
#elif defined(__arm__)
static inline void _cn_trap(void) {
  __asm__ __volatile__(".inst 0xe7f001f0");
}
#elif defined(__alpha__) && !defined(__osf__)
static inline void _cn_trap(void) {
  __asm__ __volatile__("bpt");
}
#elif defined(_54_)
static inline void _cn_trap(void) {
  __asm__ __volatile__("ESTOP");
}
#elif defined(_55_)
static inline void _cn_trap(void) {
  __asm__ __volatile__(
      ";\n .if (.MNEMONIC)\n ESTOP_1\n .else\n ESTOP_1()\n .endif\n NOP");
}
#elif defined(_64P_)
static inline void _cn_trap(void) {
  __asm__ __volatile__("SWBP 0");
}
#elif defined(_6x_)
static inline void _cn_trap(void) {
  __asm__ __volatile__("NOP\n .word 0x10000000");
}
#elif defined(__STDC_HOSTED__) && (__STDC_HOSTED__ == 0) && defined(__GNUC__)
  #define _cn_trap() __builtin_trap()
#else
  #include <signal.h>
  #if defined(SIGTRAP)
    #define _cn_trap() raise(SIGTRAP)
  #else
    #define _cn_trap() raise(SIGABRT)
  #endif
#endif

void cn_trap(void) {
  _cn_trap();
}

typedef const char* const_char_ptr;
BENNET_HASH_TABLE_DECL(const_char_ptr, uint8_t)
BENNET_HASH_TABLE_IMPL(const_char_ptr, uint8_t)

struct cn_test_reproduction {
  size_t size;
  bennet_rand_checkpoint checkpoint;
};

void cn_test_reproduce(struct cn_test_reproduction* repro) {
  bennet_set_size(repro->size);
  bennet_rand_restore(repro->checkpoint);
}

int cn_test_main(int argc, char* argv[]) {
  uint64_t begin_time = bennet_get_microseconds();
  set_cn_logging_level(CN_LOGGING_NONE);

  bennet_srand(bennet_get_milliseconds());

  // Initialize test filter hash table
  bennet_hash_table(const_char_ptr, uint8_t) test_filter;
  bennet_hash_table_init(const_char_ptr, uint8_t)(
      &test_filter, string_hash, string_equal);

  enum cn_test_gen_progress progress_level = CN_TEST_GEN_PROGRESS_ALL;
  uint64_t seed = bennet_rand();
  enum cn_logging_level logging_level = CN_LOGGING_ERROR;
  int timeout = 0;
  int input_timeout = 1000;
  int exit_fast = 0;
  int trap = 0;
  enum bennet_sizing_strategy sizing_strategy = BENNET_SIZE_QUICKCHECK;
  int replay = 1;
  int replicas = 1;
  int print_seed = 0;
  bool output_tyche = false;
  FILE* tyche_output_stream = NULL;
  bool print_size_info = false;
  bool print_backtrack_info = false;
  bool print_satisfaction_info = false;
  bool print_discard_info = false;
  bool print_timing_info = false;

  for (int i = 0; i < argc; i++) {
    char* arg = argv[i];

    if (strcmp("-S", arg) == 0 || strcmp("--seed", arg) == 0) {
      seed = strtoull(argv[i + 1], NULL, 16);
      i++;
    } else if (strcmp("--logging-level", arg) == 0) {
      char* next = argv[i + 1];
      if (strcmp("none", next) == 0) {
        logging_level = CN_LOGGING_NONE;
      } else if (strcmp("error", next) == 0) {
        logging_level = CN_LOGGING_ERROR;
      } else if (strcmp("info", next) == 0) {
        logging_level = CN_LOGGING_INFO;
      } else {
        logging_level = strtol(next, NULL, 10);
      }

      i++;
    } else if (strcmp("--trace-granularity", arg) == 0) {
      enum cn_trace_granularity granularity;

      char* next = argv[i + 1];
      if (strcmp("none", next) == 0) {
        granularity = CN_TRACE_NONE;
      } else if (strcmp("ends", next) == 0) {
        granularity = CN_TRACE_ENDS;
      } else if (strcmp("all", next) == 0) {
        granularity = CN_TRACE_ALL;
      } else {
        granularity = strtol(next, NULL, 10);
      }

      set_cn_trace_granularity(granularity);
      i++;
    } else if (strcmp("--progress-level", arg) == 0) {
      char* next = argv[i + 1];
      if (strcmp("silent", next) == 0) {
        progress_level = CN_TEST_GEN_PROGRESS_NONE;
      } else if (strcmp("function", next) == 0) {
        progress_level = CN_TEST_GEN_PROGRESS_FINAL;
      } else if (strcmp("testcase", next) == 0) {
        progress_level = CN_TEST_GEN_PROGRESS_ALL;
      } else {
        progress_level = strtol(next, NULL, 10);
      }

      i++;
    } else if (strcmp("--input-timeout", arg) == 0) {
      input_timeout = strtol(argv[i + 1], NULL, 10);
      i++;
    } else if (strcmp("--null-in-every", arg) == 0) {
      set_null_in_every(strtol(argv[i + 1], NULL, 10));
      i++;
    } else if (strcmp("--until-timeout", arg) == 0) {
      timeout = strtol(argv[i + 1], NULL, 10);
      i++;
    } else if (strcmp("--exit-fast", arg) == 0) {
      exit_fast = 1;
    } else if (strcmp("--max-stack-depth", arg) == 0) {
      bennet_set_max_depth(strtoul(argv[i + 1], NULL, 10));
      i++;
    } else if (strcmp("--max-generator-size", arg) == 0) {
      uint64_t sz = strtoul(argv[i + 1], NULL, 10);
      assert(sz != 0);
      bennet_set_max_size(sz);
      i++;
    } else if (strcmp("--allowed-size-split-backtracks", arg) == 0) {
      bennet_set_size_split_backtracks_allowed(strtoul(argv[i + 1], NULL, 10));
      i++;
    } else if (strcmp("--trap", arg) == 0) {
      trap = 1;
    } else if (strcmp("--sizing-strategy", arg) == 0) {
      char* next = argv[i + 1];
      if (strcmp("uniform", next) == 0) {
        sizing_strategy = BENNET_SIZE_UNIFORM;
      } else if (strcmp("constant", next) == 0) {
        sizing_strategy = BENNET_SIZE_CONSTANT;
      } else if (strcmp("quickcheck", next) == 0) {
        sizing_strategy = BENNET_SIZE_QUICKCHECK;
      } else {
        sizing_strategy = strtoul(next, NULL, 10);
      }

      i++;
    } else if (strcmp("--no-replays", arg) == 0) {
      replay = 0;
    } else if (strcmp("--no-replicas", arg) == 0) {
      replicas = 0;
    } else if (strcmp("--print-seed", arg) == 0) {
      print_seed = 1;
    } else if (strcmp("--output-tyche", arg) == 0) {
      char* next = argv[i + 1];
      tyche_output_stream = fopen(next, "w");
      if (tyche_output_stream != NULL) {
        output_tyche = true;
      }
    } else if (strcmp("--print-backtrack-info", arg) == 0) {
      print_backtrack_info = true;
    } else if (strcmp("--print-satisfaction-info", arg) == 0) {
      print_satisfaction_info = true;
    } else if (strcmp("--print-size-info", arg) == 0) {
      print_size_info = true;
    } else if (strcmp("--print-discard-info", arg) == 0) {
      print_discard_info = true;
    } else if (strcmp("--print-timing-info", arg) == 0) {
      print_timing_info = true;
    } else if (strcmp("--smt-pruning-at-runtime", arg) == 0) {
      cn_smt_pruning_at_runtime = true;
    } else if (strcmp("--use-solver-eval", arg) == 0) {
      cn_set_use_solver_eval(true);
    } else if (strcmp("--smt-skew-pointer-order", arg) == 0) {
      cn_smt_skew_pointer_order = true;
    } else if (strcmp("--smt-skewing", arg) == 0) {
      char* next = argv[i + 1];
      if (strcmp("uniform", next) == 0) {
        cn_set_smt_skewing_mode(CN_SMT_SKEWING_UNIFORM);
      } else if (strcmp("sized", next) == 0) {
        cn_set_smt_skewing_mode(CN_SMT_SKEWING_SIZED);
      } else if (strcmp("none", next) == 0) {
        cn_set_smt_skewing_mode(CN_SMT_SKEWING_NONE);
      } else {
        fprintf(stderr, "Error: Invalid --smt-skewing mode: %s\n", next);
        fprintf(stderr, "Valid modes: uniform, sized, none\n");
        exit(1);
      }
      i++;
    } else if (strcmp("--smt-logging", arg) == 0) {
      cn_smt_set_log_file_path(argv[i + 1]);
      i++;
    } else if (strcmp("--smt-log-unsat-cores", arg) == 0) {
      cn_smt_set_unsat_core_log_path(argv[i + 1]);
      i++;
    } else if (strcmp("--max-bump-blocks", arg) == 0) {
      cn_bump_set_max_blocks(strtoul(argv[i + 1], NULL, 10));
      i++;
    } else if (strcmp("--bump-block-size", arg) == 0) {
      cn_bump_set_block_size(strtoul(argv[i + 1], NULL, 10));
      i++;
    } else if (strcmp("--max-input-alloc", arg) == 0) {
      bennet_rand_alloc_set_mem_size(strtoul(argv[i + 1], NULL, 10));
      i++;
    } else if (strcmp("--only", arg) == 0) {
      char* test_names = argv[i + 1];
      char* test_names_copy = strdup(test_names);
      assert(test_names_copy != NULL);

      // Parse comma-separated test names
      char* token = strtok(test_names_copy, ",");
      while (token != NULL) {
        // Trim leading/trailing whitespace
        while (*token == ' ' || *token == '\t')
          token++;
        char* end = token + strlen(token) - 1;
        while (end > token && (*end == ' ' || *end == '\t'))
          end--;
        *(end + 1) = '\0';

        // Add to filter (store the original token pointer)
        char* stored_name = strdup(token);
        assert(stored_name != NULL);
        bennet_hash_table_set(const_char_ptr, uint8_t)(&test_filter, stored_name, true);

        token = strtok(NULL, ",");
      }

      free(test_names_copy);
      i++;
    }
  }

  if (output_tyche || print_size_info) {
    bennet_info_sizes_init();
  }

  if (output_tyche || print_backtrack_info) {
    bennet_info_backtracks_init();
  }

  if (print_satisfaction_info) {
    bennet_info_unsatisfied_init();
  }

  if (print_discard_info) {
    bennet_info_discards_init();
  }

  if (output_tyche || print_timing_info) {
    bennet_info_timing_init();
  }

  if (timeout != 0) {
    printf("Running until timeout of %d seconds\n", timeout);
  }

  if (print_seed) {
    printf("Using seed: %016" PRIx64 "\n", seed);
  }

  bennet_srand(seed);
  bennet_rand();  // Junk to get something to make a checkpoint from

  struct cn_test_reproduction* repros =
      calloc(CN_TEST_MAX_TEST_CASES, sizeof(struct cn_test_reproduction));
  assert(repros);
  enum cn_test_result* results =
      malloc(CN_TEST_MAX_TEST_CASES * sizeof(enum cn_test_result));
  assert(results);
  memset(results, CN_TEST_SKIP, CN_TEST_MAX_TEST_CASES * sizeof(enum cn_test_result));

  int timediff = 0;

  do {
    for (int i = 0; i < num_test_cases; i++) {
      if (results[i] == CN_TEST_FAIL) {
        continue;
      }

      // Skip tests not in the filter if filter is non-empty
      if (bennet_hash_table_size(const_char_ptr, uint8_t)(&test_filter) > 0) {
        if (!bennet_hash_table_contains(const_char_ptr, uint8_t)(
                &test_filter, test_cases[i].name)) {
          continue;
        }
      }

      if (output_tyche || print_size_info) {
        bennet_info_sizes_set_function_under_test(test_cases[i].name);
      }

      if (output_tyche || print_backtrack_info) {
        bennet_info_backtracks_set_function_under_test(test_cases[i].name);
      }

      if (print_satisfaction_info) {
        bennet_info_unsatisfied_set_function_under_test(test_cases[i].name);
      }

      if (print_discard_info) {
        bennet_info_discards_set_function_under_test(test_cases[i].name);
      }

      if (output_tyche || print_timing_info) {
        bennet_info_timing_set_function_under_test(test_cases[i].name);
      }

      struct cn_test_case* test_case = &test_cases[i];
      if (progress_level == CN_TEST_GEN_PROGRESS_ALL) {
        print_test_info(test_case->suite, test_case->name, 0, 0);
      }
      repros[i].checkpoint = bennet_rand_save();
      bennet_set_input_timeout(input_timeout);
      struct cn_test_input test_input = {.replay = false,
          .progress_level = progress_level,
          .sizing_strategy = sizing_strategy,
          .trap = 0,
          .replicas = 0,
          .log_all_backtracks = 0,
          .output_tyche = output_tyche,
          .tyche_output_stream = tyche_output_stream,
          .begin_time = begin_time};
      enum cn_test_result result = test_case->func(test_input);
      if (!(results[i] == CN_TEST_PASS && result == CN_TEST_GEN_FAIL)) {
        results[i] = result;
      }
      repros[i].size = bennet_get_size();
      if (progress_level == CN_TEST_GEN_PROGRESS_NONE) {
        continue;
      }

      printf("\n");
      switch (result) {
        case CN_TEST_PASS:
          printf("PASSED\n");
          break;
        case CN_TEST_FAIL:
          printf("FAILED\n");

          if (replay) {
            set_cn_logging_level(logging_level);
            cn_printf(CN_LOGGING_ERROR, "\n");

            cn_test_reproduce(&repros[i]);
            test_input.replay = true;
            test_input.progress_level = CN_TEST_GEN_PROGRESS_NONE;
            test_input.trap = trap;
            test_input.replicas = replicas;
            test_input.output_tyche = 0;
            enum cn_test_result replay_result = test_case->func(test_input);

            if (replay_result != CN_TEST_FAIL) {
              if (get_cn_logging_level() < CN_LOGGING_ERROR) {
                printf("\n");
              }
              fprintf(stderr,
                  "Replay of failure did not fail (result = %d).\n",
                  replay_result);
              abort();
            }

            set_cn_logging_level(CN_LOGGING_NONE);
          }

          break;
        case CN_TEST_GEN_FAIL:
          printf("FAILED TO GENERATE VALID INPUT\n");
          break;
        case CN_TEST_SKIP:
          printf("SKIPPED\n");
          break;
      }

      if (exit_fast && result == CN_TEST_FAIL) {
        goto outside_loop;
      }

      if (timeout != 0) {
        timediff = (bennet_get_microseconds() - begin_time) / 1000000;
      }
    }
    if (timediff < timeout) {
      printf("\n%d seconds remaining, rerunning tests\n\n", timeout - timediff);
    }
  } while (timediff < timeout);

outside_loop:;
  if (tyche_output_stream != NULL) {
    fclose(tyche_output_stream);
  }

  int passed = 0;
  int failed = 0;
  int errored = 0;
  int skipped = 0;

  for (int i = 0; i < num_test_cases; i++) {
    switch (results[i]) {
      case CN_TEST_PASS:
        passed++;
        break;
      case CN_TEST_FAIL:
        failed++;
        break;
      case CN_TEST_GEN_FAIL:
        errored++;
        break;
      case CN_TEST_SKIP:
        skipped++;
        break;
    }
  }

  printf("\nTesting Summary:\n");
  printf("cases: %d, passed: %d, failed: %d, errored: %d, skipped: %d\n",
      num_test_cases,
      passed,
      failed,
      errored,
      skipped);

  if (print_size_info) {
    printf("\n");

    bennet_info_sizes_print_info();
  }

  if (print_backtrack_info) {
    printf("\n");

    bennet_info_backtracks_print_backtrack_info();
  }

  if (print_satisfaction_info) {
    printf("\n");

    bennet_info_unsatisfied_print_info();
  }

  if (print_discard_info) {
    printf("\n");

    bennet_info_discards_print_info();
  }

  if (print_timing_info) {
    printf("\n");

    bennet_info_timing_print_info();
  }

  free(repros);
  free(results);

  // Clean up test filter hash table
  bennet_hash_table_free(const_char_ptr, uint8_t)(&test_filter);

  return !(failed == 0 && errored == 0);
}
