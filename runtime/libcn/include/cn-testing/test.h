#ifndef CN_TEST_H
#define CN_TEST_H

#include <sys/time.h>

#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <time.h>

#include <bennet/prelude.h>
#include <cn-testing/result.h>
#include <cn-replicate/lines.h>
#include <cn-replicate/shape.h>
#include <cn-smt/memory/std_alloc.h>
#include <cn-smt/memory/test_alloc.h>

enum cn_test_gen_progress {
  CN_TEST_GEN_PROGRESS_NONE = 0,
  CN_TEST_GEN_PROGRESS_FINAL = 1,
  CN_TEST_GEN_PROGRESS_ALL = 2
};

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

typedef enum cn_test_result cn_test_case_fn(struct cn_test_input test_input);

void cn_register_test_case(const char* suite, const char* name, cn_test_case_fn* func);

void print_test_info(const char* suite, const char* name, int tests, int discards);

/** This function is called right before rerunning a failing test case. */
void cn_trap(void);

size_t bennet_compute_size(enum bennet_sizing_strategy strategy,
    int max_tests,
    int max_discard_ratio,
    int successes,
    int recent_discards);

#define CN_UNIT_TEST_NAME(Name) cn_test_const_##Name

#define CN_EXTERN_UNIT_TEST_NAME(Name) CN_UNIT_TEST_NAME(Name)

#define CN_STATIC_UNIT_TEST_NAME(Name, File) cn_test_const_##File##_##Name

#define CN_UNIT_TEST_CASE(Name, FuncName)                                                \
  static jmp_buf buf_##FuncName;                                                         \
                                                                                         \
  void cn_test_const_##FuncName##_fail(                                                  \
      enum cn_failure_mode failure_mode, enum spec_mode spec_mode) {                     \
    longjmp(buf_##FuncName, 1);                                                          \
  }                                                                                      \
                                                                                         \
  enum cn_test_result cn_test_const_##FuncName(struct cn_test_input _input) {            \
    if (setjmp(buf_##FuncName)) {                                                        \
      return CN_TEST_FAIL;                                                               \
    }                                                                                    \
    set_cn_failure_cb(&cn_test_const_##FuncName##_fail);                                 \
                                                                                         \
    CN_TEST_INIT();                                                                      \
    FuncName();                                                                          \
                                                                                         \
    return CN_TEST_PASS;                                                                 \
  }

#define CN_EXTERN_UNIT_TEST_CASE(Name) CN_UNIT_TEST_CASE(Name, Name)

#define CN_STATIC_UNIT_TEST_CASE(Name, File) CN_UNIT_TEST_CASE(Name, File##_##Name)

#define CN_REGISTER_UNIT_TEST_CASE(Suite, Name, FuncName)                                \
  cn_register_test_case(#Suite, #Name, &CN_UNIT_TEST_CASE_NAME(FuncName));

#define CN_REGISTER_EXTERN_UNIT_TEST_CASE(Suite, Name)                                   \
  cn_register_test_case(#Suite, #Name, &CN_EXTERN_UNIT_TEST_NAME(Name));

#define CN_REGISTER_STATIC_UNIT_TEST_CASE(Suite, Name, File)                             \
  cn_register_test_case(#Suite, #Name, &CN_STATIC_UNIT_TEST_NAME(Name, File));

#define CN_RANDOM_TEST_CASE_WITH_CUSTOM_INIT(Suite, Name, FuncName, Samples, Init, ...)  \
  static jmp_buf buf_##FuncName;                                                         \
                                                                                         \
  void cn_test_gen_##FuncName##_fail(                                                    \
      enum cn_failure_mode failure_mode, enum spec_mode spec_mode) {                     \
    longjmp(buf_##FuncName, failure_mode);                                               \
  }                                                                                      \
                                                                                         \
  enum cn_test_result cn_test_gen_##FuncName(struct cn_test_input test_input) {          \
    volatile bennet_rand_checkpoint checkpoint = bennet_rand_save();                     \
    volatile int i = 0, d = 0, recentDiscards = 0;                                       \
    volatile bool successful_gen = false;                                                \
    volatile void* gen_state = NULL;                                                     \
    set_cn_failure_cb(&cn_test_gen_##FuncName##_fail);                                   \
    switch (setjmp(buf_##FuncName)) {                                                    \
      case CN_FAILURE_ASSERT:                                                            \
      case CN_FAILURE_CHECK_OWNERSHIP:                                                   \
      case CN_FAILURE_OWNERSHIP_LEAK:                                                    \
      case CN_FAILURE_GHOST_ARGS:                                                        \
        bennet_info_timing_end("execute:test");                                          \
                                                                                         \
        if (test_input.progress_level == CN_TEST_GEN_PROGRESS_FINAL) {                   \
          print_test_info(#Suite, #Name, i, d);                                          \
        }                                                                                \
                                                                                         \
        if (test_input.output_tyche) {                                                   \
          struct tyche_line_info line_info = {.test_suite = #Suite,                      \
              .test_name = #Name,                                                        \
              .status = "failed",                                                        \
              .status_reason = "TODO: Fulminate error message",                          \
              .suite_begin_time = test_input.begin_time,                                 \
              .representation = cn_replica_lines_to_json_literal()};                     \
          print_test_summary_tyche(test_input.tyche_output_stream, &line_info);          \
        }                                                                                \
                                                                                         \
        if (test_input.replicas) {                                                       \
          printf("********************** Failing input ***********************\n\n");    \
          printf("%s", cn_replica_lines_to_str());                                       \
          printf("\n************************************************************\n\n");  \
        }                                                                                \
                                                                                         \
        return CN_TEST_FAIL;                                                             \
      case CN_FAILURE_FULM_ALLOC:                                                        \
        bennet_info_timing_end("bennet");                                                \
        bennet_info_timing_end("execute:test");                                          \
                                                                                         \
        d++;                                                                             \
        recentDiscards++;                                                                \
                                                                                         \
        /* TODO: Add to discard data collection */                                       \
                                                                                         \
        bennet_info_backtracks_end_run(true);                                            \
        bennet_info_unsatisfied_end_run(true);                                           \
                                                                                         \
        if (test_input.output_tyche) {                                                   \
          struct tyche_line_info line_info = {.test_suite = #Suite,                      \
              .test_name = #Name,                                                        \
              .status = "gave_up",                                                       \
              .status_reason = "Allocation failed",                                      \
              .suite_begin_time = test_input.begin_time,                                 \
              .representation =                                                          \
                  (successful_gen) ? cn_replica_lines_to_json_literal() : ""};           \
          print_test_summary_tyche(test_input.tyche_output_stream, &line_info);          \
        }                                                                                \
                                                                                         \
        break;                                                                           \
    }                                                                                    \
    for (; i < Samples; i++) {                                                           \
      if (test_input.progress_level == CN_TEST_GEN_PROGRESS_ALL) {                       \
        printf("\r");                                                                    \
        print_test_info(#Suite, #Name, i, d);                                            \
      }                                                                                  \
      if (d == 10 * Samples) {                                                           \
        if (test_input.progress_level == CN_TEST_GEN_PROGRESS_FINAL) {                   \
          print_test_info(#Suite, #Name, i, d);                                          \
        }                                                                                \
        return CN_TEST_GEN_FAIL;                                                         \
      }                                                                                  \
      if (!test_input.replay) {                                                          \
        bennet_set_size(bennet_compute_size(                                             \
            test_input.sizing_strategy, Samples, 10, i, recentDiscards));                \
        bennet_rand_replace(checkpoint);                                                 \
      }                                                                                  \
      CN_TEST_INIT();                                                                    \
      if (!test_input.replay) {                                                          \
        bennet_set_input_timer(bennet_get_milliseconds());                               \
      } else {                                                                           \
        bennet_set_input_timeout(0);                                                     \
      }                                                                                  \
                                                                                         \
      bennet_info_backtracks_begin_run();                                                \
      bennet_info_unsatisfied_begin_run();                                               \
      successful_gen = false;                                                            \
                                                                                         \
      cn_test_generator_##FuncName##_record* res =                                       \
          cn_test_generator_##FuncName((void**)&gen_state);                              \
      bennet_info_timing_end("bennet");                                                  \
                                                                                         \
      if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                    \
        i--;                                                                             \
        d++;                                                                             \
        recentDiscards++;                                                                \
                                                                                         \
        bennet_info_discards_log(bennet_failure_get_failure_type());                     \
                                                                                         \
        bennet_info_backtracks_end_run(true);                                            \
        bennet_info_unsatisfied_end_run(true);                                           \
                                                                                         \
        if (test_input.output_tyche) {                                                   \
          struct tyche_line_info line_info = {.test_suite = #Suite,                      \
              .test_name = #Name,                                                        \
              .status = "gave_up",                                                       \
              .status_reason =                                                           \
                  (bennet_failure_get_failure_type() == BENNET_FAILURE_TIMEOUT)          \
                      ? "Generation timed out"                                           \
                      : "Generation backtracked all the way to the top",                 \
              .suite_begin_time = test_input.begin_time,                                 \
              .representation = ""};                                                     \
          print_test_summary_tyche(test_input.tyche_output_stream, &line_info);          \
        }                                                                                \
                                                                                         \
        continue;                                                                        \
      }                                                                                  \
      successful_gen = true;                                                             \
      bennet_info_sizes_log();                                                           \
      bennet_info_backtracks_end_run(test_input.log_all_backtracks);                     \
      bennet_info_unsatisfied_end_run(test_input.log_all_backtracks);                    \
                                                                                         \
      (void)Init(res);                                                                   \
      assume_##FuncName(__VA_ARGS__);                                                    \
      if (test_input.replicas || test_input.output_tyche) {                              \
        cn_replica_alloc_reset();                                                        \
        cn_replica_lines_reset();                                                        \
                                                                                         \
        cn_analyze_shape_##FuncName(__VA_ARGS__);                                        \
        cn_replicate_##FuncName(__VA_ARGS__);                                            \
      }                                                                                  \
                                                                                         \
      if (test_input.trap) {                                                             \
        cn_trap();                                                                       \
      }                                                                                  \
      bennet_info_timing_start("execute:test");                                          \
      (void)FuncName(__VA_ARGS__);                                                       \
      bennet_info_timing_end("execute:test");                                            \
      if (test_input.replay) {                                                           \
        return CN_TEST_PASS;                                                             \
      }                                                                                  \
      recentDiscards = 0;                                                                \
      if (test_input.output_tyche) {                                                     \
        struct tyche_line_info line_info = {.test_suite = #Suite,                        \
            .test_name = #Name,                                                          \
            .status = "passed",                                                          \
            .status_reason = "",                                                         \
            .suite_begin_time = test_input.begin_time,                                   \
            .representation = cn_replica_lines_to_json_literal()};                       \
        print_test_summary_tyche(test_input.tyche_output_stream, &line_info);            \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    if (test_input.progress_level != CN_TEST_GEN_PROGRESS_NONE) {                        \
      if (test_input.progress_level == CN_TEST_GEN_PROGRESS_ALL) {                       \
        printf("\r");                                                                    \
      }                                                                                  \
      print_test_info(#Suite, #Name, i, d);                                              \
    }                                                                                    \
    return CN_TEST_PASS;                                                                 \
  }

#define CN_RANDOM_TEST_CASE_WITH_INIT(Suite, Name, FuncName, Samples, ...)               \
  CN_RANDOM_TEST_CASE_WITH_CUSTOM_INIT(Suite,                                            \
      Name,                                                                              \
      FuncName,                                                                          \
      Samples,                                                                           \
      cn_test_gen_##FuncName##_cn_test_setup,                                            \
      __VA_ARGS__)

#define CN_EXTERN_RANDOM_TEST_CASE_WITH_INIT(Suite, Name, Samples, ...)                  \
  CN_RANDOM_TEST_CASE_WITH_INIT(Suite, Name, Name, Samples, __VA_ARGS__)

#define CN_STATIC_RANDOM_TEST_CASE_WITH_INIT(Suite, Name, File, Samples, ...)            \
  CN_RANDOM_TEST_CASE_WITH_INIT(Suite, Name, File##_##Name, Samples, __VA_ARGS__)

#define CN_RANDOM_TEST_NAME(Name) cn_test_gen_##Name

#define CN_EXTERN_RANDOM_TEST_NAME(Name) CN_RANDOM_TEST_NAME(Name)

#define CN_STATIC_RANDOM_TEST_NAME(Name, File) cn_test_gen_##File##_##Name

#define CN_RANDOM_TEST_CASE(Suite, Name, FuncName, Samples, ...)                         \
  CN_RANDOM_TEST_CASE_WITH_CUSTOM_INIT(Suite, Name, FuncName, Samples, , __VA_ARGS__)

#define CN_EXTERN_RANDOM_TEST_CASE(Suite, Name, Samples, ...)                            \
  CN_RANDOM_TEST_CASE(Suite, Name, Name, Samples, __VA_ARGS__)

#define CN_STATIC_RANDOM_TEST_CASE(Suite, Name, File, Samples, ...)                      \
  CN_RANDOM_TEST_CASE(Suite, Name, File##_##Name, Samples, __VA_ARGS__)

#define CN_REGISTER_RANDOM_TEST_CASE(Suite, Name, FuncName)                              \
  cn_register_test_case(#Suite, #Name, &CN_RANDOM_TEST_CASE_NAME(FuncName));

#define CN_REGISTER_EXTERN_RANDOM_TEST_CASE(Suite, Name)                                 \
  cn_register_test_case(#Suite, #Name, &CN_EXTERN_RANDOM_TEST_NAME(Name));

#define CN_REGISTER_STATIC_RANDOM_TEST_CASE(Suite, Name, File)                           \
  cn_register_test_case(#Suite, #Name, &CN_STATIC_RANDOM_TEST_NAME(Name, File));

#define CN_RANDOM_TEST_CASE(Suite, Name, FuncName, Samples, ...)                         \
  CN_RANDOM_TEST_CASE_WITH_CUSTOM_INIT(Suite, Name, FuncName, Samples, , __VA_ARGS__)

#define CN_EXTERN_SYMBOLIC_TEST_CASE(Suite, Name, Samples, ...)                          \
  CN_RANDOM_TEST_CASE(Suite, Name, Name, Samples, __VA_ARGS__)

#define CN_STATIC_SYMBOLIC_TEST_CASE(Suite, Name, File, Samples, ...)                    \
  CN_RANDOM_TEST_CASE(Suite, Name, File##_##Name, Samples, __VA_ARGS__)

#define CN_REGISTER_EXTERN_SYMBOLIC_TEST_CASE(Suite, Name)                               \
  cn_register_test_case(#Suite, #Name, &CN_EXTERN_RANDOM_TEST_NAME(Name));

#define CN_REGISTER_STATIC_SYMBOLIC_TEST_CASE(Suite, Name, File)                         \
  cn_register_test_case(#Suite, #Name, &CN_STATIC_RANDOM_TEST_NAME(Name, File));

int cn_test_main(int argc, char* argv[]);

void fulminate_destroy(void);
void fulminate_init(void);
void bennet_destroy(void);
void bennet_init(void);
void cn_smt_destroy(void);
void cn_smt_init(void);

#define CN_TEST_INIT()                                                                   \
  std_set_default_alloc();                                                               \
  cn_smt_destroy();                                                                      \
  bennet_destroy();                                                                      \
  fulminate_destroy();                                                                   \
  cn_test_free_all();                                                                    \
  fulminate_init();                                                                      \
  bennet_init();                                                                         \
  cn_smt_init();

#endif  // CN_TEST_H
