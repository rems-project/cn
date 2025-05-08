#ifndef CN_TEST_H
#define CN_TEST_H

#include <sys/time.h>

#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <time.h>

#include <cn-executable/utils.h>
#include <cn-testing/result.h>
#include <cn-testing/uniform.h>
#include <cn-replicate/lines.h>
#include <cn-replicate/shape.h>

enum cn_test_gen_progress {
  CN_TEST_GEN_PROGRESS_NONE = 0,
  CN_TEST_GEN_PROGRESS_FINAL = 1,
  CN_TEST_GEN_PROGRESS_ALL = 2
};

enum cn_gen_sizing_strategy {
  CN_GEN_SIZE_UNIFORM = 0,
  CN_GEN_SIZE_QUARTILE = 1,
  CN_GEN_SIZE_QUICKCHECK = 2
};

struct cn_test_input {
  bool replay;
  enum cn_test_gen_progress progress_level;
  enum cn_gen_sizing_strategy sizing_strategy;
  bool trap;
  bool replicas;
  bool output_tyche;
  FILE* tyche_output_stream;
  uint64_t begin_time;
};

typedef enum cn_test_result cn_test_case_fn(struct cn_test_input test_input);

void cn_register_test_case(const char* suite, const char* name, cn_test_case_fn* func);

void print_test_info(const char* suite, const char* name, int tests, int discards);

/** This function is called right before rerunning a failing test case. */
void cn_trap(void);

size_t cn_gen_compute_size(enum cn_gen_sizing_strategy strategy,
    int max_tests,
    size_t max_size,
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
    struct timeval start_time, end_time;                                                 \
    cn_gen_rand_checkpoint checkpoint = cn_gen_rand_save();                              \
    int i = 0, d = 0, recentDiscards = 0;                                                \
    set_cn_failure_cb(&cn_test_gen_##FuncName##_fail);                                   \
    switch (setjmp(buf_##FuncName)) {                                                    \
      case CN_FAILURE_ASSERT:                                                            \
      case CN_FAILURE_CHECK_OWNERSHIP:                                                   \
      case CN_FAILURE_OWNERSHIP_LEAK:                                                    \
        gettimeofday(&end_time, NULL);                                                   \
        if (test_input.progress_level == CN_TEST_GEN_PROGRESS_FINAL) {                   \
          print_test_info(#Suite, #Name, i, d);                                          \
        }                                                                                \
                                                                                         \
        if (test_input.replicas && test_input.output_tyche) {                            \
          int64_t runtime = timediff_timeval(&start_time, &end_time);                    \
          struct tyche_line_info line_info = {.test_suite = #Suite,                      \
              .test_name = #Name,                                                        \
              .status = "failed",                                                        \
              .suite_begin_time = test_input.begin_time,                                 \
              .representation = cn_replica_lines_to_json_literal(),                      \
              .init_time = 0,                                                            \
              .runtime = runtime};                                                       \
          print_test_summary_tyche(test_input.tyche_output_stream, &line_info);          \
        }                                                                                \
        if (test_input.replicas) {                                                       \
          printf("********************** Failing input ***********************\n\n");    \
          printf("%s", cn_replica_lines_to_str());                                       \
          printf("\n************************************************************\n\n");  \
        }                                                                                \
                                                                                         \
        return CN_TEST_FAIL;                                                             \
      case CN_FAILURE_ALLOC:                                                             \
        d++;                                                                             \
        recentDiscards++;                                                                \
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
        cn_gen_set_size(cn_gen_compute_size(test_input.sizing_strategy,                  \
            Samples,                                                                     \
            cn_gen_get_max_size(),                                                       \
            10,                                                                          \
            i,                                                                           \
            recentDiscards));                                                            \
        cn_gen_rand_replace(checkpoint);                                                 \
      }                                                                                  \
      CN_TEST_INIT();                                                                    \
      if (!test_input.replay) {                                                          \
        cn_gen_set_input_timer(cn_gen_get_milliseconds());                               \
      } else {                                                                           \
        cn_gen_set_input_timeout(0);                                                     \
      }                                                                                  \
      cn_gen_##FuncName##_record* res = cn_gen_##FuncName();                             \
      if (cn_gen_backtrack_type() != CN_GEN_BACKTRACK_NONE) {                            \
        i--;                                                                             \
        d++;                                                                             \
        recentDiscards++;                                                                \
        continue;                                                                        \
      }                                                                                  \
      assume_##FuncName(__VA_ARGS__);                                                    \
      Init(res);                                                                         \
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
      gettimeofday(&start_time, NULL);                                                   \
      (void)FuncName(__VA_ARGS__);                                                       \
      if (test_input.replay) {                                                           \
        return CN_TEST_PASS;                                                             \
      }                                                                                  \
      recentDiscards = 0;                                                                \
      if (!test_input.replay && test_input.output_tyche) {                               \
        gettimeofday(&end_time, NULL);                                                   \
        int64_t runtime = timediff_timeval(&start_time, &end_time);                      \
        struct tyche_line_info line_info = {.test_suite = #Suite,                        \
            .test_name = #Name,                                                          \
            .status = "passed",                                                          \
            .suite_begin_time = test_input.begin_time,                                   \
            .representation = cn_replica_lines_to_json_literal(),                        \
            .init_time = 0,                                                              \
            .runtime = runtime};                                                         \
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
  CN_RANDOM_TEST_CASE_WITH_CUSTOM_INIT(                                                  \
      Suite, Name, FuncName, Samples, cn_test_gen_##FuncName##_init, __VA_ARGS__)

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

int cn_test_main(int argc, char* argv[]);

#define CN_TEST_INIT()                                                                   \
  reset_fulminate();                                                                     \
  cn_gen_backtrack_reset();                                                              \
  cn_gen_alloc_reset();                                                                  \
  cn_gen_ownership_reset();

#endif  // CN_TEST_H
