#ifndef FULMINATE_API_H
#define FULMINATE_API_H

#include <cn-executable/eval.h>
#include <cn-executable/rts_deps.h>

void fulminate_init(void);
void fulminate_destroy(void);

enum cn_logging_level {
  CN_LOGGING_NONE = 0,
  CN_LOGGING_ERROR = 1,
  CN_LOGGING_INFO = 2
};

enum cn_logging_level get_cn_logging_level(void);

#define cn_printf(level, ...)                                                            \
  if (get_cn_logging_level() >= level) {                                                 \
    printf(__VA_ARGS__);                                                                 \
  }

/** Sets the logging level, returning the previous one */
enum cn_logging_level set_cn_logging_level(enum cn_logging_level new_level);

enum cn_trace_granularity {
  CN_TRACE_NONE = 0,
  CN_TRACE_ENDS = 1,
  CN_TRACE_ALL = 2,
};

enum cn_trace_granularity get_cn_trace_granularity(void);

/** Sets the trace granularity, returning the previous one */
enum cn_trace_granularity set_cn_trace_granularity(
    enum cn_trace_granularity new_granularity);

/* cn_failure callbacks */
enum cn_failure_mode {
  CN_FAILURE_ASSERT = 1,
  CN_FAILURE_CHECK_OWNERSHIP,
  CN_FAILURE_OWNERSHIP_LEAK,
  CN_FAILURE_FULM_ALLOC,
  CN_FAILURE_USER_ALLOC,
  CN_FAILURE_GHOST_ARGS
};

enum spec_mode {
  PRE = 1,
  POST = 2,
  LOOP = 3,
  STATEMENT = 4,
  C_ACCESS = 5,
  NON_SPEC = 6
};

typedef void (*cn_failure_callback)(enum cn_failure_mode, enum spec_mode);
void set_cn_failure_cb(cn_failure_callback callback);
void reset_cn_failure_cb(void);

void cn_failure(enum cn_failure_mode failure_mode, enum spec_mode spec_mode);

// FIXME: Include `assume` functions in Fulminate instrumented file

void cn_assume_ownership(void* generic_c_ptr, unsigned long size, char* fun);

struct loop_ownership;
void cn_get_or_put_ownership(enum spec_mode spec_mode,
    void* generic_c_ptr,
    size_t size,
    struct loop_ownership* loop_ownership);

void update_error_message_info_(
    const char* function_name, char* file_name, int line_number, char* cn_source_loc);

#define update_cn_error_message_info(x)                                                  \
  update_error_message_info_(__func__, __FILE__, __LINE__ + 1, x)

void cn_pop_msg_info(void);

void cn_assert(cn_bool* cn_b, enum spec_mode spec_mode);

// END FIXME

#endif  // FULMINATE_API_H
