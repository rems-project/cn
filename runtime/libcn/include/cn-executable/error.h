#define cn_printf(level, ...)                                                            \
  if (get_cn_logging_level() >= level) {                                                 \
    printf(__VA_ARGS__);                                                                 \
  }

  /* Error handlers */


enum cn_logging_level {
  CN_LOGGING_NONE = 0,
  CN_LOGGING_ERROR = 1,
  CN_LOGGING_INFO = 2
};

enum cn_logging_level get_cn_logging_level(void);

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

void cn_print_nr_owned_predicates(void);

struct cn_error_message_info {
  const char* function_name;
  char* file_name;
  int line_number;
  char* cn_source_loc;
  struct cn_error_message_info* parent;
  struct cn_error_message_info* child;
};

void initialise_error_msg_info_(
    const char* function_name, char* file_name, int line_number);

#define initialise_error_msg_info()                                                      \
  initialise_error_msg_info_(__func__, __FILE__, __LINE__)

void reset_error_msg_info();
void free_error_msg_info();

void update_error_message_info_(
    const char* function_name, char* file_name, int line_number, char* cn_source_loc);

void cn_pop_msg_info();

#define update_cn_error_message_info(x)                                                  \
  update_error_message_info_(__func__, __FILE__, __LINE__ + 1, x)

#define update_cn_error_message_info_access_check(x)                                     \
  update_error_message_info_(__func__, __FILE__, __LINE__, x)
