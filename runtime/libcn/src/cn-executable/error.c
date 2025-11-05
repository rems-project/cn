#include <cn-executable/error.h>

enum cn_logging_level get_cn_logging_level(void) {
  return logging_level;
}

enum cn_logging_level set_cn_logging_level(enum cn_logging_level new_level) {
  enum cn_logging_level old_level = logging_level;
  logging_level = new_level;
  return old_level;
}

void cn_failure_default(enum cn_failure_mode failure_mode, enum spec_mode spec_mode) {
  int exit_code =
      spec_mode;  // Might need to differentiate between failure modes via this exit code in the future
  switch (failure_mode) {
    case CN_FAILURE_ALLOC:
      printf("Out of memory!");
    case CN_FAILURE_ASSERT:
    case CN_FAILURE_CHECK_OWNERSHIP:
    case CN_FAILURE_OWNERSHIP_LEAK:
    case CN_FAILURE_GHOST_ARGS:
      exit(exit_code);
  }
}

static cn_failure_callback cn_failure_aux = &cn_failure_default;

void cn_failure(enum cn_failure_mode failure_mode, enum spec_mode spec_mode) {
  cn_failure_aux(failure_mode, spec_mode);
}

void set_cn_failure_cb(cn_failure_callback callback) {
  cn_failure_aux = callback;
}

void reset_cn_failure_cb(void) {
  cn_failure_aux = &cn_failure_default;
}

static enum cn_trace_granularity trace_granularity = CN_TRACE_NONE;

enum cn_trace_granularity get_cn_trace_granularity(void) {
  return trace_granularity;
}

enum cn_trace_granularity set_cn_trace_granularity(
    enum cn_trace_granularity new_granularity) {
  enum cn_trace_granularity old_granularity = trace_granularity;
  trace_granularity = new_granularity;
  return old_granularity;
}

void print_error_msg_info_single(struct cn_error_message_info* info) {
  if (exec_c_locs_mode) {
    cn_printf(CN_LOGGING_ERROR,
        "function %s, file %s, line %d\n",
        info->function_name,
        info->file_name,
        info->line_number);
  } else {
    if (info->cn_source_loc) {
      cn_printf(
          CN_LOGGING_ERROR, "original source location: \n%s\n\n", info->cn_source_loc);
    } else {
      cn_printf(CN_LOGGING_ERROR,
          "no source location found (try running with --exec-c-locs-mode enabled)")
    }
  }
}

void print_error_msg_info(struct cn_error_message_info* info) {
  if (info) {
    enum cn_trace_granularity granularity = get_cn_trace_granularity();
    if (granularity != CN_TRACE_NONE && info->parent != NULL) {
      struct cn_error_message_info* curr = info;
      while (curr->parent != NULL) {
        curr = curr->parent;
      }

      cn_printf(CN_LOGGING_ERROR,
          "********************* Originated from **********************\n");
      print_error_msg_info_single(curr);
      curr = curr->child;

      while (granularity > CN_TRACE_ENDS && curr->child != NULL) {
        cn_printf(CN_LOGGING_ERROR,
            "************************************************************\n");
        print_error_msg_info_single(curr);
        curr = curr->child;
      }
    }

    cn_printf(CN_LOGGING_ERROR,
        "************************ Failed at *************************\n");
    print_error_msg_info_single(info);
  } else {
    cn_printf(CN_LOGGING_ERROR, "Internal error: no error_msg_info available.");
    exit(SIGABRT);
  }
}