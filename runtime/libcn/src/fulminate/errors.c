#include <cn-executable/fulminate_alloc.h>
#include <fulminate/errors.h>

struct cn_error_message_info* global_error_msg_info;

struct cn_error_message_info* make_error_message_info_entry(const char* function_name,
    char* file_name,
    int line_number,
    char* cn_source_loc,
    struct cn_error_message_info* parent) {
  struct cn_error_message_info* entry =
      fulm_malloc(sizeof(struct cn_error_message_info), &fulm_default_alloc);
  entry->function_name = function_name;
  entry->file_name = file_name;
  entry->line_number = line_number;
  entry->cn_source_loc = cn_source_loc;
  entry->parent = parent;
  entry->child = NULL;
  if (parent) {
    parent->child = entry;
  }
  return entry;
}

void initialise_error_msg_info_(
    const char* function_name, char* file_name, int line_number) {
  // cn_printf(CN_LOGGING_INFO, "Initialising error message info\n");
  global_error_msg_info =
      make_error_message_info_entry(function_name, file_name, line_number, 0, NULL);
}

void update_error_message_info_(
    const char* function_name, char* file_name, int line_number, char* cn_source_loc) {
  global_error_msg_info = make_error_message_info_entry(
      function_name, file_name, line_number, cn_source_loc, global_error_msg_info);
}
void reset_error_msg_info(void) {
  global_error_msg_info = NULL;
}

void free_error_msg_info(void) {
  while (global_error_msg_info != NULL) {
    cn_pop_msg_info();
  }
}

void cn_pop_msg_info(void) {
  struct cn_error_message_info* old = global_error_msg_info;
  global_error_msg_info = old->parent;
  if (global_error_msg_info) {
    global_error_msg_info->child = NULL;
  }
  fulm_free(old, &fulm_default_alloc);
}
