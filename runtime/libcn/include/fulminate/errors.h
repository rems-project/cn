#ifndef CN_ERRORS_H
#define CN_ERRORS_H

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

void reset_error_msg_info(void);
void free_error_msg_info(void);

void update_error_message_info_(
    const char* function_name, char* file_name, int line_number, char* cn_source_loc);

void cn_pop_msg_info(void);

#define update_cn_error_message_info(x)                                                  \
  update_error_message_info_(__func__, __FILE__, __LINE__ + 1, x)

#define update_cn_error_message_info_access_check(x)                                     \
  update_error_message_info_(__func__, __FILE__, __LINE__, x)

#endif  // CN_ERRORS_H
