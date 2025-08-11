#ifndef FOCUS_CTX_H
#define FOCUS_CTX_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void initialise_focus_context(void);
void push_focus_context(void);
void pop_focus_context(void);
void insert_focus(int64_t index);
int check_focus(int64_t index);

#ifdef __cplusplus
}
#endif

#endif  // FOCUS_CTX_H
