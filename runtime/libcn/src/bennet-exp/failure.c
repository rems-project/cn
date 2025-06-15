#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <bennet-exp/failure.h>
#include <bennet-exp/size.h>

struct bennet_allocation_failure_info {
  int allocate_more_than;
  bool should_be_null;
};

struct name_list {
  char* name;
  struct name_list* next;
};

struct bennet_assertion_failure_info {};

union bennet_failure_body {
  struct bennet_allocation_failure_info allocation;
  struct bennet_assertion_failure_info assertion;
};

struct bennet_failure {
  struct name_list* blamed;
  enum bennet_failure_type type;
  union bennet_failure_body body;
};

static struct bennet_failure failure =
    (struct bennet_failure){.blamed = NULL, .type = BENNET_BACKTRACK_NONE};

void bennet_failure_reset(void) {
  failure.type = BENNET_BACKTRACK_NONE;

  while (failure.blamed != NULL) {
    void* tmp = failure.blamed->next;
    free(failure.blamed);
    failure.blamed = tmp;
  }
}

enum bennet_failure_type bennet_failure_get_failure_type(void) {
  return failure.type;
}

void bennet_failure_set_failure_type(enum bennet_failure_type type) {
  failure.type = type;
}

void bennet_failure_blame(char* varname) {
  struct name_list* new_node = (struct name_list*)malloc(sizeof(struct name_list));
  *new_node = (struct name_list){.name = varname, .next = 0};

  if (failure.blamed == NULL) {
    failure.blamed = new_node;
    return;
  }

  struct name_list* curr = failure.blamed;
  while (curr->next != NULL) {
    /* If variable is already in list, free `new_node` and return */
    if (strcmp(curr->name, varname) == 0) {
      free(new_node);
      return;
    }

    curr = curr->next;
  }

  /* Check last node */
  if (strcmp(curr->name, varname) == 0) {
    free(new_node);
    return;
  }

  curr->next = new_node;
}

void bennet_failure_blame_many(char* toAdd[]) {
  for (int i = 0; toAdd[i] != NULL; i++) {
    bennet_failure_blame(toAdd[i]);
  }
}

int bennet_failure_is_blamed(char* varname) {
  assert(failure.type != BENNET_BACKTRACK_NONE);

  struct name_list* curr = failure.blamed;
  while (curr != NULL) {
    if (strcmp(varname, curr->name) == 0) {
      return 1;
    }

    curr = curr->next;
  }
  return 0;
}

int bennet_failure_remap_blamed(char* from, char* to) {
  struct name_list* curr = failure.blamed;
  while (curr != NULL) {
    if (strcmp(from, curr->name) == 0) {
      curr->name = to;
      return 1;
    }

    curr = curr->next;
  }
  return 0;
}

int bennet_failure_remap_blamed_many(char* from[], char* to[]) {
  int number_of_remaps = 0;
  for (int i = 0; from[i] != 0; i++) {
    number_of_remaps += 1;
  }
  if (number_of_remaps == 0) {
    return 1;
  }

  char** toUnique = malloc(number_of_remaps * sizeof(char*));

  int successes = 1;
  for (int i = 0; from[i] != 0; i++) {
    // Get length of string
    size_t len = strlen(to[i]);

    // Copy the desired variable name
    toUnique[i] = (char*)malloc(len + 2);
    strcpy(toUnique[i], to[i]);

    // Give it an impossible name, but unique
    (toUnique[i][len]) = '$';
    (toUnique[i][len + 1]) = '\0';

    // We do this indirection in case there's a duplicate between `from` and `to`
    successes &= bennet_failure_remap_blamed(from[i], toUnique[i]);
  }

  for (int i = 0; from[i] != 0; i++) {
    successes &= bennet_failure_remap_blamed(toUnique[i], to[i]);
  }

  for (int i = 0; i < number_of_remaps; i++) {
    free(toUnique[i]);
  }
  free(toUnique);

  return successes;
}

void bennet_failure_set_allocation_needed(size_t sz) {
  failure.type = BENNET_BACKTRACK_ALLOC;
  failure.body.allocation.allocate_more_than = sz;
}

size_t bennet_failure_get_allocation_needed() {
  assert(failure.type == BENNET_BACKTRACK_ALLOC);
  return failure.body.allocation.allocate_more_than;
}
