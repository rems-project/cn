#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <bennet-exp/failure.h>
#include <bennet-exp/size.h>

struct name_list {
  const void* id;
  bennet_domain_failure_info* domain;
  struct name_list* next;
};

struct bennet_failure {
  struct name_list* blamed;
  enum bennet_failure_type type;
};

static struct bennet_failure failure =
    (struct bennet_failure){.blamed = NULL, .type = BENNET_FAILURE_NONE};

void bennet_failure_reset(void) {
  failure.type = BENNET_FAILURE_NONE;

  while (failure.blamed != NULL) {
    void* tmp = failure.blamed->next;
    free(failure.blamed->domain);
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

void bennet_failure_blame_domain(const void* id, bennet_domain_failure_info* domain) {
  bennet_domain_failure_info* new_domain = NULL;
  if (domain != NULL) {
    assert(
        failure.type == BENNET_FAILURE_ASSERT || failure.type == BENNET_FAILURE_ASSIGN);
    new_domain = (bennet_domain_failure_info*)malloc(sizeof(bennet_domain_failure_info));
    memcpy(new_domain, domain, sizeof(bennet_domain_failure_info));
  }

  struct name_list* new_node = (struct name_list*)malloc(sizeof(struct name_list));
  *new_node = (struct name_list){.id = id, .domain = new_domain, .next = 0};

  if (failure.blamed == NULL) {
    failure.blamed = new_node;
    return;
  }

  struct name_list* curr = failure.blamed;
  while (curr->next != NULL) {
    /* If variable is already in list, free `new_node` and return */
    if (curr->id == id) {
      free(new_node);
      return;
    }

    curr = curr->next;
  }

  /* Check last node */
  if (curr->id == id) {
    free(new_node);
    return;
  }

  curr->next = new_node;
}

void bennet_failure_blame(const void* id) {
  bennet_failure_blame_domain(id, NULL);
}

int bennet_failure_remove_blame(const void* id) {
  struct name_list* prev = NULL;
  struct name_list* curr = failure.blamed;
  while (curr != NULL) {
    if (curr->id == id) {
      if (prev) {
        prev->next = curr->next;
      } else {
        failure.blamed = curr->next;
      }

      free(curr->domain);
      free(curr);

      return 1;
    }

    prev = curr;
    curr = curr->next;
  }

  return 0;
}

void bennet_failure_blame_many(const void* toAdd[]) {
  for (int i = 0; toAdd[i] != NULL; i++) {
    bennet_failure_blame(toAdd[i]);
  }
}

bool bennet_failure_is_blamed(const void* id) {
  assert(failure.type != BENNET_FAILURE_NONE);

  struct name_list* curr = failure.blamed;
  while (curr != NULL) {
    if (curr->id == id) {
      return 1;
    }

    curr = curr->next;
  }
  return 0;
}

int bennet_failure_remap_blamed(const void* from, const void* to) {
  struct name_list* curr = failure.blamed;
  while (curr != NULL) {
    if (curr->id == from) {
      curr->id = to;
      return 1;
    }

    curr = curr->next;
  }
  return 0;
}

int bennet_failure_remap_blamed_many(const char* from[], const char* to[]) {
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

bennet_domain_failure_info* bennet_failure_get_domain(const void* id) {
  assert(failure.type == BENNET_FAILURE_ASSERT || failure.type == BENNET_FAILURE_ASSIGN);

  struct name_list* curr = failure.blamed;
  while (curr != NULL) {
    if (curr->id == id) {
      return curr->domain;
    }

    curr = curr->next;
  }

  return NULL;
}
