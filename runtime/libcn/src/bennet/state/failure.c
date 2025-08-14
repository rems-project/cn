#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <bennet/internals/size.h>
#include <bennet/state/failure.h>

struct name_list {
  const void* id;
  void* domain;
  struct name_list* next;
};

struct bennet_failure {
  /** Has not backtracked past a blamed value or branch */
  bool young;  // FIXME: Mark old when backtracking past a path variable
  struct name_list* blamed;
  enum bennet_failure_type type;
};

static struct bennet_failure failure =
    (struct bennet_failure){.young = false, .blamed = NULL, .type = BENNET_FAILURE_NONE};

void bennet_failure_reset(void) {
  failure.type = BENNET_FAILURE_NONE;

  while (failure.blamed != NULL) {
    void* tmp = failure.blamed->next;
    free(failure.blamed->domain);
    free(failure.blamed);
    failure.blamed = tmp;
  }
}

bool bennet_failure_is_young(void) {
  return failure.young;
}

void bennet_failure_mark_young(void) {
  failure.young = true;
}

void bennet_failure_mark_old(void) {
  failure.young = false;
}

enum bennet_failure_type bennet_failure_get_failure_type(void) {
  return failure.type;
}

void bennet_failure_set_failure_type(enum bennet_failure_type type) {
  failure.type = type;
  failure.young = true;
}

#define DOMAIN_FAILURE(ty)                                                               \
  void bennet_failure_blame_domain_##ty(const void* id, bennet_domain(ty) * domain) {    \
    bennet_domain(ty)* new_domain = NULL;                                                \
    if (domain != NULL) {                                                                \
      assert(failure.type == BENNET_FAILURE_ASSERT ||                                    \
             failure.type == BENNET_FAILURE_ASSIGN);                                     \
      new_domain = bennet_domain_copy(ty, domain);                                       \
    }                                                                                    \
                                                                                         \
    struct name_list* new_node = (struct name_list*)malloc(sizeof(struct name_list));    \
    *new_node = (struct name_list){.id = id, .domain = new_domain, .next = 0};           \
                                                                                         \
    if (failure.blamed == NULL) {                                                        \
      failure.blamed = new_node;                                                         \
      return;                                                                            \
    }                                                                                    \
                                                                                         \
    struct name_list* prev = NULL;                                                       \
    struct name_list* curr = failure.blamed;                                             \
    while (curr != NULL) {                                                               \
      /* If variable is already in list, free `new_node` and return */                   \
      if (curr->id == id) {                                                              \
        if (new_domain != NULL) {                                                        \
          if (curr->domain == NULL) {                                                    \
            curr->domain = new_domain;                                                   \
          } else {                                                                       \
            curr->domain = bennet_domain_meet(ty, curr->domain, new_domain);             \
            free(new_domain);                                                            \
          }                                                                              \
        }                                                                                \
        free(new_node);                                                                  \
        return;                                                                          \
      }                                                                                  \
                                                                                         \
      prev = curr;                                                                       \
      curr = curr->next;                                                                 \
    }                                                                                    \
                                                                                         \
    prev->next = new_node;                                                               \
  }                                                                                      \
                                                                                         \
  bennet_domain(ty) * bennet_failure_get_domain_##ty(const void* id) {                   \
    assert(failure.type != BENNET_FAILURE_NONE);                                         \
                                                                                         \
    struct name_list* curr = failure.blamed;                                             \
    while (curr != NULL) {                                                               \
      if (curr->id == id) {                                                              \
        return (bennet_domain(ty)*)curr->domain;                                         \
      }                                                                                  \
                                                                                         \
      curr = curr->next;                                                                 \
    }                                                                                    \
                                                                                         \
    return NULL;                                                                         \
  }

DOMAIN_FAILURE(int8_t)
DOMAIN_FAILURE(uint8_t)
DOMAIN_FAILURE(int16_t)
DOMAIN_FAILURE(uint16_t)
DOMAIN_FAILURE(int32_t)
DOMAIN_FAILURE(uint32_t)
DOMAIN_FAILURE(int64_t)
DOMAIN_FAILURE(uint64_t)
DOMAIN_FAILURE(uintptr_t)

void bennet_failure_blame(const void* id) {
  bennet_failure_blame_domain(int8_t, id, NULL);
}

int bennet_failure_remove_blame(const void* id) {
  bennet_failure_mark_old();

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

  if (failure.type == BENNET_FAILURE_TIMEOUT) {
    return false;
  }

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
