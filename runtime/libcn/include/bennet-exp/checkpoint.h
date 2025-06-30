#ifndef BENNET_EXP_CHECKPOINT_H
#define BENNET_EXP_CHECKPOINT_H

#include <bennet-exp/alloc.h>
#include <bennet-exp/rand.h>
#include <cn-executable/bump_alloc.h>

typedef struct {
  cn_bump_frame_id frame_id;
  void* alloc;
  void* ownership;
} bennet_checkpoint;

static inline bennet_checkpoint bennet_checkpoint_save(void) {
  return (bennet_checkpoint){
      .frame_id = cn_bump_get_frame_id(),
      .alloc = bennet_alloc_save(),
      .ownership = bennet_ownership_save(),
  };
}

static inline void bennet_checkpoint_restore(const bennet_checkpoint* cp) {
  cn_bump_free_after(cp->frame_id);
  bennet_alloc_restore(cp->alloc);
  bennet_ownership_restore(cp->ownership);
}

#endif  // BENNET_EXP_CHECKPOINT_H
