/* #681: Fulminate allocated a CN value for every parameter and local at entry, in a bump allocator
 * reset only at the exit of a function with a specification, so a function without one grew it with
 * every call; now only the functions that need them (a requires/ensures, a loop invariant or a CN
 * statement) get them.
 *
 * Uninstrumented driver, shared by every <n>-*.c here: a bump allocator of two 1 KB blocks, one
 * warm-up call of run() (first-call allocations are not the leak), then 10 calls, reporting the
 * bump frame's growth (block + pointer) per call. Exit 1 if the frame moved over the 10 calls. */
#include <stdio.h>
#include <cn-executable/utils.h>
#include <cn-executable/bump_alloc.h>
void run(void);
int main(void) {
  fulminate_init((struct fulm_init_config){ .max_bump_blocks = 2, .bump_block_size = 1024 });
  run();
  cn_bump_frame_id f0 = cn_bump_get_frame_id();
  for (int i = 0; i < 10; i++) {
    cn_bump_frame_id a = cn_bump_get_frame_id();
    run();
    cn_bump_frame_id b = cn_bump_get_frame_id();
    if (a.block != b.block || a.pointer != b.pointer)
      printf("  call %d: bump grew (block %u->%u, +%ld bytes)\n", i, a.block, b.block,
             a.block == b.block ? (long)(b.pointer - a.pointer) : -1L);
  }
  cn_bump_frame_id f1 = cn_bump_get_frame_id();
  int grew = f0.block != f1.block || f0.pointer != f1.pointer;
  printf("%s\n", grew ? "GREW over 10 calls" : "flat over 10 calls");
  return grew;
}
