use std::env;
use std::path::PathBuf;

fn main() {
    let libcn_include = PathBuf::from("../libcn/include");

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/");
    println!("cargo:rerun-if-changed=../libcn/include/");

    // Note: We don't link the C libraries here because dune will handle linking
    // The static library we generate will be linked with the C libraries by dune

    // Generate Rust bindings for Bennet
    // Declare only the functions we need to avoid header conflicts
    let bennet_wrapper = r#"
#ifndef BENNET_WRAPPER_H
#define BENNET_WRAPPER_H

#include <stdint.h>
#include <stdbool.h>

// Opaque FILE type - we'll use libc::FILE in Rust
typedef struct FILE FILE;

// Bennet types
enum bennet_sizing_strategy {
  BENNET_SIZE_UNIFORM = 0,
  BENNET_SIZE_CONSTANT = 1,
  BENNET_SIZE_QUICKCHECK = 2
};

typedef struct {
  void *state;
} bennet_rand_checkpoint;

enum bennet_failure_type {
  BENNET_FAILURE_NONE = 0,
  BENNET_FAILURE_TIMEOUT = 1,
  BENNET_FAILURE_BACKTRACK = 2
};

// Bennet core functions
void bennet_init(void);
void bennet_destroy(void);
void bennet_srand(uint64_t seed);
uint64_t bennet_rand(void);
bennet_rand_checkpoint bennet_rand_save(void);
void bennet_rand_restore(bennet_rand_checkpoint checkpoint);
void bennet_rand_replace(bennet_rand_checkpoint checkpoint);
uint64_t bennet_get_milliseconds(void);
uint64_t bennet_get_microseconds(void);
void bennet_set_size(uint64_t size);
uint64_t bennet_get_size(void);
void bennet_set_input_timer(uint64_t timer);
void bennet_set_input_timeout(int timeout);
void bennet_set_max_depth(uint64_t depth);
void bennet_set_max_size(uint64_t size);
void bennet_set_size_split_backtracks_allowed(uint64_t allowed);
enum bennet_failure_type bennet_failure_get_failure_type(void);
void bennet_rand_alloc_set_mem_size(uint64_t size);

// Info collection functions
void bennet_info_sizes_init(void);
void bennet_info_sizes_set_function_under_test(const char* name);
void bennet_info_sizes_log(void);
void bennet_info_sizes_print_info(void);

void bennet_info_backtracks_init(void);
void bennet_info_backtracks_set_function_under_test(const char* name);
void bennet_info_backtracks_begin_run(void);
void bennet_info_backtracks_end_run(_Bool);
void bennet_info_backtracks_print_backtrack_info(void);

void bennet_info_unsatisfied_init(void);
void bennet_info_unsatisfied_set_function_under_test(const char* name);
void bennet_info_unsatisfied_begin_run(void);
void bennet_info_unsatisfied_end_run(_Bool);
void bennet_info_unsatisfied_print_info(void);

void bennet_info_discards_init(void);
void bennet_info_discards_set_function_under_test(const char* name);
void bennet_info_discards_log(enum bennet_failure_type);
void bennet_info_discards_print_info(void);

void bennet_info_timing_init(void);
void bennet_info_timing_set_function_under_test(const char* name);
void bennet_info_timing_start(const char* label);
void bennet_info_timing_end(const char* label);
void bennet_info_timing_print_info(void);

struct tyche_line_info {
  char *test_suite;
  char *test_name;
  char *status;
  char *status_reason;
  uint64_t suite_begin_time;
  char *representation;
};

void print_test_summary_tyche(FILE *out, struct tyche_line_info *line_info);

#endif
"#;

    std::fs::write("/tmp/bennet_wrapper.h", bennet_wrapper).unwrap();

    let bennet_bindings = bindgen::Builder::default()
        .header("/tmp/bennet_wrapper.h")
        .clang_arg(format!("-I{}", libcn_include.display()))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .blocklist_type("__.*")  // Block compiler builtins
        .blocklist_type("_.*")   // Block darwin types
        .blocklist_function("v.*printf")  // Block varargs printf functions
        .blocklist_function("v.*scanf")   // Block varargs scanf functions
        .allowlist_type("bennet_.*")
        .allowlist_type("tyche_.*")
        .allowlist_type("FILE")
        .allowlist_function("bennet_.*")
        .allowlist_function("print_test_summary_tyche")
        .opaque_type("FILE")
        .generate()
        .expect("Unable to generate bennet bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bennet_bindings
        .write_to_file(out_path.join("bennet_bindings.rs"))
        .expect("Couldn't write bennet bindings!");

    // Generate Rust bindings for CN-Executable utilities
    let utils_wrapper = r#"
#ifndef UTILS_WRAPPER_H
#define UTILS_WRAPPER_H

#include <stdint.h>

enum cn_logging_level {
  CN_LOGGING_NONE = 0,
  CN_LOGGING_ERROR = 1,
  CN_LOGGING_INFO = 2
};

enum cn_trace_granularity {
  CN_TRACE_NONE = 0,
  CN_TRACE_ENDS = 1,
  CN_TRACE_ALL = 2
};

enum cn_failure_mode {
  CN_FAILURE_ASSERT = 1,
  CN_FAILURE_CHECK_OWNERSHIP = 2,
  CN_FAILURE_OWNERSHIP_LEAK = 3,
  CN_FAILURE_ALLOC = 4,
  CN_FAILURE_GHOST_ARGS = 5
};

enum spec_mode {
  SPEC_MODE_C = 0,
  SPEC_MODE_CN = 1
};

uint32_t get_cn_logging_level(void);
void set_cn_logging_level(uint32_t level);
void set_cn_trace_granularity(uint32_t granularity);
void set_null_in_every(int n);
void set_cn_failure_cb(void (*cb)(enum cn_failure_mode, enum spec_mode));

// Fulminate functions
void fulminate_init(void);
void fulminate_destroy(void);

#endif
"#;
    std::fs::write("/tmp/utils_wrapper.h", utils_wrapper).unwrap();

    let utils_bindings = bindgen::Builder::default()
        .header("/tmp/utils_wrapper.h")
        .clang_arg(format!("-I{}", libcn_include.display()))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .blocklist_type("__.*")
        .blocklist_type("_.*")
        .allowlist_type("cn_.*")
        .allowlist_type("spec_mode")
        .allowlist_function("get_cn_.*")
        .allowlist_function("set_cn_.*")
        .allowlist_function("set_null_in_every")
        .allowlist_function("fulminate_.*")
        .generate()
        .expect("Unable to generate utils bindings");

    utils_bindings
        .write_to_file(out_path.join("utils_bindings.rs"))
        .expect("Couldn't write utils bindings!");

    // Generate Rust bindings for CN-SMT
    let smt_wrapper = r#"
#ifndef SMT_WRAPPER_H
#define SMT_WRAPPER_H

#include <stdbool.h>
#include <stdint.h>

enum cn_smt_skewing_mode {
  CN_SMT_SKEWING_NONE = 0,
  CN_SMT_SKEWING_UNIFORM = 1,
  CN_SMT_SKEWING_SIZED = 2
};

// Global SMT options
extern _Bool cn_smt_pruning_at_runtime;
extern _Bool cn_smt_skew_pointer_order;

void cn_smt_init(void);
void cn_smt_destroy(void);
void cn_set_use_solver_eval(_Bool);
void cn_set_smt_skewing_mode(enum cn_smt_skewing_mode);
void cn_smt_set_log_file_path(const char*);
void cn_smt_set_unsat_core_log_path(const char*);
void cn_bump_set_max_blocks(uint64_t);
void cn_bump_set_block_size(uint64_t);
void std_set_default_alloc(void);
void cn_test_free_all(void);

#endif
"#;
    std::fs::write("/tmp/smt_wrapper.h", smt_wrapper).unwrap();

    let smt_bindings = bindgen::Builder::default()
        .header("/tmp/smt_wrapper.h")
        .clang_arg(format!("-I{}", libcn_include.display()))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .blocklist_type("__.*")
        .blocklist_type("_.*")
        .allowlist_type("cn_smt_.*")
        .allowlist_function("cn_.*")
        .allowlist_function("std_set_default_alloc")
        .allowlist_var("cn_smt_.*")
        .generate()
        .expect("Unable to generate smt bindings");

    smt_bindings
        .write_to_file(out_path.join("smt_bindings.rs"))
        .expect("Couldn't write smt bindings!");

    // Generate Rust bindings for CN-Replicate
    let replicate_wrapper = r#"
#ifndef REPLICATE_WRAPPER_H
#define REPLICATE_WRAPPER_H

void cn_replica_alloc_reset(void);
void cn_replica_lines_reset(void);
const char* cn_replica_lines_to_str(void);
const char* cn_replica_lines_to_json_literal(void);

#endif
"#;
    std::fs::write("/tmp/replicate_wrapper.h", replicate_wrapper).unwrap();

    let replicate_bindings = bindgen::Builder::default()
        .header("/tmp/replicate_wrapper.h")
        .clang_arg(format!("-I{}", libcn_include.display()))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .blocklist_type("__.*")
        .blocklist_type("_.*")
        .allowlist_function("cn_replica_.*")
        .generate()
        .expect("Unable to generate replicate bindings");

    replicate_bindings
        .write_to_file(out_path.join("replicate_bindings.rs"))
        .expect("Couldn't write replicate bindings!");
}
