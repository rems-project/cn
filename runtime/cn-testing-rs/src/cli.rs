//! Command-line argument parsing for the test framework

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_void;
use libc::{c_char, c_int};
use crate::bindings;
use crate::types::CnTestGenProgress;

/// Configuration options parsed from command-line arguments
#[derive(Debug)]
pub struct TestConfig {
    pub progress_level: CnTestGenProgress,
    pub seed: u64,
    pub logging_level: bindings::cn_logging_level,
    pub timeout: i32,
    pub input_timeout: i32,
    pub exit_fast: bool,
    pub trap: bool,
    pub sizing_strategy: bindings::bennet_sizing_strategy,
    pub replay: bool,
    pub replicas: bool,
    pub print_seed: bool,
    pub output_tyche: bool,
    pub tyche_output_stream: *mut c_void,  // Opaque FILE pointer
    pub print_size_info: bool,
    pub print_backtrack_info: bool,
    pub print_satisfaction_info: bool,
    pub print_discard_info: bool,
    pub print_timing_info: bool,
    pub test_filter: HashMap<String, bool>,
}

impl Default for TestConfig {
    fn default() -> Self {
        TestConfig {
            progress_level: CnTestGenProgress::CN_TEST_GEN_PROGRESS_ALL,
            seed: 0,
            logging_level: bindings::cn_logging_level_CN_LOGGING_ERROR,
            timeout: 0,
            input_timeout: 1000,
            exit_fast: false,
            trap: false,
            sizing_strategy: bindings::bennet_sizing_strategy_BENNET_SIZE_QUICKCHECK,
            replay: true,
            replicas: true,
            print_seed: false,
            output_tyche: false,
            tyche_output_stream: std::ptr::null_mut(),
            print_size_info: false,
            print_backtrack_info: false,
            print_satisfaction_info: false,
            print_discard_info: false,
            print_timing_info: false,
            test_filter: HashMap::new(),
        }
    }
}

impl TestConfig {
    /// Parse command-line arguments
    ///
    /// # Safety
    ///
    /// - `argv` must be a valid array of `argc` pointers to null-terminated strings
    pub unsafe fn parse(argc: c_int, argv: *mut *mut c_char) -> Self {
        let mut config = TestConfig::default();

        let mut i = 0;
        while i < argc {
            let arg = CStr::from_ptr(*argv.add(i as usize))
                .to_str()
                .unwrap_or("");

            match arg {
                "-S" | "--seed" => {
                    if i + 1 < argc {
                        let seed_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        config.seed = u64::from_str_radix(seed_str.trim_start_matches("0x"), 16)
                            .unwrap_or(0);
                        i += 1;
                    }
                }
                "--logging-level" => {
                    if i + 1 < argc {
                        let level_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("error");
                        config.logging_level = match level_str {
                            "none" => bindings::cn_logging_level_CN_LOGGING_NONE,
                            "error" => bindings::cn_logging_level_CN_LOGGING_ERROR,
                            "info" => bindings::cn_logging_level_CN_LOGGING_INFO,
                            _ => level_str
                                .parse::<u32>()
                                .unwrap_or(bindings::cn_logging_level_CN_LOGGING_ERROR),
                        };
                        i += 1;
                    }
                }
                "--trace-granularity" => {
                    if i + 1 < argc {
                        let gran_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("none");
                        let granularity = match gran_str {
                            "none" => bindings::cn_trace_granularity_CN_TRACE_NONE,
                            "ends" => bindings::cn_trace_granularity_CN_TRACE_ENDS,
                            "all" => bindings::cn_trace_granularity_CN_TRACE_ALL,
                            _ => gran_str
                                .parse::<u32>()
                                .unwrap_or(bindings::cn_trace_granularity_CN_TRACE_NONE),
                        };
                        bindings::set_cn_trace_granularity(granularity);
                        i += 1;
                    }
                }
                "--progress-level" => {
                    if i + 1 < argc {
                        let level_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("testcase");
                        config.progress_level = match level_str {
                            "silent" => CnTestGenProgress::CN_TEST_GEN_PROGRESS_NONE,
                            "function" => CnTestGenProgress::CN_TEST_GEN_PROGRESS_FINAL,
                            "testcase" => CnTestGenProgress::CN_TEST_GEN_PROGRESS_ALL,
                            _ => level_str
                                .parse::<u32>()
                                .map(|v| match v {
                                    0 => CnTestGenProgress::CN_TEST_GEN_PROGRESS_NONE,
                                    1 => CnTestGenProgress::CN_TEST_GEN_PROGRESS_FINAL,
                                    _ => CnTestGenProgress::CN_TEST_GEN_PROGRESS_ALL,
                                })
                                .unwrap_or(CnTestGenProgress::CN_TEST_GEN_PROGRESS_ALL),
                        };
                        i += 1;
                    }
                }
                "--input-timeout" => {
                    if i + 1 < argc {
                        let timeout_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("1000");
                        config.input_timeout = timeout_str.parse().unwrap_or(1000);
                        i += 1;
                    }
                }
                "--null-in-every" => {
                    if i + 1 < argc {
                        let val_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        let val = val_str.parse().unwrap_or(0);
                        bindings::set_null_in_every(val);
                        i += 1;
                    }
                }
                "--until-timeout" => {
                    if i + 1 < argc {
                        let timeout_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        config.timeout = timeout_str.parse().unwrap_or(0);
                        i += 1;
                    }
                }
                "--exit-fast" => {
                    config.exit_fast = true;
                }
                "--max-stack-depth" => {
                    if i + 1 < argc {
                        let depth_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        let depth = depth_str.parse().unwrap_or(0);
                        bindings::bennet_set_max_depth(depth);
                        i += 1;
                    }
                }
                "--max-generator-size" => {
                    if i + 1 < argc {
                        let size_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        let size = size_str.parse().unwrap_or(0);
                        assert!(size != 0, "max-generator-size must not be zero");
                        bindings::bennet_set_max_size(size);
                        i += 1;
                    }
                }
                "--allowed-size-split-backtracks" => {
                    if i + 1 < argc {
                        let val_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        let val = val_str.parse().unwrap_or(0);
                        bindings::bennet_set_size_split_backtracks_allowed(val);
                        i += 1;
                    }
                }
                "--trap" => {
                    config.trap = true;
                }
                "--sizing-strategy" => {
                    if i + 1 < argc {
                        let strategy_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("quickcheck");
                        config.sizing_strategy = match strategy_str {
                            "uniform" => bindings::bennet_sizing_strategy_BENNET_SIZE_UNIFORM,
                            "constant" => bindings::bennet_sizing_strategy_BENNET_SIZE_CONSTANT,
                            "quickcheck" => bindings::bennet_sizing_strategy_BENNET_SIZE_QUICKCHECK,
                            _ => strategy_str
                                .parse()
                                .unwrap_or(bindings::bennet_sizing_strategy_BENNET_SIZE_QUICKCHECK),
                        };
                        i += 1;
                    }
                }
                "--no-replays" => {
                    config.replay = false;
                }
                "--no-replicas" => {
                    config.replicas = false;
                }
                "--print-seed" => {
                    config.print_seed = true;
                }
                "--output-tyche" => {
                    if i + 1 < argc {
                        let path_str = CStr::from_ptr(*argv.add((i + 1) as usize));
                        let path_cstring = CString::new(path_str.to_bytes()).unwrap();
                        let mode = CString::new("w").unwrap();
                        let file_ptr = libc::fopen(path_cstring.as_ptr(), mode.as_ptr());
                        if !file_ptr.is_null() {
                            config.tyche_output_stream = file_ptr as *mut c_void;
                            config.output_tyche = true;
                        }
                        i += 1;
                    }
                }
                "--print-backtrack-info" => {
                    config.print_backtrack_info = true;
                }
                "--print-satisfaction-info" => {
                    config.print_satisfaction_info = true;
                }
                "--print-size-info" => {
                    config.print_size_info = true;
                }
                "--print-discard-info" => {
                    config.print_discard_info = true;
                }
                "--print-timing-info" => {
                    config.print_timing_info = true;
                }
                "--smt-pruning-at-runtime" => {
                    bindings::cn_smt_pruning_at_runtime = true;
                }
                "--use-solver-eval" => {
                    bindings::cn_set_use_solver_eval(true);
                }
                "--smt-skew-pointer-order" => {
                    bindings::cn_smt_skew_pointer_order = true;
                }
                "--smt-skewing" => {
                    if i + 1 < argc {
                        let mode_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("none");
                        let mode = match mode_str {
                            "uniform" => bindings::cn_smt_skewing_mode_CN_SMT_SKEWING_UNIFORM,
                            "sized" => bindings::cn_smt_skewing_mode_CN_SMT_SKEWING_SIZED,
                            "none" => bindings::cn_smt_skewing_mode_CN_SMT_SKEWING_NONE,
                            _ => {
                                eprintln!("Error: Invalid --smt-skewing mode: {}", mode_str);
                                eprintln!("Valid modes: uniform, sized, none");
                                std::process::exit(1);
                            }
                        };
                        bindings::cn_set_smt_skewing_mode(mode);
                        i += 1;
                    }
                }
                "--smt-logging" => {
                    if i + 1 < argc {
                        let path = *argv.add((i + 1) as usize);
                        bindings::cn_smt_set_log_file_path(path);
                        i += 1;
                    }
                }
                "--smt-log-unsat-cores" => {
                    if i + 1 < argc {
                        let path = *argv.add((i + 1) as usize);
                        bindings::cn_smt_set_unsat_core_log_path(path);
                        i += 1;
                    }
                }
                "--max-bump-blocks" => {
                    if i + 1 < argc {
                        let val_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        let val = val_str.parse().unwrap_or(0);
                        bindings::cn_bump_set_max_blocks(val);
                        i += 1;
                    }
                }
                "--bump-block-size" => {
                    if i + 1 < argc {
                        let val_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        let val = val_str.parse().unwrap_or(0);
                        bindings::cn_bump_set_block_size(val);
                        i += 1;
                    }
                }
                "--max-input-alloc" => {
                    if i + 1 < argc {
                        let val_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("0");
                        let val = val_str.parse().unwrap_or(0);
                        bindings::bennet_rand_alloc_set_mem_size(val);
                        i += 1;
                    }
                }
                "--only" => {
                    if i + 1 < argc {
                        let test_names_str = CStr::from_ptr(*argv.add((i + 1) as usize))
                            .to_str()
                            .unwrap_or("");

                        // Parse comma-separated test names
                        for token in test_names_str.split(',') {
                            let trimmed = token.trim();
                            if !trimmed.is_empty() {
                                config.test_filter.insert(trimmed.to_string(), true);
                            }
                        }
                        i += 1;
                    }
                }
                _ => {}
            }

            i += 1;
        }

        config
    }
}
