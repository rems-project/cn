//! Test execution engine
//!
//! This module contains the main test loop and execution logic.

use libc::{c_char, c_int};
use std::ffi::CStr;
use crate::bindings;
use crate::cli::TestConfig;
use crate::registry;
use crate::types::{CnTestInput, CnTestReproduction, CnTestResult, CN_TEST_MAX_TEST_CASES};

/// Main test harness entry point
///
/// # Safety
///
/// - `argv` must be a valid array of `argc` pointers to null-terminated strings
#[no_mangle]
pub unsafe extern "C" fn cn_test_main(argc: c_int, argv: *mut *mut c_char) -> c_int {
    // Initialize logging
    bindings::set_cn_logging_level(bindings::cn_logging_level_CN_LOGGING_NONE);

    let begin_time = bindings::bennet_get_microseconds();

    // Seed the random number generator
    bindings::bennet_srand(bindings::bennet_get_milliseconds());

    // Parse command-line arguments
    let mut config = TestConfig::parse(argc, argv);

    // Initialize info collection subsystems
    if config.output_tyche || config.print_size_info {
        bindings::bennet_info_sizes_init();
    }

    if config.output_tyche || config.print_backtrack_info {
        bindings::bennet_info_backtracks_init();
    }

    if config.print_satisfaction_info {
        bindings::bennet_info_unsatisfied_init();
    }

    if config.print_discard_info {
        bindings::bennet_info_discards_init();
    }

    if config.output_tyche || config.print_timing_info {
        bindings::bennet_info_timing_init();
    }

    // Print initial info
    if config.timeout != 0 {
        println!("Running until timeout of {} seconds", config.timeout);
    }

    if config.print_seed {
        println!("Using seed: {:016x}", config.seed);
    }

    // Seed with the configured seed
    bindings::bennet_srand(config.seed);
    bindings::bennet_rand(); // Junk to get something to make a checkpoint from

    // Initialize results and reproduction data
    let mut repros: Vec<CnTestReproduction> = Vec::with_capacity(CN_TEST_MAX_TEST_CASES);
    let mut results: Vec<CnTestResult> = Vec::with_capacity(CN_TEST_MAX_TEST_CASES);

    let num_test_cases = registry::get_test_count();
    for _ in 0..num_test_cases {
        repros.push(CnTestReproduction {
            size: 0,
            checkpoint: bindings::bennet_rand_checkpoint {
                state: std::ptr::null_mut(),
            },
        });
        results.push(CnTestResult::CN_TEST_SKIP);
    }

    let mut timediff = 0i32;

    // Main test loop
    'outer: loop {
        for i in 0..num_test_cases {
            // Skip tests that have already failed
            if results[i] == CnTestResult::CN_TEST_FAIL {
                continue;
            }

            let test_case = match registry::get_test_case(i) {
                Some(tc) => tc,
                None => continue,
            };

            // Apply test filter
            if !config.test_filter.is_empty() {
                let name_str = CStr::from_ptr(test_case.name)
                    .to_str()
                    .unwrap_or("");
                if !config.test_filter.contains_key(name_str) {
                    continue;
                }
            }

            // Set function under test for info collection
            if config.output_tyche || config.print_size_info {
                bindings::bennet_info_sizes_set_function_under_test(test_case.name);
            }

            if config.output_tyche || config.print_backtrack_info {
                bindings::bennet_info_backtracks_set_function_under_test(test_case.name);
            }

            if config.print_satisfaction_info {
                bindings::bennet_info_unsatisfied_set_function_under_test(test_case.name);
            }

            if config.print_discard_info {
                bindings::bennet_info_discards_set_function_under_test(test_case.name);
            }

            if config.output_tyche || config.print_timing_info {
                bindings::bennet_info_timing_set_function_under_test(test_case.name);
            }

            // Print initial test info
            if config.progress_level == crate::types::CnTestGenProgress::CN_TEST_GEN_PROGRESS_ALL {
                crate::utils::print_test_info(test_case.suite, test_case.name, 0, 0);
            }

            // Save checkpoint for reproduction
            repros[i].checkpoint = bindings::bennet_rand_save();
            bindings::bennet_set_input_timeout(config.input_timeout);

            // Execute the test
            let test_input = CnTestInput {
                replay: false,
                progress_level: config.progress_level,
                sizing_strategy: config.sizing_strategy,
                trap: false,
                replicas: false,
                log_all_backtracks: false,
                output_tyche: config.output_tyche,
                tyche_output_stream: config.tyche_output_stream,
                begin_time,
            };

            let result = if let Some(func) = test_case.func {
                func(test_input)
            } else {
                CnTestResult::CN_TEST_SKIP
            };

            // Update results, but preserve PASS if we get GEN_FAIL
            if !(results[i] == CnTestResult::CN_TEST_PASS
                && result == CnTestResult::CN_TEST_GEN_FAIL)
            {
                results[i] = result;
            }

            repros[i].size = bindings::bennet_get_size() as usize;

            // Skip result printing if progress level is NONE
            if config.progress_level
                == crate::types::CnTestGenProgress::CN_TEST_GEN_PROGRESS_NONE
            {
                continue;
            }

            println!();
            match result {
                CnTestResult::CN_TEST_PASS => {
                    println!("PASSED");
                }
                CnTestResult::CN_TEST_FAIL => {
                    println!("FAILED");

                    // Replay the failure if enabled
                    if config.replay {
                        bindings::set_cn_logging_level(config.logging_level);

                        if bindings::get_cn_logging_level()
                            >= bindings::cn_logging_level_CN_LOGGING_ERROR
                        {
                            println!();
                        }

                        // Restore the checkpoint
                        bindings::bennet_set_size(repros[i].size as u64);
                        bindings::bennet_rand_restore(repros[i].checkpoint);

                        let replay_input = CnTestInput {
                            replay: true,
                            progress_level: crate::types::CnTestGenProgress::CN_TEST_GEN_PROGRESS_NONE,
                            sizing_strategy: config.sizing_strategy,
                            trap: config.trap,
                            replicas: config.replicas,
                            log_all_backtracks: false,
                            output_tyche: false,
                            tyche_output_stream: std::ptr::null_mut(),
                            begin_time,
                        };

                        let replay_result = if let Some(func) = test_case.func {
                            func(replay_input)
                        } else {
                            CnTestResult::CN_TEST_SKIP
                        };

                        if replay_result != CnTestResult::CN_TEST_FAIL {
                            if bindings::get_cn_logging_level()
                                < bindings::cn_logging_level_CN_LOGGING_ERROR
                            {
                                println!();
                            }
                            eprintln!(
                                "Replay of failure did not fail (result = {:?}).",
                                replay_result
                            );
                            std::process::abort();
                        }

                        bindings::set_cn_logging_level(bindings::cn_logging_level_CN_LOGGING_NONE);
                    }
                }
                CnTestResult::CN_TEST_GEN_FAIL => {
                    println!("FAILED TO GENERATE VALID INPUT");
                }
                CnTestResult::CN_TEST_SKIP => {
                    println!("SKIPPED");
                }
            }

            // Exit fast on failure if enabled
            if config.exit_fast && result == CnTestResult::CN_TEST_FAIL {
                break 'outer;
            }

            // Check timeout
            if config.timeout != 0 {
                timediff = ((bindings::bennet_get_microseconds() - begin_time) / 1000000) as i32;
            }
        }

        if timediff < config.timeout {
            println!(
                "\n{} seconds remaining, rerunning tests\n",
                config.timeout - timediff
            );
        } else {
            break;
        }
    }

    // Close tyche output stream if opened
    if !config.tyche_output_stream.is_null() {
        libc::fclose(config.tyche_output_stream as *mut libc::FILE);
    }

    // Count results
    let mut passed = 0;
    let mut failed = 0;
    let mut errored = 0;
    let mut skipped = 0;

    for result in &results {
        match result {
            CnTestResult::CN_TEST_PASS => passed += 1,
            CnTestResult::CN_TEST_FAIL => failed += 1,
            CnTestResult::CN_TEST_GEN_FAIL => errored += 1,
            CnTestResult::CN_TEST_SKIP => skipped += 1,
        }
    }

    // Print summary
    println!("\nTesting Summary:");
    println!(
        "cases: {}, passed: {}, failed: {}, errored: {}, skipped: {}",
        num_test_cases, passed, failed, errored, skipped
    );

    // Print info if requested
    if config.print_size_info {
        println!();
        bindings::bennet_info_sizes_print_info();
    }

    if config.print_backtrack_info {
        println!();
        bindings::bennet_info_backtracks_print_backtrack_info();
    }

    if config.print_satisfaction_info {
        println!();
        bindings::bennet_info_unsatisfied_print_info();
    }

    if config.print_discard_info {
        println!();
        bindings::bennet_info_discards_print_info();
    }

    if config.print_timing_info {
        println!();
        bindings::bennet_info_timing_print_info();
    }

    // Return non-zero exit code if there were failures or errors
    if failed == 0 && errored == 0 {
        0
    } else {
        1
    }
}
