//! Backtracking statistics tracking for Bennet test generation.
//!
//! Tracks backtracking events by function under test, generator, and source location.

use std::collections::HashMap;
use std::ffi::CStr;
use std::path::Path;
use std::sync::Mutex;

use libc::{c_char, c_int};

const MAX_PRINTED: usize = 10;

/// Location key: (filename, line_number)
#[derive(Clone, Hash, Eq, PartialEq)]
struct LocationKey {
    filename: String,
    line_number: i32,
}

/// Global state for backtrack tracking
struct BacktracksState {
    initialized: bool,
    current_function: Option<String>,
    last_backtrack_counts: u64,

    /// Permanent tables: function -> (generator -> count)
    function_to_generators: Option<HashMap<String, HashMap<String, u64>>>,
    /// Permanent tables: generator -> (location -> count)
    generator_to_locations: Option<HashMap<String, HashMap<LocationKey, u64>>>,

    /// Temporary tables for current run
    function_to_generators_tmp: Option<HashMap<String, HashMap<String, u64>>>,
    generator_to_locations_tmp: Option<HashMap<String, HashMap<LocationKey, u64>>>,
}

static STATE: Mutex<BacktracksState> = Mutex::new(BacktracksState {
    initialized: false,
    current_function: None,
    last_backtrack_counts: 0,
    function_to_generators: None,
    generator_to_locations: None,
    function_to_generators_tmp: None,
    generator_to_locations_tmp: None,
});

/// Get the basename of a filename path.
fn get_basename(filename: &str) -> &str {
    Path::new(filename)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(filename)
}

/// Initialize the backtracks tracking subsystem.
#[no_mangle]
pub extern "C" fn bennet_info_backtracks_init() {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        state.function_to_generators = Some(HashMap::new());
        state.generator_to_locations = Some(HashMap::new());
        state.initialized = true;
    }
}

/// Set the current function under test for backtrack tracking.
///
/// # Safety
/// `function_name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_backtracks_set_function_under_test(
    function_name: *const c_char,
) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    assert!(!function_name.is_null());
    let name = unsafe { CStr::from_ptr(function_name) }
        .to_string_lossy()
        .into_owned();

    state.current_function = Some(name);
}

/// Begin a test run for backtrack tracking.
#[no_mangle]
pub extern "C" fn bennet_info_backtracks_begin_run() {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    // Initialize temporary tables for this run
    state.function_to_generators_tmp = Some(HashMap::new());
    state.generator_to_locations_tmp = Some(HashMap::new());

    // Insert an empty table into function_to_generators if current_function doesn't exist
    let func = state.current_function.clone();
    if let Some(func) = func {
        if let Some(ref mut ftg) = state.function_to_generators {
            ftg.entry(func).or_insert_with(HashMap::new);
        }
    }
}

/// Get the last run's total backtrack count.
#[no_mangle]
pub extern "C" fn bennet_info_backtracks_last_total() -> u64 {
    let state = STATE.lock().unwrap();
    state.last_backtrack_counts
}

/// End a test run for backtrack tracking.
#[no_mangle]
pub extern "C" fn bennet_info_backtracks_end_run(record: bool) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    // Calculate last_backtrack_counts from temporary table
    let mut total_counts = 0u64;
    if let Some(ref ftg_tmp) = state.function_to_generators_tmp {
        for gen_counter in ftg_tmp.values() {
            for count in gen_counter.values() {
                total_counts += count;
            }
        }
    }
    state.last_backtrack_counts = total_counts;

    if !record {
        // Insert empty tables for any generators in tmp that don't exist in permanent
        let generator_names: Vec<String> = state
            .generator_to_locations_tmp
            .as_ref()
            .map(|gtl_tmp| gtl_tmp.keys().cloned().collect())
            .unwrap_or_default();

        if let Some(ref mut gtl) = state.generator_to_locations {
            for generator_name in generator_names {
                gtl.entry(generator_name).or_insert_with(HashMap::new);
            }
        }

        // Clear temporary tables
        state.function_to_generators_tmp = None;
        state.generator_to_locations_tmp = None;
        return;
    }

    // Merge function_to_generators_tmp into function_to_generators
    if let Some(ftg_tmp) = state.function_to_generators_tmp.take() {
        let ftg = state
            .function_to_generators
            .get_or_insert_with(HashMap::new);

        for (function_name, tmp_gen_counter) in ftg_tmp {
            let gen_counter = ftg.entry(function_name).or_insert_with(HashMap::new);

            for (generator, tmp_count) in tmp_gen_counter {
                *gen_counter.entry(generator).or_insert(0) += tmp_count;
            }
        }
    }

    // Merge generator_to_locations_tmp into generator_to_locations
    if let Some(gtl_tmp) = state.generator_to_locations_tmp.take() {
        let gtl = state
            .generator_to_locations
            .get_or_insert_with(HashMap::new);

        for (generator_name, tmp_loc_counter) in gtl_tmp {
            let loc_counter = gtl.entry(generator_name).or_insert_with(HashMap::new);

            for (loc_key, tmp_count) in tmp_loc_counter {
                *loc_counter.entry(loc_key).or_insert(0) += tmp_count;
            }
        }
    }
}

/// Log a backtrack event.
///
/// # Safety
/// `generator`, `filename` must be valid null-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_backtracks_log(
    generator: *const c_char,
    filename: *const c_char,
    line_number: c_int,
) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    let current_function = match &state.current_function {
        Some(f) => f.clone(),
        None => {
            assert!(false, "current_function must be set");
            return;
        }
    };

    assert!(!generator.is_null());
    assert!(!filename.is_null());

    let generator_str = unsafe { CStr::from_ptr(generator) }
        .to_string_lossy()
        .into_owned();
    let filename_str = unsafe { CStr::from_ptr(filename) }.to_string_lossy();
    let basename = get_basename(&filename_str).to_string();

    // Get or create generator counter for this function in tmp table
    let ftg_tmp = state
        .function_to_generators_tmp
        .get_or_insert_with(HashMap::new);
    let gen_counter = ftg_tmp
        .entry(current_function)
        .or_insert_with(HashMap::new);

    // Increment generator counter
    *gen_counter.entry(generator_str.clone()).or_insert(0) += 1;

    // Get or create location counter for this generator in tmp table
    let gtl_tmp = state
        .generator_to_locations_tmp
        .get_or_insert_with(HashMap::new);
    let loc_counter = gtl_tmp.entry(generator_str).or_insert_with(HashMap::new);

    // Increment location counter
    let loc_key = LocationKey {
        filename: basename,
        line_number,
    };
    *loc_counter.entry(loc_key).or_insert(0) += 1;
}

/// Print backtracking statistics.
#[no_mangle]
pub extern "C" fn bennet_info_backtracks_print_backtrack_info() {
    let state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    println!("=== BACKTRACKING STATISTICS ===\n");
    println!("====================");
    println!("FUNCTIONS UNDER TEST");
    println!("====================\n");

    // Iterate through all functions and their generator counters
    if let Some(ref ftg) = state.function_to_generators {
        for (function_name, gen_counter) in ftg {
            let total_backtracks: u64 = gen_counter.values().sum();

            println!("{}: {} backtracks", function_name, total_backtracks);
            if total_backtracks == 0 {
                continue;
            }

            // Collect generator counts for sorting
            let mut entries: Vec<_> = gen_counter
                .iter()
                .map(|(k, &v)| (k.clone(), v))
                .collect();

            // Sort descending by count
            entries.sort_by(|a, b| b.1.cmp(&a.1));

            // Limit to MAX_PRINTED
            let print_count = entries.len().min(MAX_PRINTED);

            // Print sorted generator counts
            for (generator, count) in entries.iter().take(print_count) {
                let percent = (*count as f64) / (total_backtracks as f64) * 100.0;
                if percent < 1.0 {
                    continue;
                }

                println!("  {}: {:.0}% ({} backtracks)", generator, percent, count);
            }
            println!();
        }
    }

    println!("===========");
    println!("GENERATORS");
    println!("===========\n");

    // Iterate through all generators and their location counters
    if let Some(ref gtl) = state.generator_to_locations {
        for (generator_name, loc_counter) in gtl {
            let total_backtracks: u64 = loc_counter.values().sum();

            println!("{}: {} backtracks", generator_name, total_backtracks);
            if total_backtracks == 0 {
                continue;
            }

            // Collect location counts for sorting
            let mut entries: Vec<_> = loc_counter
                .iter()
                .map(|(k, &v)| (k.clone(), v))
                .collect();

            // Sort descending by count
            entries.sort_by(|a, b| b.1.cmp(&a.1));

            // Limit to MAX_PRINTED
            let print_count = entries.len().min(MAX_PRINTED);

            // Print sorted location counts
            for (loc_key, count) in entries.iter().take(print_count) {
                let percent = (*count as f64) / (total_backtracks as f64) * 100.0;
                if percent < 1.0 {
                    continue;
                }

                println!(
                    "  {}:{}: {:.0}% ({} backtracks)",
                    loc_key.filename, loc_key.line_number, percent, count
                );
            }
            println!();
        }
    }
}
