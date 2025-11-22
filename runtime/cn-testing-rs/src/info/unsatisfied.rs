//! Constraint satisfaction statistics tracking for Bennet test generation.
//!
//! Tracks which constraints were satisfied vs unsatisfied during generation.

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

/// Satisfaction counters for a location
#[derive(Clone, Default)]
struct SatisfactionCounters {
    unsatisfied: u32,
    satisfied: u32,
}

/// Entry for a function's unsatisfied tracking
struct FunctionEntry {
    /// Location -> satisfaction counters
    loc_table: HashMap<LocationKey, SatisfactionCounters>,
    run_count: u32,
}

/// Global state for unsatisfied tracking
struct UnsatisfiedState {
    initialized: bool,
    current_function: Option<String>,
    /// Permanent table: function -> entry
    function_to_unsatisfied: Option<HashMap<String, FunctionEntry>>,
    /// Temporary table for current run: function -> (location -> bool)
    function_to_unsatisfied_tmp: Option<HashMap<String, HashMap<LocationKey, bool>>>,
}

static STATE: Mutex<UnsatisfiedState> = Mutex::new(UnsatisfiedState {
    initialized: false,
    current_function: None,
    function_to_unsatisfied: None,
    function_to_unsatisfied_tmp: None,
});

/// Get the basename of a filename path.
fn get_basename(filename: &str) -> &str {
    Path::new(filename)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(filename)
}

/// Initialize the unsatisfied tracking subsystem.
#[no_mangle]
pub extern "C" fn bennet_info_unsatisfied_init() {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        state.function_to_unsatisfied = Some(HashMap::new());
        state.initialized = true;
    }
}

/// Set the current function under test for unsatisfied tracking.
///
/// # Safety
/// `function_name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_unsatisfied_set_function_under_test(
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

/// Begin a test run for unsatisfied tracking.
#[no_mangle]
pub extern "C" fn bennet_info_unsatisfied_begin_run() {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    // Initialize temporary table for this run
    state.function_to_unsatisfied_tmp = Some(HashMap::new());

    // Insert an entry for current_function if it doesn't exist
    let func = state.current_function.clone();
    if let Some(func) = func {
        let ftu = state
            .function_to_unsatisfied
            .get_or_insert_with(HashMap::new);
        ftu.entry(func).or_insert_with(|| FunctionEntry {
            loc_table: HashMap::new(),
            run_count: 0,
        });
    }
}

/// End a test run for unsatisfied tracking.
#[no_mangle]
pub extern "C" fn bennet_info_unsatisfied_end_run(record: bool) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    if !record {
        state.function_to_unsatisfied_tmp = None;
        return;
    }

    // Merge function_to_unsatisfied_tmp into function_to_unsatisfied
    if let Some(ftu_tmp) = state.function_to_unsatisfied_tmp.take() {
        let ftu = state
            .function_to_unsatisfied
            .get_or_insert_with(HashMap::new);

        for (function_name, tmp_loc_table) in ftu_tmp {
            let entry = ftu.entry(function_name).or_insert_with(|| FunctionEntry {
                loc_table: HashMap::new(),
                run_count: 0,
            });

            // Increment run counter
            entry.run_count += 1;

            // For each location in tmp_loc_table, increment the appropriate counter
            for (loc_key, unsatisfied) in tmp_loc_table {
                let counters = entry
                    .loc_table
                    .entry(loc_key)
                    .or_insert_with(SatisfactionCounters::default);

                if unsatisfied {
                    counters.unsatisfied += 1;
                } else {
                    counters.satisfied += 1;
                }
            }
        }
    }
}

/// Log a satisfaction event.
///
/// # Safety
/// `filename` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_unsatisfied_log(
    filename: *const c_char,
    line_number: c_int,
    unsatisfied: bool,
) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    let current_function = match &state.current_function {
        Some(f) => f.clone(),
        None => return,
    };

    if filename.is_null() {
        return;
    }

    let filename_str = unsafe { CStr::from_ptr(filename) }.to_string_lossy();
    let basename = get_basename(&filename_str).to_string();

    // Get or create location table for current function in tmp table
    let ftu_tmp = state
        .function_to_unsatisfied_tmp
        .get_or_insert_with(HashMap::new);
    let loc_table = ftu_tmp
        .entry(current_function)
        .or_insert_with(HashMap::new);

    // Set or update the bool for this location
    let loc_key = LocationKey {
        filename: basename,
        line_number,
    };

    // Combine with existing value using AND (true && unsatisfied)
    let current = *loc_table.get(&loc_key).unwrap_or(&true);
    loc_table.insert(loc_key, current && unsatisfied);
}

/// Location info for sorting
struct LocInfo {
    loc: LocationKey,
    sat_percentage: f64,
    hit_percentage: f64,
}

/// Print satisfaction statistics.
#[no_mangle]
pub extern "C" fn bennet_info_unsatisfied_print_info() {
    let state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    println!("=== SATISFACTION STATISTICS ===\n");
    println!("====================");
    println!("FUNCTIONS UNDER TEST");
    println!("====================\n");

    if let Some(ref ftu) = state.function_to_unsatisfied {
        for (function_name, entry) in ftu {
            println!("{} ({} runs):", function_name, entry.run_count);

            // Collect locations into an array for sorting
            let mut infos: Vec<LocInfo> = Vec::new();

            for (loc, counters) in &entry.loc_table {
                let total = counters.unsatisfied + counters.satisfied;
                if total == 0 {
                    continue;
                }

                let sat_percentage = (counters.satisfied as f64 * 100.0) / (total as f64);
                let hit_percentage = if entry.run_count > 0 {
                    (total as f64 * 100.0) / (entry.run_count as f64)
                } else {
                    0.0
                };

                infos.push(LocInfo {
                    loc: loc.clone(),
                    sat_percentage,
                    hit_percentage,
                });
            }

            // Sort by sat_percentage ascending, then hit_percentage descending
            infos.sort_by(|a, b| {
                match a.sat_percentage.partial_cmp(&b.sat_percentage) {
                    Some(std::cmp::Ordering::Equal) => {}
                    ord => return ord.unwrap_or(std::cmp::Ordering::Equal),
                }
                match b.hit_percentage.partial_cmp(&a.hit_percentage) {
                    Some(std::cmp::Ordering::Equal) => {}
                    ord => return ord.unwrap_or(std::cmp::Ordering::Equal),
                }
                // If still equal, sort by filename then line number
                match a.loc.filename.cmp(&b.loc.filename) {
                    std::cmp::Ordering::Equal => a.loc.line_number.cmp(&b.loc.line_number),
                    ord => ord,
                }
            });

            // Limit to MAX_PRINTED
            let print_count = infos.len().min(MAX_PRINTED);

            // Print in sorted order
            for info in infos.iter().take(print_count) {
                if info.sat_percentage > 0.5 {
                    continue;
                }
                if info.hit_percentage < 0.1 {
                    continue;
                }

                println!(
                    "  {}:{}: {:.1}% satisfied, {:.1}% hit",
                    info.loc.filename, info.loc.line_number, info.sat_percentage, info.hit_percentage
                );
            }
            println!();
        }
    }
}
