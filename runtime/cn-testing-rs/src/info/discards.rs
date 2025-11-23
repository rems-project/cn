//! Discard statistics tracking for Bennet test generation.
//!
//! Tracks discarded test inputs by failure type for each function under test.

use std::collections::HashMap;
use std::ffi::CStr;
use std::sync::Mutex;

use libc::c_char;

// Failure type is a u32 typedef in the bindings
// Define constants to match the C enum values
const BENNET_FAILURE_NONE: u32 = 0;
const BENNET_FAILURE_TIMEOUT: u32 = 1;
const BENNET_FAILURE_BACKTRACK: u32 = 2;

const MAX_PRINTED: usize = 10;

/// Global state for discard tracking
struct DiscardsState {
    initialized: bool,
    current_function: Option<String>,
    /// Maps function name -> (failure_type -> count)
    function_to_discards: Option<HashMap<String, HashMap<u32, u64>>>,
}

static STATE: Mutex<DiscardsState> = Mutex::new(DiscardsState {
    initialized: false,
    current_function: None,
    function_to_discards: None,
});

fn failure_type_to_string(failure_type: u32) -> &'static str {
    match failure_type {
        BENNET_FAILURE_NONE => "NONE",
        BENNET_FAILURE_TIMEOUT => "TIMEOUT",
        BENNET_FAILURE_BACKTRACK => "BACKTRACK",
        _ => "UNKNOWN",
    }
}

/// Initialize the discards tracking subsystem.
#[no_mangle]
pub extern "C" fn bennet_info_discards_init() {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        state.function_to_discards = Some(HashMap::new());
        state.initialized = true;
    }
}

/// Set the current function under test for discard tracking.
///
/// # Safety
/// `function_name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_discards_set_function_under_test(
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

    // Insert an empty table if this function doesn't exist
    if let Some(ref mut ftd) = state.function_to_discards {
        ftd.entry(name.clone()).or_insert_with(HashMap::new);
    }

    state.current_function = Some(name);
}

/// Log a discard event for the current function.
#[no_mangle]
pub extern "C" fn bennet_info_discards_log(failure_type: u32) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    let current_function = match &state.current_function {
        Some(f) => f.clone(),
        None => return,
    };

    // Get or create the discard table for this function
    let ftd = state
        .function_to_discards
        .get_or_insert_with(HashMap::new);
    let discard_table = ftd.entry(current_function).or_insert_with(HashMap::new);

    // Increment count for this failure type
    *discard_table.entry(failure_type).or_insert(0) += 1;
}

/// Print discard statistics.
#[no_mangle]
pub extern "C" fn bennet_info_discards_print_info() {
    let state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    println!("=== DISCARD STATISTICS ===\n");
    println!("====================");
    println!("FUNCTIONS UNDER TEST");
    println!("====================\n");

    if let Some(ref ftd) = state.function_to_discards {
        for (function_name, discard_table) in ftd {
            let total_count: u64 = discard_table.values().sum();

            println!("{}: {} discards", function_name, total_count);
            if total_count == 0 {
                continue;
            }

            // Collect entries for sorting
            let mut entries: Vec<_> = discard_table
                .iter()
                .map(|(&ft, &count)| (ft, count))
                .collect();

            // Sort descending by count
            entries.sort_by(|a, b| b.1.cmp(&a.1));

            // Limit to MAX_PRINTED
            let print_count = entries.len().min(MAX_PRINTED);

            // Print sorted discard counts
            for (failure_type, count) in entries.iter().take(print_count) {
                let percent = (*count as f64) / (total_count as f64) * 100.0;
                if percent < 1.0 {
                    continue;
                }

                println!(
                    "  {}: {:.0}% ({} discards)",
                    failure_type_to_string(*failure_type),
                    percent,
                    count
                );
            }
            println!();
        }
    }
}
