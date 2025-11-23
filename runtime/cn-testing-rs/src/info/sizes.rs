//! Size statistics tracking for Bennet test generation.
//!
//! Tracks memory allocation sizes for generated test inputs.

use std::collections::HashMap;
use std::ffi::CStr;
use std::sync::Mutex;

use libc::c_char;

const MAX_PRINTED: usize = 10;

extern "C" {
    fn bennet_ownership_size() -> usize;
}

/// Global state for size tracking
struct SizesState {
    initialized: bool,
    current_function: Option<String>,
    last_size: usize,
    /// Maps function name -> (size -> count)
    function_to_sizes: Option<HashMap<String, HashMap<usize, usize>>>,
}

static STATE: Mutex<SizesState> = Mutex::new(SizesState {
    initialized: false,
    current_function: None,
    last_size: 0,
    function_to_sizes: None,
});

/// Initialize the sizes tracking subsystem.
#[no_mangle]
pub extern "C" fn bennet_info_sizes_init() {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        state.function_to_sizes = Some(HashMap::new());
        state.initialized = true;
    }
}

/// Set the current function under test for size tracking.
///
/// # Safety
/// `function_name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_sizes_set_function_under_test(function_name: *const c_char) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    assert!(!function_name.is_null());
    let name = unsafe { CStr::from_ptr(function_name) }
        .to_string_lossy()
        .into_owned();

    // Insert an empty table if this function doesn't exist
    if let Some(ref mut fts) = state.function_to_sizes {
        fts.entry(name.clone()).or_insert_with(HashMap::new);
    }

    state.current_function = Some(name);
}

/// Log the current allocation size.
#[no_mangle]
pub extern "C" fn bennet_info_sizes_log() {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    let current_function = match &state.current_function {
        Some(f) => f.clone(),
        None => return,
    };

    // Get the current ownership size from C
    let size = unsafe { bennet_ownership_size() };
    state.last_size = size;

    // Get or create the size table for this function
    let fts = state.function_to_sizes.get_or_insert_with(HashMap::new);
    let size_table = fts.entry(current_function).or_insert_with(HashMap::new);

    // Increment count for this size
    *size_table.entry(size).or_insert(0) += 1;
}

/// Get the last logged size.
#[no_mangle]
pub extern "C" fn bennet_info_sizes_last_size() -> usize {
    let state = STATE.lock().unwrap();
    state.last_size
}

/// Print size statistics.
#[no_mangle]
pub extern "C" fn bennet_info_sizes_print_info() {
    let state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    println!("=== SIZE STATISTICS ===\n");
    println!("====================");
    println!("FUNCTIONS UNDER TEST");
    println!("====================\n");

    if let Some(ref fts) = state.function_to_sizes {
        for (function_name, size_table) in fts {
            let total_count: usize = size_table.values().sum();

            println!("{}: {} inputs", function_name, total_count);
            if total_count == 0 {
                continue;
            }

            // Collect entries for sorting
            let mut entries: Vec<_> = size_table.iter().map(|(&sz, &cnt)| (sz, cnt)).collect();

            // Sort descending by count
            entries.sort_by(|a, b| b.1.cmp(&a.1));

            // Limit to MAX_PRINTED
            let print_count = entries.len().min(MAX_PRINTED);

            // Print sorted size counts
            for (size, count) in entries.iter().take(print_count) {
                let percent = (*count as f64) / (total_count as f64) * 100.0;
                if percent < 1.0 {
                    continue;
                }

                println!("  {}: {:.0}% ({} inputs)", size, percent, count);
            }
            println!();
        }
    }
}
