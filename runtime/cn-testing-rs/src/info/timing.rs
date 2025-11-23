//! Timing statistics tracking for Bennet test generation.
//!
//! Tracks timing events for various operations during test generation.

use std::collections::HashMap;
use std::ffi::CStr;
use std::sync::Mutex;
use std::time::Instant;

use libc::c_char;

const MAX_PRINTED: usize = 10;

/// Timing statistics for a single event
struct TimingStats {
    min: i64,       // Minimum duration (microseconds)
    max: i64,       // Maximum duration (microseconds)
    total: i64,     // Total duration across all runs (microseconds)
    count: u64,     // Number of times this event was measured
    last: i64,      // Last measured duration (microseconds)
    start: Option<Instant>, // Start time for current measurement
}

impl TimingStats {
    fn new() -> Self {
        Self {
            min: i64::MAX,
            max: 0,
            total: 0,
            count: 0,
            last: 0,
            start: None,
        }
    }
}

/// Global state for timing tracking
struct TimingState {
    initialized: bool,
    current_function: Option<String>,
    /// function -> (event_name -> stats)
    function_to_timing: Option<HashMap<String, HashMap<String, TimingStats>>>,
}

static STATE: Mutex<TimingState> = Mutex::new(TimingState {
    initialized: false,
    current_function: None,
    function_to_timing: None,
});

/// Initialize the timing tracking subsystem.
#[no_mangle]
pub extern "C" fn bennet_info_timing_init() {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        state.function_to_timing = Some(HashMap::new());
        state.initialized = true;
    }
}

/// Set the current function under test for timing tracking.
///
/// # Safety
/// `function_name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_timing_set_function_under_test(function_name: *const c_char) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    assert!(!function_name.is_null());
    let name = unsafe { CStr::from_ptr(function_name) }
        .to_string_lossy()
        .into_owned();

    // Create an empty timing stats table for this function if it doesn't exist
    if let Some(ref mut ftt) = state.function_to_timing {
        ftt.entry(name.clone()).or_insert_with(HashMap::new);
    }

    state.current_function = Some(name);
}

/// Start timing an event.
///
/// # Safety
/// `event_name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_timing_start(event_name: *const c_char) {
    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    let current_function = match &state.current_function {
        Some(f) => f.clone(),
        None => return,
    };

    assert!(!event_name.is_null());
    let event = unsafe { CStr::from_ptr(event_name) }
        .to_string_lossy()
        .into_owned();

    // Get the stats table for current function
    let ftt = state
        .function_to_timing
        .get_or_insert_with(HashMap::new);
    let stats_table = ftt.entry(current_function).or_insert_with(HashMap::new);

    // Get or create timing stats for this event
    let stats = stats_table.entry(event).or_insert_with(TimingStats::new);

    // Record start time
    stats.start = Some(Instant::now());
}

/// End timing an event.
///
/// # Safety
/// `event_name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn bennet_info_timing_end(event_name: *const c_char) {
    let end_time = Instant::now();

    let mut state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    let current_function = match &state.current_function {
        Some(f) => f.clone(),
        None => return,
    };

    assert!(!event_name.is_null());
    let event = unsafe { CStr::from_ptr(event_name) }
        .to_string_lossy()
        .into_owned();

    // Get the stats table for current function
    let ftt = match &mut state.function_to_timing {
        Some(t) => t,
        None => return,
    };
    let stats_table = match ftt.get_mut(&current_function) {
        Some(t) => t,
        None => return,
    };

    // Get timing stats for this event
    let stats = match stats_table.get_mut(&event) {
        Some(s) => s,
        None => return,
    };

    // Calculate duration
    let start_time = match stats.start.take() {
        Some(t) => t,
        None => return,
    };

    let duration = end_time.duration_since(start_time);
    let duration_us = duration.as_micros() as i64;

    // Update statistics
    if duration_us < stats.min {
        stats.min = duration_us;
    }
    if duration_us > stats.max {
        stats.max = duration_us;
    }
    stats.total += duration_us;
    stats.count += 1;
    stats.last = duration_us;
}

/// Get the last timing values for all events.
/// Returns a HashMap that can be used internally by tyche.rs.
pub fn get_last_timing_events() -> Option<HashMap<String, i64>> {
    let state = STATE.lock().unwrap();
    if !state.initialized {
        return None;
    }

    let current_function = state.current_function.as_ref()?;

    let ftt = state.function_to_timing.as_ref()?;
    let stats_table = ftt.get(current_function)?;

    let mut result = HashMap::new();
    for (event_name, stats) in stats_table {
        result.insert(event_name.clone(), stats.last);
    }

    Some(result)
}

/// Print timing statistics.
#[no_mangle]
pub extern "C" fn bennet_info_timing_print_info() {
    let state = STATE.lock().unwrap();
    if !state.initialized {
        return;
    }

    println!("=== TIMING STATISTICS ===\n");
    println!("====================");
    println!("FUNCTIONS UNDER TEST");
    println!("====================\n");

    if let Some(ref ftt) = state.function_to_timing {
        for (function_name, stats_table) in ftt {
            println!("{}:", function_name);

            if stats_table.is_empty() {
                println!("  No timing data\n");
                continue;
            }

            // Collect timing entries for sorting
            let mut entries: Vec<_> = stats_table
                .iter()
                .map(|(event, stats)| {
                    let avg = if stats.count > 0 {
                        stats.total / stats.count as i64
                    } else {
                        0
                    };
                    (event.clone(), avg, stats.count)
                })
                .collect();

            // Sort descending by average duration
            entries.sort_by(|a, b| b.1.cmp(&a.1));

            let print_count = entries.len().min(MAX_PRINTED);

            // Print sorted timing stats
            for (event_name, _, _) in entries.iter().take(print_count) {
                let stats = stats_table.get(event_name).unwrap();
                let avg_us = if stats.count > 0 {
                    stats.total / stats.count as i64
                } else {
                    0
                };

                println!("  {}:", event_name);
                println!("    Count: {}", stats.count);
                println!("    Min: {:.6} s", stats.min as f64 / 1_000_000.0);
                println!("    Max: {:.6} s", stats.max as f64 / 1_000_000.0);
                println!("    Avg: {:.6} s", avg_us as f64 / 1_000_000.0);
                println!("    Last: {:.6} s", stats.last as f64 / 1_000_000.0);
            }

            println!();
        }
    }
}
