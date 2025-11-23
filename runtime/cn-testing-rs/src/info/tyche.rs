//! Tyche JSON output for Bennet test generation.
//!
//! Outputs test results in the Tyche JSON format.

use std::ffi::CStr;

use libc::c_char;

use super::backtracks::bennet_info_backtracks_last_total;
use super::sizes::bennet_info_sizes_last_size;
use super::timing::get_last_timing_events;

/// DJB string hash function (matches the C implementation)
fn string_hash(s: &str) -> usize {
    let mut hash: usize = 5381;
    for c in s.bytes() {
        hash = hash.wrapping_shl(5).wrapping_add(hash).wrapping_add(c as usize);
    }
    hash
}

/// Tyche line info structure
#[repr(C)]
pub struct TycheLineInfo {
    pub test_suite: *mut c_char,
    pub test_name: *mut c_char,
    pub status: *mut c_char,
    pub status_reason: *mut c_char,
    pub suite_begin_time: u64,
    pub representation: *mut c_char,
}

/// Print a test summary in Tyche JSON format.
///
/// # Safety
/// - `out` must be a valid FILE pointer
/// - `line_info` must be a valid pointer to a TycheLineInfo struct
/// - All string fields in line_info must be valid null-terminated C strings
#[no_mangle]
pub unsafe extern "C" fn print_test_summary_tyche(out: *mut libc::FILE, line_info: *mut TycheLineInfo) {
    if out.is_null() || line_info.is_null() {
        return;
    }

    let info = unsafe { &*line_info };

    // Extract strings from line_info
    let test_suite = if info.test_suite.is_null() {
        String::new()
    } else {
        unsafe { CStr::from_ptr(info.test_suite) }
            .to_string_lossy()
            .into_owned()
    };

    let test_name = if info.test_name.is_null() {
        String::new()
    } else {
        unsafe { CStr::from_ptr(info.test_name) }
            .to_string_lossy()
            .into_owned()
    };

    let status = if info.status.is_null() {
        String::new()
    } else {
        unsafe { CStr::from_ptr(info.status) }
            .to_string_lossy()
            .into_owned()
    };

    let status_reason = if info.status_reason.is_null() {
        String::new()
    } else {
        unsafe { CStr::from_ptr(info.status_reason) }
            .to_string_lossy()
            .into_owned()
    };

    let representation = if info.representation.is_null() {
        String::new()
    } else {
        unsafe { CStr::from_ptr(info.representation) }
            .to_string_lossy()
            .into_owned()
    };

    // Build timing JSON object
    let mut timing_parts: Vec<String> = Vec::new();
    if let Some(timings) = get_last_timing_events() {
        for (event_name, duration_us) in timings {
            let duration_s = duration_us as f64 / 1_000_000.0;
            timing_parts.push(format!("\"{}\": {:.6}", event_name, duration_s));
        }
    }
    let timing_json = timing_parts.join(", ");

    let run_start = info.suite_begin_time as f64 / 1_000_000.0;

    // Format the JSON output
    let output = if status == "gave_up" {
        format!(
            "\n{{ \"type\": \"test_case\", \"property\": \"{}::{}\", \"arguments\": {{ \"prog\": \"{:x}\" }}, \"run_start\": {:.6}, \"status\": \"{}\", \"status_reason\": \"{}\", \"representation\": \"{}\", \"features\": {{}}, \"timing\": {{ {} }}, \"coverage\": {{}} }}\n",
            test_suite,
            test_name,
            string_hash(&representation),
            run_start,
            status,
            status_reason,
            representation,
            timing_json
        )
    } else {
        format!(
            "\n{{ \"type\": \"test_case\", \"property\": \"{}::{}\", \"arguments\": {{ \"n\": \"{:x}\" }}, \"run_start\": {:.6}, \"status\": \"{}\", \"status_reason\": \"{}\", \"representation\": \"{}\", \"features\": {{\"Memory Allocated (Bytes)\": {}, \"Backtracks\": {}, }}, \"timing\": {{ {} }}, \"coverage\": {{}} }}\n",
            test_suite,
            test_name,
            string_hash(&representation),
            run_start,
            status,
            status_reason,
            representation,
            bennet_info_sizes_last_size(),
            bennet_info_backtracks_last_total(),
            timing_json
        )
    };

    // Write to the FILE stream
    // Use libc::fprintf or write directly via libc::fwrite
    let bytes = output.as_bytes();
    unsafe {
        libc::fwrite(
            bytes.as_ptr() as *const libc::c_void,
            1,
            bytes.len(),
            out,
        );
    }
}
