//! Utility functions for the test framework

use libc::{c_char, c_int};
use std::ffi::CStr;
use std::io::{self, Write};

/// Print information about a test case
///
/// # Safety
///
/// - `suite` and `name` must be valid null-terminated C strings
#[no_mangle]
pub unsafe extern "C" fn print_test_info(
    suite: *const c_char,
    name: *const c_char,
    tests: c_int,
    discards: c_int,
) {
    let suite_str = CStr::from_ptr(suite).to_str().unwrap_or("<invalid>");
    let name_str = CStr::from_ptr(name).to_str().unwrap_or("<invalid>");

    if tests == 0 && discards == 0 {
        print!("Testing {}::{}:", suite_str, name_str);
    } else if discards == 0 {
        print!("Testing {}::{}: {} runs", suite_str, name_str, tests);
    } else {
        print!(
            "Testing {}::{}: {} runs, {} discards",
            suite_str, name_str, tests, discards
        );
    }

    io::stdout().flush().ok();
}

/// Platform-specific debug trap implementation
///
/// This function triggers a debug breakpoint on supported platforms.
#[no_mangle]
pub extern "C" fn cn_trap() {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    unsafe {
        std::arch::asm!("int3");
    }

    #[cfg(target_arch = "aarch64")]
    unsafe {
        std::arch::asm!(".inst 0xd4200000");
    }

    #[cfg(target_arch = "arm")]
    unsafe {
        std::arch::asm!(".inst 0xe7f001f0");
    }

    #[cfg(not(any(
        target_arch = "x86",
        target_arch = "x86_64",
        target_arch = "aarch64",
        target_arch = "arm"
    )))]
    {
        // Fallback to raising SIGTRAP if available
        #[cfg(unix)]
        unsafe {
            libc::raise(libc::SIGTRAP);
        }

        #[cfg(not(unix))]
        unsafe {
            libc::raise(libc::SIGABRT);
        }
    }
}

// Note: bennet_compute_size is provided by libbennet.a, not exported from Rust
// to avoid duplicate symbol errors.
