//! C-compatible types for the CN testing framework

use libc::{c_char, c_int};
use std::os::raw::c_void;
use crate::bindings;

/// Result of a test case execution
#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CnTestResult {
    CN_TEST_PASS = 0,
    CN_TEST_FAIL = 1,
    CN_TEST_GEN_FAIL = 2,
    CN_TEST_SKIP = 3,
}

/// Progress reporting level for test generation
#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CnTestGenProgress {
    CN_TEST_GEN_PROGRESS_NONE = 0,
    CN_TEST_GEN_PROGRESS_FINAL = 1,
    CN_TEST_GEN_PROGRESS_ALL = 2,
}

/// Input configuration for a test execution
#[repr(C)]
#[derive(Copy, Clone)]
pub struct CnTestInput {
    pub replay: bool,
    pub progress_level: CnTestGenProgress,
    pub sizing_strategy: bindings::bennet_sizing_strategy,
    pub trap: bool,
    pub replicas: bool,
    pub log_all_backtracks: bool,
    pub output_tyche: bool,
    pub tyche_output_stream: *mut c_void,  // Opaque FILE pointer
    pub begin_time: u64,
}

/// Function pointer type for test cases
pub type CnTestCaseFn =
    unsafe extern "C" fn(test_input: CnTestInput) -> CnTestResult;

/// Internal structure for storing registered test cases
#[repr(C)]
#[derive(Copy, Clone)]
pub struct CnTestCase {
    pub suite: *const c_char,
    pub name: *const c_char,
    pub func: Option<CnTestCaseFn>,
}

/// Reproduction information for a test
#[repr(C)]
pub struct CnTestReproduction {
    pub size: usize,
    pub checkpoint: bindings::bennet_rand_checkpoint,
}

/// Information for Tyche output format
#[repr(C)]
pub struct TycheLineInfo {
    pub test_suite: *mut c_char,
    pub test_name: *mut c_char,
    pub status: *mut c_char,
    pub status_reason: *mut c_char,
    pub suite_begin_time: u64,
    pub representation: *mut c_char,
}

/// Maximum number of test cases that can be registered
pub const CN_TEST_MAX_TEST_CASES: usize = 1000;

impl Default for CnTestResult {
    fn default() -> Self {
        CnTestResult::CN_TEST_SKIP
    }
}

impl From<c_int> for CnTestResult {
    fn from(value: c_int) -> Self {
        match value {
            0 => CnTestResult::CN_TEST_PASS,
            1 => CnTestResult::CN_TEST_FAIL,
            2 => CnTestResult::CN_TEST_GEN_FAIL,
            _ => CnTestResult::CN_TEST_SKIP,
        }
    }
}

impl From<CnTestResult> for c_int {
    fn from(result: CnTestResult) -> Self {
        result as c_int
    }
}
