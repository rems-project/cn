//! Test case registry
//!
//! This module manages the registration and storage of test cases.

use libc::c_char;
use std::sync::Mutex;
use crate::types::{CnTestCase, CnTestCaseFn, CN_TEST_MAX_TEST_CASES};

/// Global test case registry
// Safety: The registry is only accessed through the Mutex, ensuring thread safety
static TEST_REGISTRY: Mutex<TestRegistry> = Mutex::new(TestRegistry::new());

/// Registry for storing test cases
struct TestRegistry {
    test_cases: Vec<CnTestCase>,
}

// Safety: TestRegistry is only accessed through a Mutex, which ensures thread safety.
// The raw pointers in CnTestCase point to static string literals that live for the
// entire program duration, so they're safe to share across threads.
unsafe impl Send for TestRegistry {}
unsafe impl Sync for TestRegistry {}

impl TestRegistry {
    /// Create a new empty test registry
    const fn new() -> Self {
        TestRegistry {
            test_cases: Vec::new(),
        }
    }

    /// Register a test case
    fn register(&mut self, suite: *const c_char, name: *const c_char, func: CnTestCaseFn) -> bool {
        if self.test_cases.len() >= CN_TEST_MAX_TEST_CASES {
            return false;
        }

        self.test_cases.push(CnTestCase {
            suite,
            name,
            func: Some(func),
        });

        true
    }

    /// Get the number of registered test cases
    fn len(&self) -> usize {
        self.test_cases.len()
    }

    /// Get a reference to a test case by index
    fn get(&self, index: usize) -> Option<&CnTestCase> {
        self.test_cases.get(index)
    }

    /// Get a slice of all test cases
    fn all(&self) -> &[CnTestCase] {
        &self.test_cases
    }
}

/// Register a test case (C FFI)
///
/// # Safety
///
/// - `suite` and `name` must be valid null-terminated C strings
/// - `func` must be a valid function pointer
/// - Pointers must remain valid for the lifetime of the program
#[no_mangle]
pub unsafe extern "C" fn cn_register_test_case(
    suite: *const c_char,
    name: *const c_char,
    func: Option<CnTestCaseFn>,
) {
    let func = match func {
        Some(f) => f,
        None => {
            eprintln!("Error: Attempted to register test with null function pointer");
            std::process::exit(1);
        }
    };

    let mut registry = TEST_REGISTRY.lock().unwrap();
    if !registry.register(suite, name, func) {
        eprintln!("Error: Tried to register too many tests.");
        std::process::exit(1);
    }
}

/// Get the number of registered test cases
pub(crate) fn get_test_count() -> usize {
    TEST_REGISTRY.lock().unwrap().len()
}

/// Get a test case by index
pub(crate) fn get_test_case(index: usize) -> Option<CnTestCase> {
    TEST_REGISTRY.lock().unwrap().get(index).copied()
}

/// Execute a function with access to all test cases
pub(crate) fn with_test_cases<F, R>(f: F) -> R
where
    F: FnOnce(&[CnTestCase]) -> R,
{
    let registry = TEST_REGISTRY.lock().unwrap();
    f(registry.all())
}
