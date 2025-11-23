//! CN Testing Runtime Library (Rust Implementation)
//!
//! This library provides the test harness infrastructure for CN's test generation
//! and execution system. It exposes a C ABI for compatibility with existing C test code.

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

mod bindings;
mod types;
mod registry;
mod executor;
mod cli;
mod utils;
pub mod info;

pub use types::*;
pub use registry::cn_register_test_case;
pub use utils::{print_test_info, cn_trap};

// Re-export the main entry point
pub use executor::cn_test_main;
