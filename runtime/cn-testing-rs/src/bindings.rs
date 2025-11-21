//! FFI bindings to C libraries
//!
//! This module contains automatically generated bindings to:
//! - Bennet: Test generation framework
//! - CN-Executable: Core utilities
//! - CN-SMT: SMT solver interface
//! - CN-Replicate: Counterexample replication

#![allow(dead_code)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(improper_ctypes)]

// Include generated bindings
pub mod bennet {
    include!(concat!(env!("OUT_DIR"), "/bennet_bindings.rs"));
}

pub mod utils {
    include!(concat!(env!("OUT_DIR"), "/utils_bindings.rs"));
}

pub mod smt {
    include!(concat!(env!("OUT_DIR"), "/smt_bindings.rs"));
}

pub mod replicate {
    include!(concat!(env!("OUT_DIR"), "/replicate_bindings.rs"));
}

// Re-export commonly used types and functions
pub use bennet::*;
pub use utils::*;
pub use smt::*;
