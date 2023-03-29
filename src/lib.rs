//! A parser for openqasm 2.0.
//!
//! # Example
//! ```
//! use openqasm_parser::openqasm;
//! use std::path::Path;
//!
//! let program = openqasm::parse_openqasm(Path::new("openqasmfile.qasm"));
//! ```

pub mod openqasm;
mod parser;

