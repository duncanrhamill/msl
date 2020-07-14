//! # Mission Scripting Language
//!
//! The mission scripting language (MSL) is a way of scripting the execution of
//! a mission. It is a deterministic, timeline-drive, event-capable language
//! which allows `rust` objects to be generated at specific times.
//!
//! ## Writing script files
//!
//! A number of example scripts are provided in `scripts/`. A simple script may
//! look like this:
//!
//! ```text
//! 1.0: { TcType::Heartbeat },
//! 10.0: { TcType::Shutdown }
//! ```
//!
//! This short script contains two timed commands, one at `1.0` time units and
//! another at `10.0` (the units may be seconds, for instance).

// ---------------------------------------------------------------------------
// MODULES
// ---------------------------------------------------------------------------

mod interpreter;
mod lexer;
mod parser;
mod util;

// ---------------------------------------------------------------------------
// EXPORTS
// ---------------------------------------------------------------------------

pub use interpreter::Interpreter;