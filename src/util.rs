//! # Utility functions
//!
//! This module provides some small utility functions for test processing.

// ---------------------------------------------------------------------------
// IMPORTS
// ---------------------------------------------------------------------------

use std::ops::Range;
use std::fmt;

// ---------------------------------------------------------------------------
// DATA STRUCTURES
// ---------------------------------------------------------------------------

/// A position in a file.
#[derive(Debug, Copy, Clone)]
pub struct FilePos {
    /// Line number in the file
    pub line: usize,

    /// Column number in the file
    pub column: usize,

    /// Length of the spanned part of the file.
    pub length: Option<usize>
}

// ---------------------------------------------------------------------------
// PUBLIC FUNCTIONS
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// IMPLEMENTATIONS
// ---------------------------------------------------------------------------

impl FilePos {
    
    /// Convert an index into a file string to a `FilePos` struct.
    pub fn from_index(s: &str, i: usize) -> Self {

        // Counters for line and column, starting each at one as this is the 
        // convension for line numbers.
        let mut line = 1;
        let mut col = 1;

        // Loop through the parts of the string, if a newline is found 
        // increment the line counter and reset the column counter, up to i.
        for idx in 0..i {
            if s[idx..idx+1] == *"\n" {
                line += 1;
                col = 1;
            }

            col += 1;
        }

        Self {
            line,
            column: col,
            length: None
        }
    }

    /// Converts a span (`Range<usize>`) to a `FilePos` struct.
    pub fn from_span(s: &str, span: Range<usize>) -> Self {

        // Counters for line and column, starting each at one as this is the 
        // convension for line numbers.
        let mut line = 1;
        let mut col = 1;

        // Loop through the parts of the string, if a newline is found 
        // increment the line counter and reset the column counter, up to i.
        for idx in 0..span.start {
            if s[idx..idx+1] == *"\n" {
                line += 1;
                col = 1;
            }

            col += 1;
        }

        Self {
            line,
            column: col,
            length: Some(span.end - span.start)
        }
    }
}

impl fmt::Display for FilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "line {}:{}{}",
            self.line, self.column,
            match self.length {
                Some(l) => format!(" ({} long)", l),
                None => String::from("")
            }
        )
    }
}