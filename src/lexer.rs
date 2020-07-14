//! # Lexing for MSL strings.
//!
//! This module uses [`logos`] to lex MSL strings.

// ---------------------------------------------------------------------------
// IMPORTS
// ---------------------------------------------------------------------------

use logos::{Logos, Lexer};
use rson_rs;

use crate::interpreter::Directive;
use crate::util::FilePos;

// ---------------------------------------------------------------------------
// TYPES
// ---------------------------------------------------------------------------

/// Type alias around a vector of tokens and file positions.
pub(crate) type Tokens = Vec<(Token, FilePos)>;

/// Type alias around a vector of references to tokens and file positions.
pub(crate) type TokenRefs<'a> = Vec<&'a (Token, FilePos)>;

/// A peakable iterator over tokens.
pub(crate) type TokenPeekable<'a> = 
    std::iter::Peekable<std::slice::Iter<'a, (Token, FilePos)>>;

// ---------------------------------------------------------------------------
// LEXING TOKENS
// ---------------------------------------------------------------------------

/// A token inside an MSL file.
///
/// This item is `pub` not `pub(crate)` to allow error types to contain tokens.
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    /// Error token, skipping whitespace.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"//?.*", logos::skip)]
    #[error]
    Error,

    #[regex(r"//!.*", metadata)]
    Metadata(String),

    /// A timestamp token
    #[regex(r"\d+.?\d*", timestamp)]
    Timestamp(f64),

    /// A plus token, used to indicate that an item is relative to the previous
    /// item.
    #[token("+")]
    Plus,

    /// A colon, separating a timestamp and a payload
    #[token(":")]
    Colon,

    /// Directives, which allow the more advanced features of a script
    #[regex(r"[a-zA-Z]+(\s*\([^)]+\))?", directive)]
    Directive(Directive),
    
    /// Opening braces, which will start the payload.
    #[token("{", payload)]
    Payload(String),

    /// Comma which separates commands.
    #[token(",")]
    Comma,

    /// Start of a block
    #[token("[")]
    BlockStart,

    /// End of a block
    #[token("]")]
    BlockEnd
}

// ---------------------------------------------------------------------------
// LEXING FUNCTIONS
// ---------------------------------------------------------------------------

/// Parses a metadata comment token.
fn metadata(lex: &mut Lexer<Token>) -> Option<String> {
    let slice = lex.slice();

    Some(slice.replace("//!", "").trim().to_owned())
}

/// Capture a timestamp token.
///
/// An invalid timestamp will return `None`.
fn timestamp(lex: &mut Lexer<Token>) -> Option<f64> {
    Some(lex.slice().parse().ok()?)
}

/// Capture a payload token.
///
/// Since the paylod itself can contain closing braces we need to to brace 
/// matching to find the end.
fn payload(lex: &mut Lexer<Token>) -> Option<String> {
    // Get a vector of chars of the remaining part of the source, so we can 
    // search for the end of the payload.
    let remainder: Vec<char> = lex.remainder().chars().collect();

    // Counters for number of opening and closing braces, remembering that
    // we've already had an opening brace in the token itself
    let mut opens: i32 = 1;
    let mut closes: i32 = 0;

    // Character counter
    let mut idx: usize = 0;

    // Loop over each character in the remainder, when closes matches opens
    // we have reached the end of the payload.
    while idx < remainder.len()
        && opens != closes 
    {
        match remainder[idx] {
            '{' => opens += 1,
            '}' => closes += 1,
            _ => ()
        };

        // Increment the character counter
        idx += 1;
    }

    // If closes == opens we matched the end of the payload and can bump this
    // token up to `idx`, and capture the payload in the string.
    if opens == closes {
        lex.bump(idx);
        let slice = lex.slice();

        // Drop the opening and closing brace from the payload string.
        Some(slice[1..slice.len() - 1].to_owned())
    }
    // If we reached the end of the source and couldn't match the payload then
    // the payload itself is improperly formatted, so this is an error.
    else {
        None
    }
}

/// Capture a directive
fn directive(lex: &mut Lexer<Token>) -> Option<Directive> {
    // Deserialise the slice as a directive from RSON
    match rson_rs::de::from_str(lex.slice()) {
        Ok(d) => Some(d),
        Err(_) => None
    }
}

// ---------------------------------------------------------------------------
// PUBLIC FUNCTIONS
// ---------------------------------------------------------------------------

/// Run the lexer over the given string
pub(crate) fn lex(source: &str) -> Tokens {

    // Get the lex struct from the source
    let lex = Token::lexer(source);

    // Map the spanned tokens into FilePos tokens
    lex.spanned()
        .map(|(t, s)| (t, FilePos::from_span(source, s)))
        .collect()
}

// ---------------------------------------------------------------------------
// IMPLEMENTATIONS
// ---------------------------------------------------------------------------

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Error => write!(f, "UNKNOWN"),
            Token::Plus => write!(f, "PLUS"),
            Token::Colon => write!(f, "COLON"),
            Token::Comma => write!(f, "COMMA"),
            Token::BlockStart => write!(f, "OPEN_SQUARE_BRACKET"),
            Token::BlockEnd => write!(f, "CLOSE_SQUARE_BRACKET"),
            Token::Metadata(s) => write!(f, "Metadata({})", s),
            Token::Timestamp(t) => write!(f, "Timestamp({})", t),
            Token::Directive(d) => write!(f, "{:?}", d),
            Token::Payload(_) => write!(f, "Payload(_)")
        }
    }
}

// ---------------------------------------------------------------------------
// TESTS
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Test that a metadata comment is lexed corretly
    #[test]
    fn test_metadata() {
        let mut lex = Token::lexer("//! Some metadata");

        assert_eq!(
            lex.next(), 
            Some(Token::Metadata(String::from("Some metadata")))
        );
        assert_eq!(lex.slice(), "//! Some metadata");
    }

    /// Test the lexing of a simple string
    #[test]
    fn test_simple() {
        let mut lex = Token::lexer("1.0: { Simple Text }");

        assert_eq!(lex.next(), Some(Token::Timestamp(1.0)));
        assert_eq!(lex.slice(), "1.0");

        assert_eq!(lex.next(), Some(Token::Colon));

        assert_eq!(
            lex.next(), 
            Some(Token::Payload(String::from(" Simple Text ")))
        );
        assert_eq!(lex.slice(), "{ Simple Text }");
    }

    /// Test the lexing of an offset command
    #[test]
    fn test_relative() {
        let mut lex = Token::lexer("+1.0: { Simple Text }");

        assert_eq!(lex.next(), Some(Token::Plus));

        assert_eq!(lex.next(), Some(Token::Timestamp(1.0)));
        assert_eq!(lex.slice(), "1.0");

        assert_eq!(lex.next(), Some(Token::Colon));

        assert_eq!(
            lex.next(), 
            Some(Token::Payload(String::from(" Simple Text ")))
        );
        assert_eq!(lex.slice(), "{ Simple Text }");
    }

    /// Test the lexing of a simple directive
    #[test]
    fn test_directive() {
        let mut lex = Token::lexer("1.0: WaitForEvent(\"EventId\")");

        assert_eq!(lex.next(), Some(Token::Timestamp(1.0)));
        assert_eq!(lex.slice(), "1.0");

        assert_eq!(lex.next(), Some(Token::Colon));

        assert_eq!(
            lex.next(),
            Some(Token::Directive(
                Directive::WaitForEvent(String::from("EventId")))
            )
        );
        assert_eq!(lex.slice(), "WaitForEvent(\"EventId\")");
    }

    /// Test a nested payload
    #[test]
    fn test_nested_payload() {
        let mut lex = Token::lexer("1.0: { SomeRson { msg: \"Hello World\"} }");

        assert_eq!(lex.next(), Some(Token::Timestamp(1.0)));
        assert_eq!(lex.slice(), "1.0");

        assert_eq!(lex.next(), Some(Token::Colon));

        assert_eq!(
            lex.next(),
            Some(Token::Payload(
                String::from(" SomeRson { msg: \"Hello World\"} ")
            ))
        );
        assert_eq!(lex.slice(), "{ SomeRson { msg: \"Hello World\"} }");
    }

    /// Test a complex script from a file.
    #[test]
    fn test_complex() {
        use std::fs::OpenOptions;
        use std::io::prelude::*;
        use crate::util::FilePos;

        // Load the file
        let mut script_file = OpenOptions::new()
            .read(true)
            .open("scripts/complex.msl")
            .unwrap();

        let mut script = String::new();
        script_file.read_to_string(&mut script).unwrap();

        // If no characters are loaded panic
        if script.len() == 0 {
            panic!(
                "Expected some characters to be loaded from \
                `scripts/complex.msl` but none were"
            );
        }
    
        // Lex the file
        let lex = Token::lexer(&script);

        // If any token in the file is an error then panic
        for (tok, span) in lex.spanned() {
            if tok == Token::Error {
                panic!(
                    "Cannot lex token from at {}", 
                    FilePos::from_span(&script, span)
                );
            }
        }
    }
}