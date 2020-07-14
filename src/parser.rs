//! # Parser for MSL files
//!
//! This module provides for parsing of MSL files.

// ---------------------------------------------------------------------------
// IMPORTS
// ---------------------------------------------------------------------------

use semver::Version;
use rson_rs;
use thiserror::Error;
use regex::Regex;
use crate::lexer::{self, Token, Tokens, TokenPeekable};
use crate::util::FilePos;
use crate::interpreter::*;

// ---------------------------------------------------------------------------
// ENUMERATIONS
// ---------------------------------------------------------------------------

/// Errors which can occur during parsing.
#[allow(dead_code)]
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Source is empty")]
    EmptySource,

    #[error("Expected some tokens but none were found")]
    EmptyTokens,

    #[error("Unrecognised tokens:\n{0:#?}")]
    TokenErrors(Tokens),

    #[error("Expected a {0}, found {1} at {2}")]
    UnexpectedPunctuation(Token, Token, FilePos),

    #[error("Expected {0}, found {1} at {2}")]
    UnexpectedToken(&'static str, Token, FilePos),

    #[error("Expected {0}, but reached the end of the token stream")]
    EndOfTokens(&'static str),

    #[error("Invalid metadata comment \"{0}\" at {1}")]
    InvalidMetadataComment(String, FilePos),

    #[error("Invalid MSL version: {0}")]
    InvalidMslVersion(semver::SemVerError),

    #[error("Invalid payload type: {0}")]
    InvalidPayloadType(rson_rs::de::Error),

    #[error("Payload at {0} is invalid: {1}")]
    InvalidPayload(FilePos, String)
}

// ---------------------------------------------------------------------------
// PUBLIC FUNCTIONS
// ---------------------------------------------------------------------------

/// Parse a string into an MSL script.
pub(crate) fn parse(source: &str) -> Result<Script, ParseError> {
    // If the string is empty return now to save time.
    if source.len() == 0 {
        return Err(ParseError::EmptySource)
    }

    // Lex the source
    let mut toks = lexer::lex(source);

    // Search through the tokens for errors, storing their indexes in a 
    // separate vector.
    let mut num_errors = 0;

    for (tok, _) in toks.iter() {
        if *tok == Token::Error {
            num_errors += 1;
        }
    }

    // If there were any errors return now
    if num_errors > 0 {
        // Discard non error tokens
        toks.retain(|tok| tok.0 == Token::Error);

        return Err(ParseError::TokenErrors(toks))
    }
    
    // Create a peekable iterator over the tokens
    let mut tok_peekable: TokenPeekable = toks.iter().peekable();

    // Parse the metadata tokens
    let metadata = parse_metadata(&mut tok_peekable)?;

    // Create the root block
    let root = parse_block(
        &mut tok_peekable, 
        true, 
        &metadata
    )?;

    Ok(Script {
        metadata,
        root
    })
}

// ---------------------------------------------------------------------------
// PRIVATE FUNCTIONS
// ---------------------------------------------------------------------------

/// Parse the metadata tokens contained within the file.
///
/// This will convert the metadata comments found in an MSL file into a
/// metadata struct. See [here](crate) for metadata options.
///
/// This function takes a [`TokenPeekable`] argument as it is designed to be 
/// called as a part of the higher level [`parse`] function, which allows 
/// iteration over the tokens in nested functions while maintaining the iter
/// state.
fn parse_metadata(tokens: &mut TokenPeekable) -> Result<Metadata, ParseError> 
{

    // Metadata to be returned
    let mut metadata = Metadata::default();

    // Regex used to match a key and value in the metadata comments
    let md_regex = Regex::new(r"^(\w+)\s*:\s*(.+)$").unwrap();

    // While we still find metadata tokens parse them, if there is no token
    // return the metadata struct
    loop {
        match tokens.peek() {
            // Parse the metadata
            Some((Token::Metadata(s), pos)) => {
                // Check that the comment matches the regex
                if md_regex.is_match(s) {

                    // Match the groups in the regex
                    let caps = md_regex.captures(s).unwrap();

                    // Switch based on the key (first element)
                    match &caps[1] {
                        "msl_version" => {
                            // Parse the version number
                            match Version::parse(&caps[2]) {
                                Ok(v) => metadata.version = v,
                                Err(e) => return Err(
                                    ParseError::InvalidMslVersion(e)
                                )
                            }
                        },
                        "payload_format" => {
                            // Parse the type of the payload.
                            match rson_rs::de::from_str(&caps[2]) {
                                Ok(t) => metadata.payload_format = t,
                                Err(e) => return Err(
                                    ParseError::InvalidPayloadType(e)
                                )
                            }
                        },
                        _ => {
                            // Invalid key
                            return Err(
                                ParseError::InvalidMetadataComment(
                                    s.to_owned(), *pos
                                )
                            )
                        }
                    }

                }
                else {
                    return Err(
                        ParseError::InvalidMetadataComment(s.to_owned(), *pos)
                    )
                }

                // Consume this token
                tokens.next();
            },
            // Not a metadata, return the constructed metadata struct
            //
            // Note that this will also just return if the file only contains
            // metadata, which is ok as the parse_block function will check to
            // make sure there is actually a script.
            _ => return Ok(metadata)
        }
    }
}

/// Recursively parse a block.
///
/// This function will recursively descend into blocks in order to parse
/// them.
///
/// The calling function shall consume the starting "[", so that the next token
/// is the first item in the block.
///
/// If this is a root block the `is_root` argument shall be true. This will 
/// cause the function to not look for an ending "]". If it is not the root
/// block the caller shall pass `is_root = false`.
///
/// This function takes a [`TokenPeekable`] argument as it is designed to be 
/// called as a part of the higher level [`parse`] function, which allows 
/// iteration over the tokens in nested functions while maintaining the iter
/// state.
fn parse_block(
    tokens: &mut TokenPeekable, 
    is_root: bool,
    metadata: &Metadata
) -> Result<Block, ParseError> 
{
    // If there are no tokens left then raise the end of tokens error.
    if tokens.peek().is_none() {
        return match is_root {
            true => Err(ParseError::EndOfTokens("script")),
            false => Err(ParseError::EndOfTokens("block"))
        }
    }

    // Vector of items to be populated
    let mut items = Vec::new();

    // Parse items while there are tokens left in the script
    while tokens.peek().is_some() {
        items.push(parse_item(tokens, metadata)?);

        // Expect a comma to separate items, an end-of-block to end the block,
        // or end of tokens.
        match tokens.peek() {
            Some((Token::Comma, _)) => {
                // Consume the comma
                tokens.next();
            },
            Some((Token::BlockEnd, _)) => {
                // Consume the block
                tokens.next();

                // exit the loop
                break;
            }
            Some((t, p)) => return Err(
                ParseError::UnexpectedToken(
                    "COMMA or CLOSE_SQUARE_BRACKET", t.clone(), *p
                )
            ),
            // If this is the end of the script the while loop will break
            None => ()
        }
    }

    // Return the block
    //
    // Note: Since blocks start relative to their containing item this will be
    // modified inside the interpreter to be correct.
    Ok(Block {
        start_time: 0.0,
        last_fired_index: None,
        items
    })

}

/// Parse an item from a token list.
///
/// This function will parse a single item from a list of tokens. If the item
/// contains a block this function will descend into that block and parse it.
///
/// The [`tokens`] argument should point to the first token in the item, this
/// can be one of:
///
/// - [`Token::Timestamp`] - the item begins with a timestamp
/// - [`Token::Plus`] - the item begins with a relative timestamp
/// - [`Token::Directive`] - the item begins with a directive, equivalent to a
///   zero timestamp followed by a directive.
/// - [`Token::Payload`] - the item begins with a payload, equivalent to a zero
///   timestamp followed by a payload.
///
/// Any other starting tokens are considered invalid.
///
/// An item is expected to match one of the following formats (square brackets
/// indicate optional items):
///
/// ```text
/// [+] TIMESTAMP : [DIRECTIVE [BLOCK | PAYLOAD] | PAYLOAD]
/// [+] [DIRECTIVE [BLOCK | PAYLOAD] | PAYLOAD]
/// ```
///
/// This function takes a [`TokenPeekable`] argument as it is designed to be 
/// called as a part of the higher level [`parse`] function, which allows 
/// iteration over the tokens in nested functions while maintaining the iter
/// state.
fn parse_item(
    tokens: &mut TokenPeekable, 
    metadata: &Metadata
) -> Result<Item, ParseError> 
{

    // If there are no tokens return an error
    if tokens.len() == 0 {
        return Err(ParseError::EmptyTokens)
    }
    
    // Check if relative, should be first token
    let is_relative = match tokens.peek() {
        Some((Token::Plus, _)) => {
            // Consume this token
            tokens.next();
            true
        },
        Some(_) => false,
        None => return Err(ParseError::EndOfTokens("PLUS"))
    };

    // If there's a timestamp use that, otherwise use 0.
    let trigger_time = match tokens.peek() {
        Some((Token::Timestamp(t), _)) => {
            // Consume this token
            tokens.next();

            // Consume the colon that follows a timestamp
            match tokens.next() {
                Some((Token::Colon, _)) => (),
                Some((t, p)) => return Err(
                    ParseError::UnexpectedPunctuation(
                        Token::Colon, t.clone(), *p
                    )
                ),
                None => return Err(ParseError::EndOfTokens("COLON"))
            }

            // Return the timestamp
            *t
        },
        Some(_) => 0.0,
        None => return Err(ParseError::EndOfTokens("Timestamp"))
    };

    // Next we expect either a directive or a payload, or since some directives
    // don't have these, a comma, end block, or end of script
    let (data, block) = match tokens.peek() {
        Some((Token::Directive(d), _)) => {
            // Consume the token
            tokens.next();

            // If there's a block start, consume it and call the parse_block
            // function.
            let block = match tokens.peek() {
                Some((Token::BlockStart, _)) => {
                    // Consume
                    tokens.next();

                    // Parse
                    Some(parse_block(tokens, false, metadata)?)
                }
                _ => None
            };

            // If it's a payload, then the data is DirectiveAndObject, 
            // otherwise it's just Directive.
            let data = match tokens.peek() {
                Some((Token::Payload(s), _)) => 
                    ItemData::DirectiveAndObject(d.clone(), s.clone()),
                _ => ItemData::Directive(d.clone())
            };

            (data, block)
        },
        Some((Token::Payload(s), pos)) => {
            // Consume this token
            tokens.next();

            // Parse the string to check that it is valid in the script's
            // payload format
            // TODO
            
            (ItemData::Object(s.clone()), None)
        },
        Some((t, p)) => return Err(
            ParseError::UnexpectedToken("Directive or Payload", t.clone(), *p)
        ),
        None => return Err(ParseError::EndOfTokens("Directive or payload"))
    };


    // Build the item
    Ok(Item {
        fired: false,
        is_relative,
        timestamp: None,
        trigger_time,
        data,
        block
    })
}

// ---------------------------------------------------------------------------
// IMPLEMENTATIONS
// ---------------------------------------------------------------------------

impl Default for Metadata {
    fn default() -> Self {
        Self {
            version: Version::parse(env!("CARGO_PKG_VERSION")).unwrap(),
            payload_format: PayloadFormat::Rson
        }
    }
}

// ---------------------------------------------------------------------------
// TESTS
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Test metadata parsing
    #[test]
    fn test_metadata() {

        // Lex two metadata comments
        let toks = lexer::lex(
            "//! msl_version: 1.0.0\n//! payload_format: JSON"
        );

        // Parse the metadata
        let md = parse_metadata(&mut toks.iter().peekable())
            .unwrap();

        // Assert that the metadata struct matches the expected format
        assert_eq!(md.version, Version::parse("1.0.0").unwrap());
        assert_eq!(md.payload_format, PayloadFormat::Json);
    }

    /// Test that the complex script can be parsed
    #[test]
    fn test_complex() {
        use std::fs::OpenOptions;
        use std::io::prelude::*;

        // Open the file
        let mut script_file = OpenOptions::new()
            .read(true)
            .open("scripts/complex.msl")
            .unwrap();
        
        // Read the script from the file
        let mut script = String::new();
        script_file.read_to_string(&mut script).unwrap();

        // Parse the script
        match parse(&script) {
            Ok(s) => println!("Parsed script: {:#?}", s),
            Err(e) => panic!("Failed to parse script: {}", e)
        }
    }

    #[test]
    #[should_panic(expected = "Failed to parse script")]
    fn test_no_payload() {
        match parse("1.0: ,") {
            Ok(_) => (),
            Err(e) => panic!("Failed to parse script: {}", e)
        }
    }

}