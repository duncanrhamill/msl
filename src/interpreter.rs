//! # MSL interpreter module
//!
//! This module provides a runtime interpreter for MSL scripts.

// ---------------------------------------------------------------------------
// IMPORTS
// ---------------------------------------------------------------------------

use serde::Deserialize;
use semver::Version;
use std::path::Path;
use thiserror::Error;
use std::fs::OpenOptions;
use std::io::prelude::*;

use crate::parser::{parse, ParseError};

// ---------------------------------------------------------------------------
// DATA STRUCTURES
// ---------------------------------------------------------------------------

/// An instance of an MSL interpreter.
///
/// Before running a script it must be loaded using the 
/// [`Interpreter::from_str`] or [`Interpreter::from_path`]. The script will be
/// loaded and parsed, and any syntax errors will be reported through errors
/// in these functions.
///
/// ## Getting objects from the script
///
/// The interpreter is single threaded, and is interacted with using the 
/// [`Interpreter::get_pending`] function, which will return a vector of 
/// pending objects at the current time.
///
/// ## Events and flags
///
/// The interpreter supports events and flags. These can be interacted with
/// using the [`Interpreter::fire_event`], [`Interpreter::set_flag`], and
/// [`Interpreter::unset_flag`] functions.
///
/// Events allow the script to respond dynamically to events which can occur
/// during the execution of the script. Events have a unique string ID which
/// can be awaited using the `WaitForEvent(id)` directive (or it's timed 
/// equivalent). Once a fired event has been awaited it is unset.
///
/// Flags are similar to events however they are not unset when consumed, 
/// instead they must be unset by using the [`Interpreter::unset_flag`] 
/// explicitly.
pub struct Interpreter {
    /// The current time in the interpreter
    current_time: f64,

    /// Events which are currently fired
    events: Vec<String>,

    /// Flags which are currently set
    flags: Vec<String>,

    /// The script to be run
    script: Script,

    /// The order in which items were run.
    as_run_items: Vec<Item>
}

/// An MSL script
#[derive(Debug)]
pub(crate) struct Script {
    /// The metadata associated with the script
    pub(crate) metadata: Metadata,

    /// The root block of the script
    pub(crate) root: Block
}

/// Metadata about an MSL file.
///
/// All metadata comments are optional, and will default to:
///
/// - The current version of MSL (the version of this library)
/// - RSON for payload parsing
#[derive(Debug)]
pub(crate) struct Metadata {
    /// The version of MSL this script targets
    pub(crate) version: Version,

    /// The format of payloads within the script
    pub(crate) payload_format: PayloadFormat
}

/// Represents a block within a script.
///
/// Blocks are regions of the script in which timed items exist. The times on 
/// these items are relative to the start time of the block.
#[derive(Debug)]
pub(crate) struct Block {
    /// The time at which this block starts.
    pub(crate) start_time: f64,

    /// The index of the last item to be fired in this block.
    pub(crate) last_fired_index: Option<usize>,

    /// The items contained within this block.
    pub(crate) items: Vec<Item>
}

/// An item within the script.
///
/// Items contain a timestamp, which is the time, relative to the start of the
/// containing [`Block`], which the item will be triggered at.
///
/// Items may contain either objects (data to be emmited to the subscriber), or
/// directives (which themselves may contain).
///
/// In addition items may optionally contain a [`Block`], which will be started
/// when the item is started.
#[derive(Debug)]
pub(crate) struct Item {

    /// Indicates that the item has or hasn't been fired yet.
    pub(crate) fired: bool,

    /// Indicates that the item is relative to the previous one.
    pub(crate) is_relative: bool,

    /// The absolute timestamp of the item, relative to the start of the script.
    pub(crate) timestamp: Option<f64>,

    /// The trigger time, relative to the start of the block, or relative to
    /// the previous item if `is_relative` is `true`.
    pub(crate) trigger_time: f64,

    /// The data contained in the item
    pub(crate) data: ItemData,

    /// An optional block which may be started by this item.
    pub(crate) block: Option<Block>
}

// ---------------------------------------------------------------------------
// ENUMERATIONS
// ---------------------------------------------------------------------------

/// The format of the payload that an MSL file contains. 
///
/// This will determine which deserializer is used for converting payloads 
/// into objects.
#[derive(Deserialize, Debug, PartialEq)]
pub(crate) enum PayloadFormat {
    /// Use JSON for the payload
    #[serde(alias = "JSON")]
    Json,

    /// Use RSON for the payload
    #[serde(alias = "RSON")]
    Rson
}

/// Data which may be stored inside an item, either an [`Object`] (in the form
/// of a string for deserialisation later), or [`Directive`].
#[derive(Debug)]
pub(crate) enum ItemData {
    Object(String),
    Directive(Directive),
    DirectiveAndObject(Directive, String)
}

/// A list of possible directives.
///
/// Directives instruct the scripting language to perform some action rather 
/// than emmit an object. For instance the `WaitForEvent()` directive will only
/// allow relative execution of commands after a particular event fires (see 
/// [`events`]) for more information).
#[derive(Debug, PartialEq, Deserialize, Clone)]
pub enum Directive {
    /// Continue execution only after an event matching the wrapped string has
    /// fired. See [`events`] for more information.
    WaitForEvent(String),

    /// Similar to [`WaitForEvent`] but includes a timeout. If the event 
    /// doesn't fire within the specified number of time units then the
    /// `IfTimeout` directive will fire. Otherwise the `IfNotTimeout` directive
    /// will.
    WaitForEventTimeout(String, f64),

    /// Continue execution if the flag matching the wrapped string is set.
    IfSet(String),

    /// Continue execution if the flag matching the wrapped string is not set.
    IfNotSet(String),

    /// Continue execution if the previous `WaitForEventTimeout` directive 
    /// timedout.
    IfTimeout,

    /// Continue execution if the previous `WaitForEventTimeout` directive 
    /// did not timeout.
    IfNotTimeout,

    /// Stop the script.
    ///
    /// This may be used to stop the script once a particular event fires. 
    /// There is no need to include a `Stop` directive at the end of the script
    /// file as this is implied.
    Stop,
}

/// Errors which can arise from loading a script from a path
#[derive(Debug, Error)]
pub enum FromPathError {
    #[error("The specified path does not exist")]
    DoesNotExist,

    #[error("The specified path is not a file")]
    NotAFile,

    #[error("Cannot open the file: {0}")]
    CannotOpenFile(std::io::Error),

    #[error("Cannot read the file: {0}")]
    CannotReadFile(std::io::Error),

    #[error("The script file could not be parsed: {0}")]
    ParseError(ParseError)
}

/// An error that can occur in the interpreter during runtime.
#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Expected a time greater than the current time ({0}), but got {1}")]
    TimeInPast(f64, f64)
}

#[derive(Debug)]
pub enum ItemEvalResult {
    Object(String),
    Block(Block),
    
}

// ---------------------------------------------------------------------------
// IMPLEMENTATIONS  
// ---------------------------------------------------------------------------

impl Interpreter {

    /// Create a new interpreter instance from the input string.
    pub fn from_str(source: &str) -> Result<Self, ParseError> {
        // Parse the source into a script
        let script = parse(source)?;

        // Build the struct
        Ok(Self {
            current_time: 0.0,
            events: Vec::new(),
            flags: Vec::new(),
            script,
            as_run_items: Vec::new()
        })
    }

    /// Create a new interpreter instance from the file located at the input 
    /// path.
    pub fn from_path<P: AsRef<Path>>(path: P) -> Result<Self, FromPathError> {
        
        // If the path doesn't exist
        if !path.as_ref().exists() {
            return Err(FromPathError::DoesNotExist)
        }

        // If the path isn't a file
        if !path.as_ref().is_file() {
            return Err(FromPathError::NotAFile)
        }

        // Open the file
        let mut script_file = match OpenOptions::new()
            .read(true)
            .open(path)
        {
            Ok(f) => f,
            Err(e) => return Err(FromPathError::CannotOpenFile(e))
        };

        // Read the file
        let mut script_str = String::new();
        match script_file.read_to_string(&mut script_str) {
            Ok(_) => (),
            Err(e) => return Err(FromPathError::CannotReadFile(e))
        };

        // Parse the string
        match Interpreter::from_str(&script_str) {
            Ok(i) => Ok(i),
            Err(e) => Err(FromPathError::ParseError(e))
        }
    }

    /// Set the current time in the interpreter.
    ///
    /// This function updates the internal time of the interpreter. It should
    /// be called before calling [`get_pending`].
    ///
    /// Attempting to pass in a time which is before the current time will 
    /// result in a runtime error.
    pub fn set_current_time(&mut self, time: f64) -> Result<(), RuntimeError> {
        if time < self.current_time {
            Err(RuntimeError::TimeInPast(self.current_time, time))
        }
        else {
            self.current_time = time;
            Ok(())
        }
    }

    /// Returns `true` if the script is still running.
    pub fn is_running(&self) -> bool {
        self.script.root.is_running()
    }

    /// Retrieve a pending object from the interpreter.
    ///
    /// Returns `Ok(Some(T))` if an object is ready to be fired, or `Ok(None)`
    /// otherwise. The subscriber should call this function until `None` is
    /// returned.
    ///
    /// The returned object must be deserializable (`serde::Deserialize`).
    pub fn get_pending<'a, T: Deserialize<'a>>(
        &mut self
    ) -> Result<Option<T>, RuntimeError> 
    {
        // If the script is not running return `None`
        if !self.is_running() {
            return Ok(None)
        }

        // Get pending item from the root block
        self.script.root.get_pending(self.current_time)
    }
}

impl Block {

    /// Returns `true` if this block is running.
    ///
    /// A running block is one in which not all items have fired.
    fn is_running(&self) -> bool {
        if self.last_fired_index.is_none() {
            true
        }
        else {
            self.last_fired_index.unwrap() < self.items.len()
        }
    }
    
    /// Get the first pending item from this block.
    ///
    /// If there is a pending item it will be marked as fired and the 
    /// last_fired index updated.
    fn get_pending<'a, T: Deserialize<'a>>(
        &mut self, time: f64
    ) -> Result<Option<Item>, RuntimeError> 
    {
        // Find items in the block which are past the current time and have not
        // been fired. We start searching from the last index to be fired to 
        // save time.
        //
        // Note that since item's times are relative to the start of the block
        // we need to add the block's start time to their times.
        let mut start_idx = match self.last_fired_index {
            Some(i) => i,
            None => 0
        };

        for i in start_idx..self.items.len() {
            // Get the timestamp of the item at this index
            let timestamp = self.get_item_timestamp(i);

            // If the item is in the past (or at the current time), and the
            // item hasn't been fired.
            if timestamp <= time
                &&
                !self.items[i].fired 
            {
                // Set the item as fired and update the last item fired index
                self.items[i].fired = true;
                self.last_fired_index = Some(i);

                // Return the item 
                return Ok(Some(self.items[i]))
            }
            // If we've reached a timestamp that is beyond the current time
            // we return None
            else {
                return Ok(None)
            }
        }

        Ok(None)
    }

    /// Get the timestamp of the item at the given index.
    ///
    /// This function will return the absolute timestamp by either:
    /// - The already calculated `timestamp`, if this function has already been
    ///   called for this item.
    /// - Calculating, accounting for relative items, the absolute time. This
    ///   will then be saved so that the calculation does not need to be 
    ///   performed in the future.
    fn get_item_timestamp(&mut self, index: usize) -> f64 {

        // If the timestamp is already calculated return that
        if let Some(t) = self.items[index].timestamp {
            t
        }
        // Calculate the timestamp
        else {
            // Add the trigger time of this item to the block start time.
            let mut timestamp = 
                self.start_time + self.items[index].trigger_time;

            // If the item is relative get the time of the previous item
            // recursively, which will account for any previous items that are
            // also relative.
            if self.items[index].is_relative {
                // If the index of this item is zero, we can't get the 
                // timestamp of the previous item. A relative first item is
                // just equivalent to being a non-relative item, so we don't 
                // add anything else.
                if index > 0 {
                    timestamp += self.get_item_timestamp(index - 1);
                }
            }

            // Save the timestamp
            self.items[index].timestamp = Some(timestamp);

            // Return the timestamp
            timestamp
        }
    }
}

impl Item {
    fn evaluate(&mut self) ->
}