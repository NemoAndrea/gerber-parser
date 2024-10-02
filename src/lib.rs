//! # Gerber-parser
//! 
//! This is a simple parser in rust for Gerber file (or string). The underlying representation
//! is built on the `gerber-types` crate. 
//! 
//! Gerber files are the de-facto file format for PCB manufacturing, but are also used in other 
//! context, such as in microfabrication. It is an old format with a lot of baggage, but the 
//! [specification is well documented](https://www.ucamco.com/en/guest/downloads/gerber-format),
//! there is an [online free viewer available](https://gerber-viewer.ucamco.com/) to check your designs,
//! and the format is plaintext making it easy to work with. 
//! 
//! As the crate is still in the early version, expect significant changes over time, as both this
//! crate and `gerber-types` will need to undergo changes.

pub mod parser;
pub mod gerber_doc;
pub mod error;
