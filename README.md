# Gerber-parser

A simple `gerber` parser written in rust to be used with the `gerber-types` crate. 

```rust
use std::fs::File;
use std::io::{BufRead, BufReader};
use gerber_parser::parser::parse_gerber;

// open a .gbr file from system
let file = File::open(path).unwrap();
let reader = BufReader::new(file);

// Now we parse the file to a GerberDoc 
let gerber_doc = parse_gerber(reader);

// And, if you want to have the representation purely in terms of Vec<Command> of the gerber-types crate then you can run
let commands = gerber_doc.to_commands();
```

### Current State

⚠️ Note: this package is still in development and does not cover the full Gerber spec

Currently missing

* All `TF` commands
* All `AM` commands
* `LM`, `LR`, `LS` commands

In addition, comments in the header section of the file (i.e. unit type declaration, format specification and aperture declarations) will be placed below the header when the parsed Gerber is converted back to string via serialisation. 

### General to-do

* Make parsing a bit cleaner, with `Result`
* Reduce the number of panics 
* Make error messages clearer
* Mini-tests for all commands