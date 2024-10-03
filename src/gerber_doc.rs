use gerber_types::{Unit, CoordinateFormat, Aperture, Command, ExtendedCode, ApertureDefinition};
use::std::collections::HashMap;
use std::fmt;
use std::iter::repeat;
use crate::error::GerberParserErrorWithContext;

#[derive(Debug)]
// Representation of Gerber document 
pub struct GerberDoc {
    // unit type, defined once per document
    pub units: Option<Unit>,
    // format specification for coordinates, defined once per document
    pub format_specification: Option<CoordinateFormat>,
    /// map of apertures which can be used in draw commands later on in the document. 
    pub apertures: HashMap::<i32, Aperture>,
    // Anything else, draw commands, comments, attributes
    pub commands: Vec<Result<Command, GerberParserErrorWithContext>>,
    /// Image Name, 8.1.3. Deprecated, but still used by fusion 360.
    pub image_name: Option<String>
}

impl GerberDoc {
    // instantiate a empty gerber document ready for parsing
    pub fn new() -> GerberDoc {
        GerberDoc {
            units: None,
            format_specification: None,
            apertures: HashMap::new(),
            commands: Vec::new(),
            image_name: None,
        }
    }

    /// Turns a GerberDoc into a &vec of gerber-types Commands
    /// 
    /// Get a representation of a gerber document *purely* in terms of elements provided
    /// in the gerber-types rust crate.
    /// 
    /// This will ignore any errors encountered during parsing, to access those use `get_errors`
    pub fn to_commands(self) -> Vec<Command> {
        self.commands.into_iter().filter_map(|element|{
            match element {
                Ok(com) => Some(com),
                Err(_) => None
            }
        }).collect()
    }
    
    /// Similar to `to_commands()`, but does not consume the document, and returns refs
    ///
    /// Get a representation of a gerber document *purely* in terms of elements provided
    /// in the gerber-types rust crate.
    ///
    /// This will ignore any errors encountered during parsing, to access those use `get_errors`
    pub fn as_commands(&self) -> Vec<&Command> {
        self.commands.iter().filter_map(|element|{
            match element {
                Ok(com) => Some(com),
                Err(_) => None
            }
        }).collect()
    }
    
    pub fn get_errors(&self) -> Vec<&GerberParserErrorWithContext> {
        let mut error_vec: Vec<&GerberParserErrorWithContext> = Vec::new();
        for command in &self.commands {
            if let Err(error) = command {
                error_vec.push(error);
            }
        }
        error_vec
    }
}

impl fmt::Display for GerberDoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let int_str: String = repeat("_")
            .take(self.format_specification.unwrap().integer as usize).collect();
        let dec_str: String = repeat("_")
            .take(self.format_specification.unwrap().decimal as usize).collect();
        writeln!(f, "GerberDoc")?;
        writeln!(f, "- units: {:?}", self.units)?;
        writeln!(f, "- format spec: {}.{} ({}|{})", 
                 int_str, 
                 dec_str, 
                 self.format_specification.unwrap().integer, 
                 self.format_specification.unwrap().decimal)?;
        writeln!(f, "- apertures: ")?;
        for (code, _) in &self.apertures {
            writeln!(f, "\t {}", code)?;
        }
        write!(f, "- commands: {}", &self.commands.len())
    }
}