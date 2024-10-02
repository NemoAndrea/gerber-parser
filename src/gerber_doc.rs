use gerber_types::{Unit, CoordinateFormat, Aperture, Command, ExtendedCode, ApertureDefinition};
use::std::collections::HashMap;
use std::fmt;
use std::iter::repeat;
use crate::error::GerberParserError;

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
    pub commands: Vec<Result<Command, GerberParserError>>,
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
    /// in the gerber-types rust crate. Note that aperture definitions will be sorted by code number
    /// with lower codes being at the top of the command. This is independent of their order during
    /// parsing.
    pub fn to_commands(self) -> Vec<Command> {
        let mut gerber_cmds: Vec<Command> = Vec::new();
        match self.format_specification{
            None => {}
            Some(format_spec) => {
                gerber_cmds.push(ExtendedCode::CoordinateFormat(format_spec).into());
            }
        }
        match self.units{
            None => {}
            Some(units) => {
                gerber_cmds.push(ExtendedCode::Unit(units).into());
            }
        }
        
        // we add the apertures to the list, but we sort by code. This means the order of the output
        // is reproducible every time. 
        let mut apertures = self.apertures.into_iter().collect::<Vec<_>>();
        apertures.sort_by_key(|tup| tup.0);
        for (code, aperture) in apertures {
            gerber_cmds.push(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code,
                aperture,
            }).into());
        }
        gerber_cmds.extend(self.commands.into_iter().filter_map(|command|{
            match command{
                Ok(real_com) => {
                    Some(real_com)
                }
                Err(_) => {
                    None
                }
            }
        }));
        
        // TODO implement for units        
        return gerber_cmds
    }
    
    pub fn get_errors(&self) -> Vec<&GerberParserError> {
        let mut error_vec: Vec<&GerberParserError> = Vec::new();
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
        let int_str: String = repeat("_").take(self.format_specification.unwrap().integer as usize).collect();
        let dec_str: String = repeat("_").take(self.format_specification.unwrap().decimal as usize).collect();
        writeln!(f, "GerberDoc")?;
        writeln!(f, "- units: {:?}", self.units)?;
        writeln!(f, "- format spec: {}.{} ({}|{})", int_str, dec_str, self.format_specification.unwrap().integer, self.format_specification.unwrap().decimal)?;
        writeln!(f, "- apertures: ")?;
        for (code, _) in &self.apertures {
            writeln!(f, "\t {}", code)?;
        }
        write!(f, "- commands: {}", &self.commands.len())
    }
}