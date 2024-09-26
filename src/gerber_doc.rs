use gerber_types::{Unit, CoordinateFormat, Aperture, Command, ExtendedCode, ApertureDefinition};
use::std::collections::HashMap;
use std::fmt;
use std::iter::repeat;


#[derive(Debug, PartialEq)]
// Representation of Gerber document 
pub struct GerberDoc {
    // unit type, defined once per document
    pub units: Option<Unit>,
    // format specification for coordinates, defined once per document
    pub format_specification: Option<CoordinateFormat>,
    /// map of apertures which can be used in draw commands later on in the document. 
    pub apertures: HashMap::<i32, Aperture>,
    // Anything else, draw commands, comments, attributes
    pub commands: Vec<Command>,
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
    pub fn to_commands(mut self) -> Vec<Command> {
        let mut gerber_cmds: Vec<Command> = Vec::new();
        gerber_cmds.push(ExtendedCode::CoordinateFormat(self.format_specification.unwrap()).into());
        gerber_cmds.push(ExtendedCode::Unit(self.units.unwrap()).into());

        // we add the apertures to the list, but we sort by code. This means the order of the output
        // is reproducible every time. 
        let mut apertures = self.apertures.into_iter().collect::<Vec<_>>();
        apertures.sort_by_key(|tup| tup.0);
        for (code, aperture) in apertures {
            gerber_cmds.push(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: code,
                aperture: aperture}).into())
        }        

        gerber_cmds.append(&mut self.commands);
        // TODO implement for units        
        return gerber_cmds
    }
}

impl fmt::Display for GerberDoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let int_str: String = repeat("_").take(self.format_specification.unwrap().integer as usize).collect();
        let dec_str: String = repeat("_").take(self.format_specification.unwrap().decimal as usize).collect();
        writeln!(f, "GerberDoc").unwrap();
        writeln!(f, "- units: {:?}", self.units).unwrap();
        writeln!(f, "- format spec: {}.{} ({}|{})", int_str, dec_str, self.format_specification.unwrap().integer, self.format_specification.unwrap().decimal).unwrap();
        writeln!(f, "- apertures: ").unwrap();
        for (code, _) in &self.apertures {
            writeln!(f, "\t {}", code).unwrap();
        }
        write!(f, "- commands: {}", &self.commands.len())
    }
}