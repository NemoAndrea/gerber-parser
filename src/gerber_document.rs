use gerber_types::{Unit, CoordinateFormat, Aperture, Command};
use::std::collections::HashMap;

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
    pub commands: Vec<Command>    
}

// instantiate a empty gerber document ready for parsing, for convenience
pub fn empty_gerber() -> GerberDoc {
    GerberDoc {
        units: None,
        format_specification: None,
        apertures: HashMap::new(),
        commands: Vec::new()}
}


impl GerberDoc {
    /// Turns a GerberDoc into a &vec of gerber-types Commands
    /// 
    /// Get a representation of a gerber document purely in terms of elements provided
    /// in the gerber-types rust crate. 
    pub fn to_atomic(&self) -> &Vec<Command> {
        // TODO implement for units        
        return &self.commands
    }
}