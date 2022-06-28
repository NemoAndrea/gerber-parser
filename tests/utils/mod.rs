use std::io::BufReader;
use std::str;
use stringreader::StringReader;
use gerber_types::GerberCode;
use gerber_parser::gerber_doc::GerberDoc;

pub fn gerber_to_reader(gerber_string: &str) -> BufReader<StringReader> {
    BufReader::new(StringReader::new(gerber_string))
}


pub fn gerber_doc_to_str(gerber_doc: GerberDoc) -> String {
    let mut filevec = Vec::<u8>::new();
    // we use the serialisation methods of the gerber-types crate
    gerber_doc.to_commands().serialize(&mut filevec).unwrap();
    str::from_utf8(&filevec).unwrap().to_string()
}