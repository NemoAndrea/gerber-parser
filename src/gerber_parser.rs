use std::{io::{Read, BufReader, BufRead}, convert::TryInto, os::raw, process::CommandArgs};
use gerber_types::{Command, ExtendedCode, Unit, FunctionCode, GCode, CoordinateFormat,
     ApertureDefinition, Aperture, Circle, Rectangular, Polygon, MCode, DCode, Polarity};
use regex::Regex;
use std::str::Chars;
use crate::gerber_document::{ GerberDoc, empty_gerber};

fn parse_gerber<T: Read>(reader: BufReader<T>) -> GerberDoc {
    let mut gerber_cmds:Vec<Command> = Vec::new();
    let mut gerber_doc = empty_gerber();

    // naively define some regex queries
    let re_units = Regex::new(r"%MO(.*)\*%").unwrap();
    let re_comment = Regex::new(r"G04 (.*)\*").unwrap();
    let re_formatspec = Regex::new(r"%FSLAX(.*)Y(.*)\*%").unwrap();
    let re_aperture = Regex::new(r"%ADD([0-9]+)([A-Z]),(.*)\*%").unwrap();

    for (index, line) in reader.lines().enumerate() {
        let rawline = line.unwrap(); 
        // TODO combine this with line above
        let line = rawline.trim();

        // Show the line 
        //println!("{}. {}", index + 1, &line);
        if !line.is_empty() {
            let mut linechars = line.chars();

            match linechars.next().unwrap() {
                'G' => {
                    match linechars.next().unwrap() {
                        '0' =>  match linechars.next().unwrap() {
                            '1' => {}, // G01
                            '2' => {}, // G02
                            '3' => {}, // G03
                            '4' => {parse_comment(line, &re_comment, &mut gerber_doc) }, // G04
                            _ => line_parse_failure(line, index),
                        },
                        '3'=> match linechars.next().unwrap() {
                            '6' => {}, // G35
                            '7' => {}, // G36
                            _ => line_parse_failure(line, index),
                        },
                        '7' => {}, // G75
                        _ => line_parse_failure(line, index),             }
                },
                '%' => {
                    match linechars.next().unwrap() {
                        'M' => { parse_units(line, &re_units, &mut gerber_doc) },
                        'F' => { parse_format_spec(line, &re_formatspec, &mut gerber_doc) },
                        'A' => match linechars.next().unwrap() {
                            'D' => { parse_aperture_defs(line, &re_aperture, &mut gerber_doc) }, // AD
                            'M' => { panic!("Aperture Macros (AM) are not supported yet.") }, // AM 
                            _ => line_parse_failure(line, index)
                        }, 
                        _ => line_parse_failure(line, index)
                    }
                },
                'X' => {linechars.next_back(); match linechars.next_back().unwrap() { 
                    '1' => {}, // D01
                    '2' => {}, // D02
                    '3' => {}, // D03
                    _ => line_parse_failure(line, index)
                }},
                'D' => { // select aperture D<num>*                   
                    linechars.next_back(); // remove the trailing '*'
                    gerber_cmds.push(FunctionCode::DCode(DCode::SelectAperture(
                    linechars.as_str().parse::<i32>().expect("Failed to parse aperture selection"))).into())}, 
                'L' => match linechars.next().unwrap() {
                    'P' => match linechars.next().unwrap() {
                        'D' => gerber_cmds.push(ExtendedCode::LoadPolarity(Polarity::Dark).into()), // LPD
                        'C' => gerber_cmds.push(ExtendedCode::LoadPolarity(Polarity::Clear).into()), // LPC
                        _ => line_parse_failure(line, index)
                    }, // LP
                    'M' => parse_load_mirroring(linechars, &mut gerber_cmds ), // LM 
                    'R' => { panic!("Load Mirroring (LM) command not supported yet.") }, // LR
                    'S' => { panic!("Load Scaling (LS) command not supported yet.") }, // LS
                    _ => line_parse_failure(line, index)
                },
                'M' => { gerber_cmds.push(FunctionCode::MCode(MCode::EndOfFile).into())}
                'T' => match linechars.next().unwrap() {
                    'F' => { panic!("Attribute commands not supported yet.") },
                    'A' => { panic!("Attribute commands not supported yet.") },
                    'O' => { panic!("Attribute commands not supported yet.") },
                    'D' => { panic!("Attribute commands not supported yet.") },
                    _ => line_parse_failure(line, index)
                }
                _ => line_parse_failure(line, index)
            }           
        }
    }

    // check that we ended with a gerber EOF command
    assert_eq!(gerber_cmds.last().unwrap(), &Command::FunctionCode(FunctionCode::MCode(MCode::EndOfFile)),
        "Missing M02 statement at end of file");

    return gerber_doc
}

fn line_parse_failure(line: &str, index: usize) {
    panic!("Cannot parse line:\n{} | {}", index, line)
} 

fn parse_comment(line: &str, re: &Regex, gerber_doc: &mut GerberDoc) {
    if let Some(regmatch) = re.captures(line) {
        let comment = regmatch.get(1).unwrap().as_str();
        gerber_doc.commands.push(FunctionCode::GCode(GCode::Comment(comment.to_string())).into());
    } 
}

fn parse_units(line: &str, re: &Regex, gerber_doc: &mut GerberDoc) {
    // Check that the units are not set yet (that would imply the set unit command is present twice)
    if let Some(Unit) = gerber_doc.units { panic!{"Cannot set unit type twice in the same document!"} }
    // Set the unit type
    if let Some(regmatch) = re.captures(line) {
        let units_str = regmatch.get(1).unwrap().as_str();
        if units_str == "MM" {
            gerber_doc.units = Some(Unit::Millimeters);
        } else if units_str == "IN" {
            gerber_doc.units = Some(Unit::Inches);
        } else { panic!("Incorrect gerber units format")}
    }
}

fn parse_format_spec(line: &str, re: &Regex, gerber_doc: &mut GerberDoc) {
    // Ensure that FS was not set before, which would imply two FS statements in the same doc
    if let Some(FormatSpec) = gerber_doc.format_specification { panic!("Cannot set format specification twice in the same document!") }
    // Set Format Specification
    if let Some(regmatch) = re.captures(line) {
        let mut fs_chars = regmatch.get(1).unwrap().as_str().chars();
        let integer:u8 = fs_chars.next().unwrap().to_digit(10).unwrap() as u8;
        let decimal:u8 = fs_chars.next().unwrap().to_digit(10).unwrap() as u8;

        // the gerber spec states that the integer value can be at most 6
        assert!(integer >= 1 && integer <= 6, "format spec integer value must be between 1 and 6");

        let fs = CoordinateFormat::new(integer, decimal);                  
        gerber_doc.format_specification = Some(fs);
    } 
}

fn parse_aperture_defs(line: &str, re: &Regex, gerber_doc: &mut GerberDoc) {
    // aperture definitions
    // TODO: prevent the same aperture code being used twice
    if let Some(regmatch) = re.captures(line) {
        let code = regmatch.get(1).unwrap().as_str().parse::<i32>().expect("Failed to parse aperture code");
        assert!(code > 9, "Aperture codes 0-9 cannot be used for custom apertures");
        
        let aperture_type = regmatch.get(2).unwrap().as_str();
        let aperture_args:  Vec<&str> = regmatch.get(3).unwrap().as_str().split("X").collect();

        //println!("The code is {}, and the aperture type is {} with params {:?}", code, aperture_type, aperture_args);
        let insert_state = match aperture_type {
            "C" => gerber_doc.apertures.insert(code, Aperture::Circle(Circle {
                diameter: aperture_args[0].trim().parse::<f64>().unwrap(),
                hole_diameter: if aperture_args.len() > 1 {
                    Some(aperture_args[1].trim().parse::<f64>().unwrap())} else {None}
                })),
            "R" => gerber_doc.apertures.insert(code, Aperture::Rectangle(Rectangular {
                    x: aperture_args[0].trim().parse::<f64>().unwrap(),
                    y: aperture_args[1].trim().parse::<f64>().unwrap(),
                    hole_diameter: if aperture_args.len() > 2 {
                        Some(aperture_args[2].trim().parse::<f64>().unwrap())} else {None}
                })),
            "O" => gerber_doc.apertures.insert(code, Aperture::Obround(Rectangular {
                    x: aperture_args[0].trim().parse::<f64>().unwrap(),
                    y: aperture_args[1].trim().parse::<f64>().unwrap(),
                    hole_diameter: if aperture_args.len() > 2 {
                        Some(aperture_args[2].trim().parse::<f64>().unwrap())} else {None}
                })),
            // note that for polygon we HAVE TO specify rotation if we want to add a hole
            "P" => gerber_doc.apertures.insert(code, Aperture::Polygon(Polygon {
                    diameter: aperture_args[0].trim().parse::<f64>().unwrap(),
                    vertices: aperture_args[1].trim().parse::<u8>().unwrap(),
                    rotation: if aperture_args.len() > 2 {
                        Some(aperture_args[2].trim().parse::<f64>().unwrap())} else {None},
                    hole_diameter: if aperture_args.len() > 3 {
                        Some(aperture_args[3].trim().parse::<f64>().unwrap())} else {None}
                })),                  
            _ => { panic!("Encountered unknown aperture definition statement") }                   
        };

        // the insert state will be None if the key (i.e. aperture code) was not present yet,
        // or a Some(Aperture) value if the key was already in use (see behaviour of HashMap.insert)
        // If a key is already present we have to throw an error, as this is invalid 
        if let Some(Aperture) = insert_state { panic!("Cannot use the aperture code {} more than once!", code)}
    }
}

fn parse_load_mirroring(mut linechars: Chars, commands: &mut Vec<Command> ) {
    // match linechars.next().unwrap() {
    //     'N' => commands.push(value), //LMN
    //     'Y' => commands.push(value), // LMY
    //     'X' => match linechars.next() { 
    //         Some('Y') => {} //LMXY
    //         None => {} // LMX
    //         _ => panic!("Invalid load mirroring (LM) command: {}", linechars.as_str())
    //     }
    //     _ => panic!("Invalid load mirroring (LM) command: {}", linechars.as_str())
    // }
    panic!("Load Mirroring (LM) command not supported yet.")
}



#[cfg(test)]
mod tests {
    use std::io::BufReader;
    use stringreader::StringReader;
    use gerber_types::{GCode, FunctionCode, Command, ExtendedCode, Unit, CoordinateFormat,
        ApertureDefinition, Aperture, Circle, Rectangular, Polygon};
    use::std::collections::HashMap;

    use super::parse_gerber;

    fn gerber_to_reader(gerber_string: &str) -> BufReader<StringReader> {
        BufReader::new(StringReader::new(gerber_string))
    }

    static SAMPLE_GERBER_1: &str ="    
    %FSLAX23Y23*%
    %MOMM*%

    G04 Define the primive apertures*
    %ADD801C, 0.005*%
    %ADD802C, 0.01*%
    %ADD803R, 0.01X0.15*%

    G04 Chip outline*
    D801*

    G75*
    G01*
    X4000Y1500D02*
    X4000Y18D01*
    X4000Y-18D02*
    X4000Y-1500D01*
    X200Y-1500D01*
    X0Y-750D01*
    X-200Y-1500D01*
    X-4000Y-1500D01*
    X-4000Y-18D01*
    X-4000Y18D02*
    X-4000Y1500D01*
    X-200Y1500D01*
    X0Y750D01*
    X200Y1500D01*
    X4000Y1500D01*

    D802*
    X0Y0D03*
    X0Y300D03*
    X0Y-300D03*
    X0Y600D03*
    X0Y-600D03*

    D803*
    X0Y150D03*
    X0Y450D03*
    X0Y-150D03*
    X0Y-450D03*
    X0Y-450D03*
    X0Y-720D03*
    X0Y720D03*
    M02*
    ";

    #[test]
    fn test_full_gerber() {
        let gerber_reader = gerber_to_reader(&SAMPLE_GERBER_1);

        assert_eq!(parse_gerber(gerber_reader).to_atomic(), &Vec::<Command>::new());
    }

    #[test]
    fn format_specification() {
        let reader_fs_1 = gerber_to_reader("
        %FSLAX15Y15*%
        %MOMM*%
        M02*        
        ");

        let reader_fs_2 = gerber_to_reader("
        %FSLAX36Y36*%
        %MOIN*%
        G04 Actual apertures and draw commands go here*
        M02*        
        ");

        let sample_gerber_1 = gerber_to_reader(&SAMPLE_GERBER_1);

        assert_eq!(parse_gerber(reader_fs_1).format_specification, Some(CoordinateFormat::new(1, 5)));

        assert_eq!(parse_gerber(reader_fs_2).format_specification, Some(CoordinateFormat::new(3, 6)));

        assert_eq!(parse_gerber(sample_gerber_1).format_specification, Some(CoordinateFormat::new(2, 3)));
    }

    #[test]
    fn units() {
        let reader_mm = gerber_to_reader("
        G04 The next line specifies the precision of the units*
        %FSLAX23Y23*%
        G04 The next line specifies the units (inches or mm)*
        %MOMM*%

        G04 Actual apertures and draw commands go here*
        M02*        
        ");

        let reader_in = gerber_to_reader("
        G04 The next line specifies the precision of the units*
        %FSLAX23Y23*%
        G04 The next line specifies the units (inches or mm)*
        %MOIN*%

        G04 Actual apertures and draw commands go here*
        M02*        
        ");

        assert_eq!(parse_gerber(reader_mm).units , Some(Unit::Millimeters));
        assert_eq!(parse_gerber(reader_in).units , Some(Unit::Inches));
    }

    #[test]
    fn comments() {
        let reader = gerber_to_reader("
        G04 Comment before typical configuration lines*
        %FSLAX23Y23*%
        %MOMM*%

        G04 And now a comment after them*
        M02*        
        ");

        let filter_commands = |cmds:Vec<Command>| -> Vec<Command> {
            cmds.into_iter().filter(|cmd| match cmd {
                 Command::FunctionCode(FunctionCode::GCode(GCode::Comment(_))) => true, _ => false}).collect()};

        assert_eq!(filter_commands(parse_gerber(reader).commands), vec![
            Command::FunctionCode(FunctionCode::GCode(GCode::Comment("Comment before typical configuration lines".to_string()))),
            Command::FunctionCode(FunctionCode::GCode(GCode::Comment("And now a comment after them".to_string())))])
    }

    #[test]
    fn aperture_definitions() {
        let reader = gerber_to_reader("
        %FSLAX26Y26*%
        %MOMM*%

        G04 Aperture Definitions*
        %ADD999C, 0.01*%
        %ADD22R, 0.01X0.15*%
        %ADD23O, 0.01X0.15*%
        %ADD21P, 0.7X10*%
        %ADD24P, 0.7X10X16.5*%

        G04 Apertures with holes*
        %ADD123C, 0.01X0.003*%
        %ADD124R, 0.1X0.15X0.00001*%
        %ADD125O, 0.1X0.15X0.019*%
        %ADD126P, 1X7X5.5X0.7*%

        M02*        
        ");

        assert_eq!(parse_gerber(reader).apertures,  HashMap::from([
            (999, Aperture::Circle(Circle {diameter: 0.01, hole_diameter: None})),
            (22, Aperture::Rectangle(Rectangular{x: 0.01,y: 0.15,hole_diameter: None})),
            (23, Aperture::Obround(Rectangular{x: 0.01,y: 0.15,hole_diameter: None})),
            (21, Aperture::Polygon(Polygon{diameter: 0.7,vertices: 10,rotation: None, hole_diameter: None})),
            (24, Aperture::Polygon(Polygon{diameter: 0.7,vertices: 10,rotation: Some(16.5),hole_diameter: None})),
            (123, Aperture::Circle(Circle {diameter: 0.01,hole_diameter: Some(0.003)})),
            (124, Aperture::Rectangle(Rectangular {x: 0.1,y: 0.15,hole_diameter: Some(0.00001)})),
            (125, Aperture::Obround(Rectangular {x: 0.1,y: 0.15,hole_diameter: Some(0.019)})),
            (126, Aperture::Polygon(Polygon{diameter: 1.0,vertices: 7,rotation: Some(5.5),hole_diameter: Some(0.7)})),
            ]))
    }

    #[test]
    #[should_panic]
    fn conflicting_aperture_codes() {
        let reader = gerber_to_reader("
        %FSLAX23Y23*%
        %MOMM*%        
        %ADD24P, 0.7X10X16.5*%
        %ADD39P, 0.7X10X16.5*%
        G04 We cannot use the same code (24) again in the same document*
        %ADD24P, 1X10X20.0*%

        M02*      
        ");
        parse_gerber(reader);
    }

    #[test]
    #[should_panic]
    fn missing_EOF() {
        let reader = gerber_to_reader("
        %FSLAX23Y23*%
        %MOMM*%-

        G04 We should have a MO2 at the end, but what if we forget it?*      
        ");
        parse_gerber(reader);
    }

    #[test]
    #[should_panic]
    fn multiple_unit_statements() {
        let reader = gerber_to_reader("
        %FSLAX23Y23*%
        %MOMM*%
        G04 We can only declare the unit type once in a document* 
        %MOIN*%
            
        M02*  
        ");
        parse_gerber(reader);
    }

    #[test]
    #[should_panic]
    fn multiple_FS_statements() {
        let reader = gerber_to_reader("
        %FSLAX23Y23*%
        G04 We can only declare the format specification once in a document* 
        %FSLAX46Y46*%
        %MOMM*%
            
        M02*  
        ");
        parse_gerber(reader);
    }
}
