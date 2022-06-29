use std::{io::{Read, BufReader, BufRead}, fs::File};
use gerber_types::{Command, ExtendedCode, Unit, FunctionCode, GCode, CoordinateFormat,
     Aperture, Circle, Rectangular, Polygon, MCode, DCode, Polarity,
      InterpolationMode, QuadrantMode, Operation, Coordinates, CoordinateNumber, CoordinateOffset,
       ApertureAttribute, ApertureFunction, FiducialScope, SmdPadType, FileAttribute, FilePolarity,
        Part, FileFunction, ObjectAttribute};
use regex::Regex;
use std::str::Chars;
use crate::gerber_doc::{ GerberDoc};

/// Parse a gerber string (in BufReader) to a GerberDoc
/// 
/// Take the contents of a Gerber (.gbr) file and parse it to a GerberDoc struct. The parsing does
/// some semantic checking, but is is certainly not exhaustive - so don't rely on it to check if
/// your Gerber file is valid according to the spec. Some of the parsing steps are greedy - they may
/// match something unexpected (rather than panicking) if there is a typo/fault in your file.
pub fn parse_gerber<T: Read>(reader: BufReader<T>) -> GerberDoc {
    let mut gerber_doc = GerberDoc::new();
    // The gerber spec allows omission of X or Y statements in D01/2/3 commands, where the omitted
    // coordinate is to be taken as whatever was used in the previous coordinate-using command
    // By default the 'last coordinate' can be taken to be (0,0)
    let mut last_coords = (0i64,0i64);

    // naively define some regex terms
    // TODO see which ones can be done without regex for better performance?
    let re_units = Regex::new(r"%MO(.*)\*%").unwrap();
    let re_comment = Regex::new(r"G04 (.*)\*").unwrap();
    let re_formatspec = Regex::new(r"%FSLAX(.*)Y(.*)\*%").unwrap();
    let re_aperture = Regex::new(r"%ADD([0-9]+)([A-Z]),(.*)\*%").unwrap();
    let re_interpolation = Regex::new(r"X?(-?[0-9]+)?Y?(-?[0-9]+)?I?(-?[0-9]+)?J?(-?[0-9]+)?D01\*").unwrap();
    let re_move_or_flash = Regex::new(r"X?(-?[0-9]+)?Y?(-?[0-9]+)?D0[2-3]*").unwrap();
    // TODO: handle escaped characters for attributes
    let re_attributes = Regex::new(r"%T[A-Z].([A-Z]+?),?").unwrap();  

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
                            '1' => gerber_doc.commands.push(FunctionCode::GCode(
                                GCode::InterpolationMode(InterpolationMode::Linear)).into()), // G01
                            '2' => gerber_doc.commands.push(FunctionCode::GCode(
                                GCode::InterpolationMode(InterpolationMode::ClockwiseCircular)).into()), // G02
                            '3' => gerber_doc.commands.push(FunctionCode::GCode(
                                GCode::InterpolationMode(InterpolationMode::CounterclockwiseCircular)).into()), // G03
                            '4' => {parse_comment(line, &re_comment, &mut gerber_doc) }, // G04
                            _ => line_parse_failure(line, index),
                        },
                        '3'=> match linechars.next().unwrap() {
                            '6' => gerber_doc.commands.push(FunctionCode::GCode(GCode::RegionMode(true)).into()), // G36
                            '7' => gerber_doc.commands.push(FunctionCode::GCode(GCode::RegionMode(false)).into()), // G37
                            _ => line_parse_failure(line, index),
                        },
                        '7' => match linechars.next().unwrap() {
                            // the G74 command is technically part of the Deprecated commands
                            '4' => gerber_doc.commands.push(FunctionCode::GCode(GCode::QuadrantMode(QuadrantMode::Single)).into()), // G74
                            '5' => gerber_doc.commands.push(FunctionCode::GCode(GCode::QuadrantMode(QuadrantMode::Multi)).into()), // G74
                            _ => line_parse_failure(line, index),
                        }, // G75
                        _ => line_parse_failure(line, index),             
                    }
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
                        'L' => match linechars.next().unwrap() {
                            'P' => match linechars.next().unwrap() {
                                'D' => gerber_doc.commands.push(ExtendedCode::LoadPolarity(Polarity::Dark).into()), // LPD
                                'C' => gerber_doc.commands.push(ExtendedCode::LoadPolarity(Polarity::Clear).into()), // LPC
                                _ => line_parse_failure(line, index)
                            }, // LP
                            'M' => parse_load_mirroring(linechars, &mut gerber_doc), // LM 
                            'R' => { panic!("Load Mirroring (LM) command not supported yet.") }, // LR
                            'S' => { panic!("Load Scaling (LS) command not supported yet.") }, // LS
                            _ => line_parse_failure(line, index)
                        },
                        'T' => match linechars.next().unwrap() {
                            'F' => { parse_file_attribute(linechars, &re_attributes,  &mut gerber_doc) },
                            'A' => { parse_aperture_attribute(linechars, &re_attributes, &mut gerber_doc) },
                            'O' => { parse_object_attribute(linechars, &re_attributes, &mut gerber_doc) },
                            'D' => { parse_delete_attribute(linechars, &re_attributes, &mut gerber_doc) },
                            _ => line_parse_failure(line, index)
                        }
                        _ => line_parse_failure(line, index)
                    }
                },
                'X' | 'Y' => {linechars.next_back(); match linechars.next_back().unwrap() { 
                    '1' => parse_interpolation(line, &re_interpolation,&mut gerber_doc, &mut last_coords), // D01
                    '2' => parse_move_or_flash(line, &re_move_or_flash,&mut gerber_doc, &mut last_coords, false), // D02
                    '3' => parse_move_or_flash(line, &re_move_or_flash,&mut gerber_doc, &mut last_coords, true), // D03
                    _ => line_parse_failure(line, index)
                }},
                'D' => { // select aperture D<num>*                   
                    linechars.next_back(); // remove the trailing '*'
                    parse_aperture_selection(linechars, &mut gerber_doc)
                },                
                'M' => { gerber_doc.commands.push(FunctionCode::MCode(MCode::EndOfFile).into())}                
                _ => line_parse_failure(line, index)
            }           
        }
    }

    // check that we ended with a gerber EOF command
    assert_eq!(gerber_doc.commands.last().unwrap(), &Command::FunctionCode(FunctionCode::MCode(MCode::EndOfFile)),
        "Missing M02 statement at end of file");

    return gerber_doc
}


// print a simple message in case the parser hits a dead end
fn line_parse_failure(line: &str, index: usize) {
    panic!("Cannot parse line:\n{} | {}", index, line)
} 


// parse a Gerber Comment (e.g. 'G04 This is a comment*')
fn parse_comment(line: &str, re: &Regex, gerber_doc: &mut GerberDoc) {
    if let Some(regmatch) = re.captures(line) {
        let comment = regmatch.get(1).unwrap().as_str();
        gerber_doc.commands.push(FunctionCode::GCode(GCode::Comment(comment.to_string())).into());
    } 
}


// parse a Gerber unit statement (e.g. '%MOMM*%')
fn parse_units(line: &str, re: &Regex, gerber_doc: &mut GerberDoc) {
    // Check that the units are not set yet (that would imply the set unit command is present twice)
    if gerber_doc.units != None { panic!{"Cannot set unit type twice in the same document!"} }
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


// parse a Gerber format spec statement (e.g. '%FSLAX23Y23*%')
fn parse_format_spec(line: &str, re: &Regex, gerber_doc: &mut GerberDoc) {
    // Ensure that FS was not set before, which would imply two FS statements in the same doc
    if gerber_doc.format_specification != None { panic!("Cannot set format specification twice in the same document!") }
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


// parse a Gerber aperture definition e.g. '%ADD44R, 2.0X3.0*%')
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
        if insert_state != None { panic!("Cannot use the aperture code {} more than once!", code)}
    }
}


fn parse_aperture_selection(linechars: Chars, gerber_doc: &mut GerberDoc) {
    let aperture_code = linechars.as_str().parse::<i32>().expect("Failed to parse aperture selection");
    assert!(gerber_doc.apertures.contains_key(&aperture_code), "Cannot select an aperture that is not defined");
    gerber_doc.commands.push(FunctionCode::DCode(DCode::SelectAperture(
        aperture_code)).into());    
}


// TODO clean up the except statements a bit
// parse a Gerber interpolation command (e.g. 'X2000Y40000I300J50000D01*')
fn parse_interpolation(line: &str, re: &Regex, gerber_doc: &mut GerberDoc, last_coords: &mut (i64, i64)) {
    if let Some(regmatch) = re.captures(line) {
        let x_coord = match regmatch.get(1) {
            Some(x) => { let new_x = x.as_str().trim().parse::<i64>().unwrap();
                last_coords.0 = new_x;
                new_x
            }
            None => last_coords.0, // if match is None, then the coordinate must have been implicit
        };
        let y_coord = match regmatch.get(2) {
            Some(y) => { let new_y = y.as_str().trim().parse::<i64>().unwrap();
                last_coords.1 = new_y;
                new_y
            }
            None => last_coords.1, // if match is None, then the coordinate must have been implicit
        };

        if let Some(_) = regmatch.get(3){  //  we have X,Y,I,J parameters and we are doing circular interpolation
            let i_offset = regmatch.get(3).expect("Unable to match I offset").as_str().trim().parse::<i64>().unwrap();
            let j_offset = regmatch.get(4).expect("Unable to match J offset").as_str().trim().parse::<i64>().unwrap();

            gerber_doc.commands.push(FunctionCode::DCode(DCode::Operation(
                Operation::Interpolate(coordinates_from_gerber(x_coord, y_coord,
                     gerber_doc.format_specification.expect("Operation statement called before format specification")),
                     Some(coordinates_offset_from_gerber(i_offset, j_offset, gerber_doc.format_specification.unwrap())))))
                .into());
        } else { // linear interpolation, only X,Y parameters
            gerber_doc.commands.push(FunctionCode::DCode(DCode::Operation(
                Operation::Interpolate(coordinates_from_gerber(x_coord, y_coord,
                     gerber_doc.format_specification.expect("Operation statement called before format specification")),
                     None)))
                .into());
        }            
    } else { panic!("Unable to parse D01 (interpolate) command: {}", line)}    
}


// TODO clean up the except statements a bit
// parse a Gerber move or flash command (e.g. 'X2000Y40000D02*')
fn parse_move_or_flash(line: &str, re: &Regex, gerber_doc: &mut GerberDoc, last_coords: &mut (i64, i64), flash: bool) {
    if let Some(regmatch) = re.captures(line) {
        let x_coord = match regmatch.get(1) {
            Some(x) => { let new_x = x.as_str().trim().parse::<i64>().unwrap();
                last_coords.0 = new_x;
                new_x
            }
            None => last_coords.0, // if match is None, then the coordinate must have been implicit
        };
        let y_coord = match regmatch.get(2) {
            Some(y) => { let new_y = y.as_str().trim().parse::<i64>().unwrap();
                last_coords.1 = new_y;
                new_y
            }
            None => last_coords.1, // if match is None, then the coordinate must have been implicit
        };

        if flash {
            gerber_doc.commands.push(FunctionCode::DCode(DCode::Operation(
                Operation::Flash(coordinates_from_gerber(x_coord, y_coord,
                     gerber_doc.format_specification.expect("Operation statement called before format specification"),
            )))).into());
        } else {
            gerber_doc.commands.push(FunctionCode::DCode(DCode::Operation(
                Operation::Move(coordinates_from_gerber(x_coord, y_coord,
                     gerber_doc.format_specification.expect("Operation statement called before format specification"),
            )))).into());
        }
    } else { panic!("Unable to parse D02 (move) or D03 (flash) command")}    
}


fn parse_load_mirroring(mut linechars: Chars, gerber_doc: &mut GerberDoc) {
    // match linechars.next().unwrap() {
    //     'N' => gerber_doc.commands.push(value), //LMN
    //     'Y' => gerber_doc.commands.push(value), // LMY
    //     'X' => match linechars.next() { 
    //         Some('Y') => {} //LMXY
    //         None => {} // LMX
    //         _ => panic!("Invalid load mirroring (LM) command: {}", linechars.as_str())
    //     }
    //     _ => panic!("Invalid load mirroring (LM) command: {}", linechars.as_str())
    // }
    panic!("Load Mirroring (LM) command not supported yet.")
}


/// Parse an Aperture Attribute (%TF.<AttributeName>[,<AttributeValue>]*%) into Command
/// 
/// For now we consider two types of TA statements:
/// 1. Aperture Function (AperFunction) with field: String
/// 2. Drill tolerance (DrillTolerance) with fields: [1] num [2] num 
/// 
/// ⚠️ Any other Attributes (which seem to be valid within the gerber spec) we will **fail** to parse!
/// 
/// ⚠️ This parsing statement needs a lot of tests and validation at the current stage!
fn parse_file_attribute(line: Chars, re: &Regex, gerber_doc: &mut GerberDoc) {
    let attr_args = get_attr_args(line);
    if attr_args.len() >= 2 {  // we must have at least 1 field
        //println!("TF args are: {:?}", attr_args);
        let file_attr: FileAttribute = match attr_args[0] {
            "Part" => match attr_args[1]{
                "Single" => FileAttribute::Part(Part::Single),
                "Array" => FileAttribute::Part(Part::Array),
                "FabricationPanel" => FileAttribute::Part(Part::FabricationPanel),
                "Coupon" => FileAttribute::Part(Part::Coupon),
                "Other" => FileAttribute::Part(Part::Other(attr_args[2].to_string())),
                _ => panic!("Unsupported Part type '{}' in TF statement", attr_args[1])
            },
            // TODO do FileFunction properly, but needs changes in gerber-types
            "FileFunction" => FileAttribute::FileFunction(FileFunction::Other(attr_args[1].to_string())),  
            "FilePolarity" => match attr_args[1]{
                "Positive" => FileAttribute::FilePolarity(FilePolarity::Positive),
                "Negative" => FileAttribute::FilePolarity(FilePolarity::Negative),
                _ => panic!("Unsupported Polarity type '{}' in TF statement", attr_args[1]) 
            },
            "Md5" => FileAttribute::Md5(attr_args[1].to_string()),
            _ => panic!("The AttributeName '{}' is currently not supported for File Attributes", attr_args[0])
        };
        gerber_doc.commands.push(
            ExtendedCode::FileAttribute(file_attr).into()
        )
    }
    else { panic!("Unable to parse file attribute (TF)" )}
}


/// Parse an Aperture Attribute (%TA.<AttributeName>[,<AttributeValue>]*%) into Command
/// 
/// For now we consider two types of TA statements:
/// 1. Aperture Function (AperFunction) with field: String
/// 2. Drill tolerance (DrillTolerance) with fields: [1] num [2] num 
/// 
/// ⚠️ Any other Attributes (which seem to be valid within the gerber spec) we will **fail** to parse!
/// 
/// ⚠️ This parsing statement needs a lot of tests and validation at the current stage!
fn parse_aperture_attribute(line: Chars, re: &Regex, gerber_doc: &mut GerberDoc) {
    let attr_args = get_attr_args(line);
    println!("TA ARGS: {:?}", attr_args);
    if attr_args.len() >= 2 {  // we must have at least 1 field
        //println!("TA args are: {:?}", attr_args);
        match attr_args[0] {
            "AperFunction" => {
                let aperture_func: ApertureFunction = match attr_args[1] {
                    "ViaDrill" => ApertureFunction::ViaDrill,
                    "BackDrill" => ApertureFunction::BackDrill,
                    "ComponentDrill" => ApertureFunction::ComponentDrill{ press_fit: None },  // TODO parse this
                    "CastellatedDrill" => ApertureFunction::CastellatedDrill,
                    "MechanicalDrill" => ApertureFunction::MechanicalDrill { function: None }, // TODO parse this
                    "Slot" => ApertureFunction::Slot,
                    "CutOut" => ApertureFunction::CutOut,
                    "Cavity" => ApertureFunction::Cavity,
                    "OtherDrill" => ApertureFunction::OtherDrill(attr_args[2].to_string()),
                    "ComponentPad " => ApertureFunction::ComponentPad{ press_fit: None }, // TODO parse this
                    "SmdPad" => match attr_args[2] {
                        "CopperDefined" => ApertureFunction::SmdPad(SmdPadType::CopperDefined),
                        "SoldermaskDefined" => ApertureFunction::SmdPad(SmdPadType::SoldermaskDefined),
                        _ => panic!("Unsupported SmdPad type in TA statement")
                    },
                    "BgaPad" => match attr_args[2] {
                        "CopperDefined" => ApertureFunction::BgaPad(SmdPadType::CopperDefined),
                        "SoldermaskDefined" => ApertureFunction::BgaPad(SmdPadType::SoldermaskDefined),
                        _ => panic!("Unsupported SmdPad type in TA statement")
                    },
                    "HeatsinkPad" => ApertureFunction::HeatsinkPad,
                    "TestPad" => ApertureFunction::TestPad,
                    "CastellatedPad" => ApertureFunction::CastellatedPad,
                    "FiducialPad" => match attr_args[2]{
                        "Global" => ApertureFunction::FiducialPad(FiducialScope::Global),
                        "Local" => ApertureFunction::FiducialPad(FiducialScope::Local),
                        _ => panic!("Unsupported FiducialPad type in TA statement"),
                    },
                    "ThermalReliefPad" => ApertureFunction::ThermalReliefPad,
                    "WasherPad" => ApertureFunction::WasherPad,
                    "AntiPad" => ApertureFunction::AntiPad,
                    "OtherPad" => ApertureFunction::OtherPad(attr_args[2].to_string()),
                    "Conductor" => ApertureFunction::Conductor,
                    "NonConductor" => ApertureFunction::NonConductor,
                    "CopperBalancing" => ApertureFunction::CopperBalancing,
                    "Border" => ApertureFunction::Border,
                    "OtherCopper" => ApertureFunction::OtherCopper(attr_args[2].to_string()),
                    "Profile" => ApertureFunction::Profile,
                    "NonMaterial" => ApertureFunction::NonMaterial,
                    "Material" => ApertureFunction::Material,
                    "Other" => ApertureFunction::Other(attr_args[2].to_string()),
                    _ => panic!("The  Aperture Function '{}' is currently not supported (/known)", attr_args[1])
                };
                gerber_doc.commands.push(
                    ExtendedCode::ApertureAttribute(ApertureAttribute::ApertureFunction(aperture_func)).into()
                )
            },
            "DrillTolerance" => {
                gerber_doc.commands.push(
                    ExtendedCode::ApertureAttribute(ApertureAttribute::DrillTolerance{
                         plus: attr_args[1].parse::<f64>().unwrap(),
                         minus: attr_args[2].parse::<f64>().unwrap()
                         }).into()
                )
            }
            _ => panic!("The AttributeName '{}' is currently not supported for Aperture Attributes", attr_args[0])
        }        
    }
    else { panic!("Unable to parse aperture attribute (TA)" )} 
}


fn parse_object_attribute(line: Chars, re: &Regex, gerber_doc: &mut GerberDoc) {
    let attr_args = get_attr_args(line);
    if attr_args.len() >= 2 {
        gerber_doc.commands.push(
            ExtendedCode::ObjectAttribute(ObjectAttribute{
                attribute_name: attr_args[0].to_string(),
                values: attr_args[1..].into_iter().map(|val| val.to_string()).collect()
            }).into()
        )
    } else if attr_args.len() == 1 {
        panic!("Unable to add Object Attribute (TO) - TO statements need at least 1 field value on top of the name: '{}'", attr_args[0]);
    } else {
         panic!("Unable to parse object attribute (TO)");
    }
}


fn parse_delete_attribute(line: Chars, re: &Regex, gerber_doc: &mut GerberDoc) {
    let attr_args = get_attr_args(line);
    if attr_args.len() == 1 {   
        gerber_doc.commands.push(
            ExtendedCode::DeleteAttribute(attr_args[0].to_string()).into()
        )        
    } else if attr_args.len() > 1 {
        panic!("Unable to parse delete attribute (TD) - TD should not have any fields, got field '{}'", attr_args[0]);
    } else {
        panic!("Unable to parse delete attribute (TD)");
    }
}


/// Extract the individual elements (AttributeName and Fields) from Chars
/// 
/// The arguments of the attribute statement can have whitespace as this will be trimmed. 
/// `attribute_chars` argument must be the **trimmed line** from the gerber file
/// with the **first three characters removed**. E.g. ".Part,single*%" not "%TF.Part,single*%"
/// ```
/// # use gerber_parser::parser::get_attr_args;
/// let attribute_chars = ".DrillTolerance, 0.02, 0.01 *%".chars();
/// 
/// let arguments = get_attr_args(attribute_chars);
/// assert_eq!(arguments, vec!["DrillTolerance","0.02","0.01"])
/// ```
pub fn get_attr_args(mut attribute_chars: Chars) -> Vec<&str>{
    attribute_chars.next_back().unwrap();
    attribute_chars.next_back().unwrap();
    attribute_chars.next().unwrap();
    attribute_chars.as_str().split(",").map(|el| el.trim()).collect()
} 


pub fn coordinates_from_gerber(mut x_as_int: i64, mut y_as_int: i64, fs: CoordinateFormat) -> Coordinates {
    // we have the raw gerber string as int but now have to convert it to nano precision format 
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    Coordinates::new(CoordinateNumber::new(x_as_int), CoordinateNumber::new(y_as_int), fs)
}


pub fn coordinates_offset_from_gerber(mut x_as_int: i64, mut y_as_int: i64, fs: CoordinateFormat) -> CoordinateOffset {
    // we have the raw gerber string as int but now have to convert it to nano precision format 
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    CoordinateOffset::new(CoordinateNumber::new(x_as_int), CoordinateNumber::new(y_as_int), fs)
}


