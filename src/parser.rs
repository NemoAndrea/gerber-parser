use std::io::{Read, BufReader, BufRead};

use gerber_types::{Command, ExtendedCode, Unit, FunctionCode, GCode, CoordinateFormat,
                   Aperture, Circle, Rectangular, Polygon, MCode, DCode, Polarity,
                   InterpolationMode, QuadrantMode, Operation, Coordinates, CoordinateNumber, 
                   CoordinateOffset, ApertureAttribute, ApertureFunction, FiducialScope, SmdPadType, 
                   FileAttribute, FilePolarity, Part, FileFunction, StepAndRepeat};
use regex::Regex;
use std::str::Chars;
use crate::error::GerberParserError;
use crate::gerber_doc::{ GerberDoc};
use lazy_regex::*;

// naively define some regex terms
// TODO see which ones can be done without regex for better performance?
static RE_UNITS: Lazy<Regex> = lazy_regex!(r"%MO(.*)\*%");
static RE_COMMENT: Lazy<Regex> = lazy_regex!(r"G04 (.*)\*");
static RE_FORMAT_SPEC: Lazy<Regex> = lazy_regex!(r"%FSLAX(.*)Y(.*)\*%");
static RE_APERTURE: Lazy<Regex> = lazy_regex!(r"%ADD([0-9]+)([A-Z]),(.*)\*%");
static RE_INTERPOLATION: Lazy<Regex> = lazy_regex!(r"X?(-?[0-9]+)?Y?(-?[0-9]+)?I?(-?[0-9]+)?J?(-?[0-9]+)?D01\*");
static RE_MOVE_OR_FLASH: Lazy<Regex> = lazy_regex!(r"X?(-?[0-9]+)?Y?(-?[0-9]+)?D0[2-3]*");
static RE_IMAGE_NAME: Lazy<Regex> = lazy_regex!(r"%IN(.*)\*%");
static RE_IMAGE_POLARITY: Lazy<Regex> = lazy_regex!(r"%IP(.*)\*%");
// TODO: handle escaped characters for attributes
static RE_ATTRIBUTES: Lazy<Regex> = lazy_regex!(r"%T[A-Z].([A-Z]+?),?");
static RE_STEP_REPEAT: Lazy<Regex> = lazy_regex!(r"%SRX([0-9]+)Y([0-9]+)I(\d+\.?\d*)J(\d+\.?\d*)\*%");


/// Parse a gerber string (in BufReader) to a GerberDoc
///
/// Take the contents of a Gerber (.gbr) file and parse it to a GerberDoc struct. The parsing does
/// some semantic checking, but is certainly not exhaustive - so don't rely on it to check if
/// your Gerber file is valid according to the spec. Some of the parsing steps are greedy - they may
/// match something unexpected (rather than panicking) if there is a typo/fault in your file.
pub fn parse_gerber<T: Read>(reader: BufReader<T>) -> GerberDoc {
    let mut gerber_doc = GerberDoc::new();
    // The gerber spec allows omission of X or Y statements in D01/2/3 commands, where the omitted
    // coordinate is to be taken as whatever was used in the previous coordinate-using command
    // By default the 'last coordinate' can be taken to be (0,0)
    let mut last_coords = (0i64,0i64);
    

    for (index, line) in reader.lines().enumerate() {
        let raw_line = line.expect("IO Error reading line"); 
        // TODO combine this with line above
        let line = raw_line.trim();

        // Show the line 
        //println!("{}. {}", index + 1, &line);
        if !line.is_empty() {
            let mut linechars = line.chars();

            match linechars.next().unwrap() {
                'G' => {
                    match linechars.next().unwrap() {
                        '0' =>  match linechars.next().unwrap() {
                            '1' => gerber_doc.commands.push(
                                Ok(FunctionCode::GCode(
                                    GCode::InterpolationMode(InterpolationMode::Linear)
                                ).into())
                            ), // G01
                            '2' => gerber_doc.commands.push(
                                Ok(FunctionCode::GCode(
                                    GCode::InterpolationMode(InterpolationMode::ClockwiseCircular)
                                ).into())
                            ), // G02
                            '3' => gerber_doc.commands.push(
                                Ok(FunctionCode::GCode(
                                    GCode::InterpolationMode(InterpolationMode::CounterclockwiseCircular)
                                ).into())
                            ), // G03
                            '4' => {gerber_doc.commands.push(parse_comment(line)) }, // G04
                            _ => gerber_doc.commands.push(
                                Err(GerberParserError::UnknownCommand(line.to_string()))
                            ),
                        },
                        '3'=> match linechars.next().unwrap() {
                            '6' => gerber_doc.commands.push(
                                Ok(FunctionCode::GCode(GCode::RegionMode(true)).into())
                            ), // G36
                            '7' => gerber_doc.commands.push(
                                Ok(FunctionCode::GCode(GCode::RegionMode(false)).into())
                            ), // G37
                            _ => gerber_doc.commands.push(
                                Err(GerberParserError::UnknownCommand(line.to_string()))
                            ),
                        },
                        '7' => match linechars.next().unwrap() {
                            // the G74 command is technically part of the Deprecated commands
                            '4' => gerber_doc.commands.push(
                                Ok(FunctionCode::GCode(
                                    GCode::QuadrantMode(QuadrantMode::Single)
                                ).into())
                            ), // G74
                            '5' => gerber_doc.commands.push(
                                Ok(FunctionCode::GCode(
                                    GCode::QuadrantMode(QuadrantMode::Multi)
                                ).into())
                            ), // G74
                            _ => gerber_doc.commands.push(
                                Err(GerberParserError::UnknownCommand(line.to_string()))
                            ),
                        }, // G75
                        _ => gerber_doc.commands.push(
                            Err(GerberParserError::UnknownCommand(line.to_string()))
                        ),
                    }
                },
                '%' => {
                    match linechars.next().unwrap() {
                        'M' => { 
                            match parse_units(line, &gerber_doc){
                                Ok(units) => {
                                    gerber_doc.units = Some(units);
                                }
                                Err(e) => {
                                    gerber_doc.commands.push(Err(e));
                                }
                            }
                        },
                        'F' => { 
                            match parse_format_spec(line, &gerber_doc){
                                Ok(format_spec) => {
                                    gerber_doc.format_specification = Some(format_spec);
                                }
                                Err(e) => {
                                    gerber_doc.commands.push(Err(e));
                                }
                            } 
                        },
                        'A' => match linechars.next().unwrap() {
                            'D' => { 
                                match parse_aperture_defs(line, &gerber_doc){
                                    Ok((code, ap)) => {
                                        gerber_doc.apertures.insert(code, ap); 
                                        // Safety: While insert can 'fail' (misbehave) if the key 
                                        // already exists, 
                                        // `parse_aperture_defs` explicitly checks for this
                                    }
                                    Err(err) => {
                                        gerber_doc.commands.push(Err(err));
                                    }
                                } 
                            }, // AD
                            'M' => {gerber_doc.commands.push(
                                Err(GerberParserError::UnsupportedCommand(line.to_string()))
                            )}, // AM
                            _ => gerber_doc.commands.push(
                                Err(GerberParserError::UnknownCommand(line.to_string()))
                            )
                        }, 
                        'L' => match linechars.next().unwrap() {
                            'P' => match linechars.next().unwrap() {
                                'D' => gerber_doc.commands.push(
                                    Ok(ExtendedCode::LoadPolarity(Polarity::Dark).into())
                                ), // LPD
                                'C' => gerber_doc.commands.push(
                                    Ok(ExtendedCode::LoadPolarity(Polarity::Clear).into())
                                ), // LPC
                                _ => gerber_doc.commands.push(
                                    Err(GerberParserError::UnknownCommand(line.to_string()))
                                )
                            }, // LP
                            'M' => { gerber_doc.commands.push(
                                Err(GerberParserError::UnsupportedCommand(line.to_string()))
                            ) }, // LM 
                            'R' => { gerber_doc.commands.push(
                                Err(GerberParserError::UnsupportedCommand(line.to_string()))
                            ) }, // LR
                            'S' => { gerber_doc.commands.push(
                                Err(GerberParserError::UnsupportedCommand(line.to_string()))
                            ) }, // LS
                            _ => gerber_doc.commands.push(
                                Err(GerberParserError::UnknownCommand(line.to_string()))
                            )
                        },
                        'T' => match linechars.next().unwrap() {
                            'F' => {
                                gerber_doc.commands.push(
                                    parse_file_attribute(linechars).map(|file_attr| {
                                        ExtendedCode::FileAttribute(file_attr).into()
                                    })
                                );
                            },
                            'A' => { gerber_doc.commands.push(parse_aperture_attribute(linechars)) },
                            'D' => { gerber_doc.commands.push(parse_delete_attribute(linechars)) },
                            _ => gerber_doc.commands.push(
                                Err(GerberParserError::UnknownCommand(line.to_string()))
                            )
                        },
                        'S' => match linechars.next().unwrap() { 
                            'R' => match linechars.next().unwrap() {
                                'X' => gerber_doc.commands.push(parse_step_repeat_open(line)),
                                // a statement %SR*% closes a step repeat command, which has no parameters
                                '*' => gerber_doc.commands.push(
                                    Ok(ExtendedCode::StepAndRepeat(StepAndRepeat::Close).into())
                                ),
                                _ => gerber_doc.commands.push(
                                    Err(GerberParserError::UnknownCommand(line.to_string()))
                                )
                            },
                            _ => gerber_doc.commands.push(
                                Err(GerberParserError::UnknownCommand(line.to_string()))
                            )                          
                        },
                        'I' => match linechars.next().unwrap() {
                            'N' => { // Image Name, 8.1.3. Deprecated, but still used by fusion 360.
                                match parse_image_name(line, &gerber_doc) {
                                    Ok(name) => {
                                        gerber_doc.image_name = Some(name);
                                    }
                                    Err(e) => {
                                        gerber_doc.commands.push(Err(e))
                                    }
                                }
                            }, 
                            'P' => { gerber_doc.commands.push(
                                Err(GerberParserError::UnsupportedCommand(line.to_string()))
                            ) }, // Image Polarity, basically useless, but used by fusion
                            _ => gerber_doc.commands.push(
                                Err(GerberParserError::UnknownCommand(line.to_string()))
                            )
                        }
                        _ => gerber_doc.commands.push(
                            Err(GerberParserError::UnknownCommand(line.to_string()))
                        )
                    }
                },
                'X' | 'Y' => {linechars.next_back(); match linechars.next_back().unwrap() { 
                    '1' => gerber_doc.commands.push(
                        parse_interpolation(line, &gerber_doc, &mut last_coords)
                    ), // D01
                    '2' => gerber_doc.commands.push(
                        parse_move_or_flash(line, &gerber_doc, &mut last_coords, false)
                    ), // D02
                    '3' => gerber_doc.commands.push(
                        parse_move_or_flash(line, &gerber_doc, &mut last_coords, true)
                    ), // D03
                    _ => gerber_doc.commands.push(
                        Err(GerberParserError::UnknownCommand(line.to_string()))
                    )
                }},
                'D' => { // select aperture D<num>*                   
                    linechars.next_back(); // remove the trailing '*'
                    gerber_doc.commands.push(parse_aperture_selection(linechars, &gerber_doc))
                },                
                'M' => { gerber_doc.commands.push(Ok(FunctionCode::MCode(MCode::EndOfFile).into())) }                
                _ => gerber_doc.commands.push(Err(GerberParserError::UnknownCommand(line.to_string())))
            }           
        }
    }

    // TODO: check that we ended with a gerber EOF command
    match gerber_doc.commands.last(){
        None => {gerber_doc.commands.push(Err(GerberParserError::NoEndOfFile))}
        Some(command) => {
            match command{
                Ok(Command::FunctionCode(FunctionCode::MCode(MCode::EndOfFile))) => {}
                _ => {gerber_doc.commands.push(Err(GerberParserError::NoEndOfFile))}
            }
        }
    }
    return gerber_doc
}


/// parse a Gerber Comment (e.g. 'G04 This is a comment*')
fn parse_comment(line: &str) -> Result<Command, GerberParserError> {
    match RE_COMMENT.captures(line) {
        Some(regmatch) => {
            let comment = regmatch.get(1)
                .ok_or(GerberParserError::MissingRegexCapture(line.to_string(), RE_COMMENT.clone()))?
                .as_str();
            Ok(FunctionCode::GCode(GCode::Comment(comment.to_string())).into())
        }
        None => { Err(GerberParserError::NoRegexMatch(line.to_string(), RE_COMMENT.clone())) }
    }
    
}

/// parse an image name. This is optional and deprecated, but included in all exports from Fusion 360
fn parse_image_name(line: &str, gerber_doc: &GerberDoc) -> Result<String, GerberParserError> {
    if gerber_doc.image_name.is_some(){
        Err(GerberParserError::TriedToSetImageNameTwice(line.to_string()))
    } else {
        match RE_IMAGE_NAME.captures(line) {
            Some(regmatch) => {
                let image_name = regmatch.get(1)
                    .ok_or(GerberParserError::MissingRegexCapture(line.to_string(), RE_IMAGE_NAME.clone()))?
                    .as_str();
                Ok(String::from(image_name))
            }
            None => { Err(GerberParserError::NoRegexMatch(line.to_string(), RE_IMAGE_NAME.clone())) }
        }
    }
}


/// parse a Gerber unit statement (e.g. '%MOMM*%')
fn parse_units(line: &str, gerber_doc: &GerberDoc) -> Result<Unit, GerberParserError> {
    // Check that the units are not set yet (that would imply the set unit command is present twice)
    if gerber_doc.units.is_some() { 
        Err(GerberParserError::TriedToSetUnitsTwice(line.to_string()))
    } else {
        match RE_UNITS.captures(line) {
            Some(regmatch) => {
                let units_str = regmatch.get(1)
                    .ok_or(GerberParserError::MissingRegexCapture(line.to_string(), RE_UNITS.clone()))?
                    .as_str();
                match units_str {
                    "MM" => Ok(Unit::Millimeters),
                    "IN" => Ok(Unit::Inches),
                    _ => Err(GerberParserError::InvalidUnitFormat(line.to_string())),
                }
            }
            None => { Err(GerberParserError::NoRegexMatch(line.to_string(), RE_UNITS.clone())) }
        }
    }
}


/// parse a Gerber format spec statement (e.g. '%FSLAX23Y23*%')
fn parse_format_spec(line: &str, gerber_doc: &GerberDoc) -> Result<CoordinateFormat, GerberParserError> {
    // Ensure that FS was not set before, which would imply two FS statements in the same doc
    if gerber_doc.format_specification.is_some() { 
        Err(GerberParserError::TriedToFormatTwice(line.to_string())) 
    } else {
        match RE_FORMAT_SPEC.captures(line) {
            Some(regmatch) => {
                let mut fs_chars = regmatch.get(1)
                    .ok_or(GerberParserError::MissingRegexCapture(line.to_string(), RE_FORMAT_SPEC.clone()))?
                    .as_str().chars();
                let integer:u8 = parse_char(fs_chars.next()
                    .ok_or(GerberParserError::ParseFormatErrorWrongNumDigits(line.to_string()))?)?;
                let decimal:u8 = parse_char(fs_chars.next()
                    .ok_or(GerberParserError::ParseFormatErrorWrongNumDigits(line.to_string()))?)?;

                // the gerber spec states that the integer value can be at most 6
                if integer < 1 || integer > 6 {
                    return Err(GerberParserError::ParseFormatErrorInvalidDigit(integer))
                }

                Ok(CoordinateFormat::new(integer, decimal))
            }
            None => { Err(GerberParserError::NoRegexMatch(line.to_string(), RE_FORMAT_SPEC.clone())) }
        }
    }
}

/// helper function to move some ugly repeated .ok_or().unwrap().Arc<Mutex<Future>>
fn parse_char(char_in: char) -> Result<u8, GerberParserError> {
    Ok(char_in.to_digit(10).ok_or(GerberParserError::ParseDigitError(char_in))? as u8)
}


// parse a Gerber aperture definition e.g. '%ADD44R, 2.0X3.0*%')
fn parse_aperture_defs(line: &str, gerber_doc: &GerberDoc) -> Result<(i32, Aperture), GerberParserError> {
    // aperture definitions
    match RE_APERTURE.captures(line) {
        Some(regmatch) => {
            let code_str = regmatch.get(1)
                .ok_or(GerberParserError::MissingRegexCapture(line.to_string(), RE_APERTURE.clone()))?
                .as_str();
            let code = parse_aperture_code(code_str)?;

            let aperture_type = regmatch.get(2)
                .ok_or(GerberParserError::MissingRegexCapture(line.to_string(), RE_APERTURE.clone()))?
                .as_str();
            let aperture_args: Vec<&str> = regmatch.get(3)
                .ok_or(GerberParserError::MissingRegexCapture(line.to_string(), RE_APERTURE.clone()))?
                .as_str().split("X").collect();
            
            if gerber_doc.apertures.contains_key(&code){
                return Err(GerberParserError::ApertureDefinedTwice(code, line.to_string()));
            }

            //println!("The code is {}, and the aperture type is {} with params {:?}", code, aperture_type, aperture_args);
            match aperture_type {
                "C" => Ok((code, Aperture::Circle(Circle {
                    diameter: aperture_args[0].trim().parse::<f64>()
                        .map_err(|_| {
                            GerberParserError::ParseApertureDefinitionBodyError(code, line.to_string())
                        })?,
                    hole_diameter: if aperture_args.len() > 1 {
                        Some(aperture_args[1].trim().parse::<f64>()
                            .map_err(|_| {
                                GerberParserError::ParseApertureDefinitionBodyError(code, line.to_string())
                            })?
                        )
                    } else { None }
                }))),
                "R" => Ok((code, Aperture::Rectangle(Rectangular {
                    x: parse_coord::<f64>(aperture_args[0])?,
                    y: parse_coord::<f64>(aperture_args[1])?,
                    hole_diameter: if aperture_args.len() > 2 {
                        Some(aperture_args[2].trim().parse::<f64>()
                            .map_err(|_| {
                                GerberParserError::ParseApertureDefinitionBodyError(code, line.to_string())
                            })?
                        )
                    } else { None }
                }))),
                "O" => Ok((code, Aperture::Obround(Rectangular {
                    x: parse_coord::<f64>(aperture_args[0])?,
                    y: parse_coord::<f64>(aperture_args[1])?,
                    hole_diameter: if aperture_args.len() > 2 {
                        Some(aperture_args[2].trim().parse::<f64>()
                            .map_err(|_| {
                                GerberParserError::ParseApertureDefinitionBodyError(code, line.to_string())
                            })?)
                    } else { None }
                }))),
                // note that for polygon we HAVE TO specify rotation if we want to add a hole
                "P" => Ok((code, Aperture::Polygon(Polygon {
                    diameter: aperture_args[0].trim().parse::<f64>()
                        .map_err(|_| {
                            GerberParserError::ParseApertureDefinitionBodyError(code, line.to_string())
                        })?,
                    vertices: aperture_args[1].trim().parse::<u8>()
                        .map_err(|_| {
                            GerberParserError::ParseApertureDefinitionBodyError(code, line.to_string())
                        })?,
                    rotation: if aperture_args.len() > 2 {
                        Some(aperture_args[2].trim().parse::<f64>()
                            .map_err(|_| {
                                GerberParserError::ParseApertureDefinitionBodyError(code, line.to_string())
                            })?)
                    } else { None },
                    hole_diameter: if aperture_args.len() > 3 {
                        Some(aperture_args[3].trim().parse::<f64>()
                            .map_err(|_| {
                                GerberParserError::ParseApertureDefinitionBodyError(code, line.to_string())
                            })?)
                    } else { None }
                }))),
                unknown_type => { 
                    Err(GerberParserError::UnknownApertureType(unknown_type.to_string(), line.to_string()))
                }
            }
        }
        None => { Err(GerberParserError::NoRegexMatch(line.to_string(), RE_APERTURE.clone())) }
    }
}

fn parse_coord<T: std::str::FromStr>(coord_str: &str) -> Result<T, GerberParserError> {
    coord_str.trim().parse::<T>()
        .map_err(|_| {GerberParserError::FailedToParseCoordinate(coord_str.to_string())})
}


fn parse_aperture_code(code_str: &str) -> Result<i32, GerberParserError> {
    match code_str.parse::<i32>(){
        Ok(v) if (v > 9) => {
            Ok(v)
        }
        Err(_) => {
            Err(GerberParserError::ApertureCodeParseFailed(code_str.to_string()))
        }
        Ok(v) => {
            Err(GerberParserError::ApertureCodeParseFailed(code_str.to_string()))
        }
    }
}
fn parse_aperture_selection(
    linechars: Chars, 
    gerber_doc: &GerberDoc
)
    -> Result<Command, GerberParserError>
{
    let aperture_str = linechars.as_str();
    let aperture_code = parse_aperture_code(aperture_str)?;
    match gerber_doc.apertures.contains_key(&aperture_code) {
        true => {
            Ok(FunctionCode::DCode(DCode::SelectAperture(aperture_code)).into())
        }
        false => {
            Err(GerberParserError::ApertureNotDefined(aperture_code, aperture_str.to_string()))
        }
    }
}


// TODO clean up the except statements a bit
// parse a Gerber interpolation command (e.g. 'X2000Y40000I300J50000D01*')
fn parse_interpolation(
    line: &str, 
    gerber_doc: &GerberDoc, 
    last_coords: &mut (i64, i64)
)
    -> Result<Command, GerberParserError> 
{
    match RE_INTERPOLATION.captures(line) {
        Some(regmatch) => {
            let x_coord = match regmatch.get(1) {
                Some(x) => { 
                    let new_x = parse_coord::<i64>(x.as_str())?;
                    last_coords.0 = new_x;
                    new_x
                }
                None => last_coords.0, // if match is None, then the coordinate must have been implicit
            };
            let y_coord = match regmatch.get(2) {
                Some(y) => {
                    let new_y = parse_coord::<i64>(y.as_str())?;
                    last_coords.1 = new_y;
                    new_y
                }
                None => last_coords.1, // if match is None, then the coordinate must have been implicit
            };
    
            if let Some((i_offset_raw, j_offset_raw)) = regmatch
                .get(3)
                .zip(regmatch.get(4))
            {  //  we have X,Y,I,J parameters and we are doing circular interpolation
                let i_offset = parse_coord::<i64>(i_offset_raw.as_str())?;
                let j_offset = parse_coord::<i64>(j_offset_raw.as_str())?;
    
                Ok(FunctionCode::DCode(DCode::Operation(
                    Operation::Interpolate(
                        coordinates_from_gerber(
                            x_coord, 
                            y_coord, 
                            gerber_doc.format_specification.ok_or(
                                GerberParserError::OperationBeforeFormat(line.to_string())
                            )?
                        ), 
                        Some(coordinates_offset_from_gerber(
                            i_offset, 
                            j_offset, 
                            gerber_doc.format_specification.unwrap(/*Already checked above*/)
                        ))
                    )
                )).into())
            } else { // linear interpolation, only X,Y parameters
                Ok(FunctionCode::DCode(DCode::Operation(
                    Operation::Interpolate(
                        coordinates_from_gerber(
                            x_coord, 
                            y_coord, 
                            gerber_doc.format_specification.ok_or(
                                GerberParserError::OperationBeforeFormat(line.to_string())
                            )?
                        ), 
                        None
                    )
                )).into())
            }
        }
        None => { Err(GerberParserError::NoRegexMatch(line.to_string(), RE_INTERPOLATION.clone())) }
    }
}


// parse a Gerber move or flash command (e.g. 'X2000Y40000D02*')
fn parse_move_or_flash(
    line: &str, 
    gerber_doc: &GerberDoc, 
    last_coords: &mut (i64, i64), 
    flash: bool
) 
    -> Result<Command, GerberParserError> 
{
    match RE_MOVE_OR_FLASH.captures(line) {
        Some(regmatch) => {
            let x_coord = match regmatch.get(1) {
                Some(x) => {
                    let new_x = parse_coord::<i64>(x.as_str())?;
                    last_coords.0 = new_x;
                    new_x
                }
                None => last_coords.0, // if match is None, then the coordinate must have been implicit
            };
            let y_coord = match regmatch.get(2) {
                Some(y) => {
                    let new_y = parse_coord::<i64>(y.as_str())?;
                    last_coords.1 = new_y;
                    new_y
                }
                None => last_coords.1, // if match is None, then the coordinate must have been implicit
            };
            
            let coords = coordinates_from_gerber(
                x_coord, 
                y_coord, 
                gerber_doc.format_specification
                    .ok_or(GerberParserError::OperationBeforeFormat(line.to_string()))?,
            );
    
            if flash {
                Ok(FunctionCode::DCode(DCode::Operation(Operation::Flash(coords))).into())
            } else {
                Ok(FunctionCode::DCode(DCode::Operation(Operation::Move(coords))).into())
            }
        }
        None => { Err(GerberParserError::NoRegexMatch(line.to_string(), RE_MOVE_OR_FLASH.clone())) }
    }   
}


// fn parse_load_mirroring(mut linechars: Chars, gerber_doc: &mut GerberDoc) {
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
    // panic!("Load Mirroring (LM) command not supported yet.")
// }

// a step and repeat open statement has four (required) parameters that we need to extract
// X (pos int) Y (pos int), I (decimal), J (decimal)
fn parse_step_repeat_open(line: &str) -> Result<Command, GerberParserError> {
    match RE_STEP_REPEAT.captures(line) {
        Some(regmatch) => {
            Ok(ExtendedCode::StepAndRepeat(StepAndRepeat::Open{
                repeat_x: parse_coord::<u32>(regmatch.get(1)
                    .ok_or(GerberParserError::MissingRegexCapture(
                        line.to_string(), RE_STEP_REPEAT.clone()
                    ))?.as_str())?,
                repeat_y: parse_coord::<u32>(regmatch.get(2)
                    .ok_or(GerberParserError::MissingRegexCapture(
                        line.to_string(), RE_STEP_REPEAT.clone()
                    ))?.as_str())?,
                distance_x: parse_coord::<f64>(regmatch.get(3)
                    .ok_or(GerberParserError::MissingRegexCapture(
                        line.to_string(), RE_STEP_REPEAT.clone()
                    ))?.as_str())?,
                distance_y: parse_coord::<f64>(regmatch.get(4)
                    .ok_or(GerberParserError::MissingRegexCapture(
                        line.to_string(), RE_STEP_REPEAT.clone()
                    ))?.as_str())?,
            }).into())
        }
        None => { Err(GerberParserError::NoRegexMatch(line.to_string(), RE_STEP_REPEAT.clone())) }
    }
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
fn parse_file_attribute(line: Chars) -> Result<FileAttribute, GerberParserError> {

    let raw_line = line.as_str().to_string();
    
    let attr_args = get_attr_args(line)?;
    if attr_args.len() >= 2 {  // we must have at least 1 field
        //println!("TF args are: {:?}", attr_args);
        match attr_args[0] {
            "Part" => match attr_args[1]{
                "Single" => Ok(FileAttribute::Part(Part::Single)),
                "Array" => Ok(FileAttribute::Part(Part::Array)),
                "FabricationPanel" => Ok(FileAttribute::Part(Part::FabricationPanel)),
                "Coupon" => Ok(FileAttribute::Part(Part::Coupon)),
                "Other" => Ok(FileAttribute::Part(Part::Other(attr_args[2].to_string()))),
                _ => Err(GerberParserError::UnsupportedPartType(attr_args[1].to_string()))
            },
            // TODO do FileFunction properly, but needs changes in gerber-types
            "FileFunction" => Ok(FileAttribute::FileFunction(FileFunction::Other(
                attr_args[1].to_string()
            ))),  
            "FilePolarity" => match attr_args[1]{
                "Positive" => Ok(FileAttribute::FilePolarity(FilePolarity::Positive)),
                "Negative" => Ok(FileAttribute::FilePolarity(FilePolarity::Negative)),
                _ => Err(GerberParserError::UnsupportedPolarityType(attr_args[1].to_string()))
            },
            "Md5" => Ok(FileAttribute::Md5(attr_args[1].to_string())),
            _ => Err(GerberParserError::UnsupportedFileAttribute(attr_args[0].to_string()))
        }
    }
    else {
        Err(GerberParserError::FileAttributeParseError(raw_line))
    }
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
fn parse_aperture_attribute(line: Chars) -> Result<Command, GerberParserError> {
    let raw_line = line.as_str().to_string();
    let attr_args = get_attr_args(line)?;
    println!("TA ARGS: {:?}", attr_args);
    if attr_args.len() >= 2 {  // we must have at least 1 field
        match attr_args[0] {
            "AperFunction" => {
                Ok(ExtendedCode::ApertureAttribute(ApertureAttribute::ApertureFunction(match attr_args[1] {
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
                        _ => return Err(GerberParserError::UnsupportedApertureAttribute(raw_line))
                    },
                    "BgaPad" => match attr_args[2] {
                        "CopperDefined" => ApertureFunction::BgaPad(SmdPadType::CopperDefined),
                        "SoldermaskDefined" => ApertureFunction::BgaPad(SmdPadType::SoldermaskDefined),
                        _ => return Err(GerberParserError::UnsupportedApertureAttribute(raw_line))
                    },
                    "HeatsinkPad" => ApertureFunction::HeatsinkPad,
                    "TestPad" => ApertureFunction::TestPad,
                    "CastellatedPad" => ApertureFunction::CastellatedPad,
                    "FiducialPad" => match attr_args[2]{
                        "Global" => ApertureFunction::FiducialPad(FiducialScope::Global),
                        "Local" => ApertureFunction::FiducialPad(FiducialScope::Local),
                        _ => return Err(GerberParserError::UnsupportedApertureAttribute(raw_line))
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
                    _ => return Err(GerberParserError::UnsupportedApertureAttribute(raw_line))
                })).into())
            },
            "DrillTolerance" => {
                Ok(ExtendedCode::ApertureAttribute(ApertureAttribute::DrillTolerance{
                     plus: attr_args[1].parse::<f64>().unwrap(),
                     minus: attr_args[2].parse::<f64>().unwrap()
                     }).into())
            }
            _ => Err(GerberParserError::UnsupportedApertureAttribute(raw_line))
        }
    }
    else { Err(GerberParserError::InvalidApertureAttribute(raw_line)) }
}


fn parse_delete_attribute(line: Chars) -> Result<Command, GerberParserError>{
    let raw_line = line.as_str().to_string();
    let attr_args = get_attr_args(line)?;
    if attr_args.len() == 1 {
        Ok(ExtendedCode::DeleteAttribute(attr_args[0].to_string()).into())
    } else {
        Err(GerberParserError::InvalidDeleteAttribute(raw_line))
    }
}


/// Extract the individual elements (AttributeName and Fields) from Chars
/// 
/// The arguments of the attribute statement can have whitespace as this will be trimmed. 
/// `attribute_chars` argument must be the **trimmed line** from the gerber file
/// with the **first three characters removed**. E.g. ".Part,single*%" not "%TF.Part,single*%"
/// ```
/// use gerber_parser::parser::get_attr_args;
/// let attribute_chars = ".DrillTolerance, 0.02, 0.01 *%".chars();
/// 
/// let arguments = get_attr_args(attribute_chars).unwrap();
/// assert_eq!(arguments, vec!["DrillTolerance","0.02","0.01"])
/// ```
pub fn get_attr_args(mut attribute_chars: Chars) -> Result<Vec<&str>, GerberParserError> {
    attribute_chars.next_back()
        .ok_or(GerberParserError::InvalidFileAttribute(attribute_chars.as_str().to_string()))?;
    attribute_chars.next_back()
        .ok_or(GerberParserError::InvalidFileAttribute(attribute_chars.as_str().to_string()))?;
    attribute_chars.next()
        .ok_or(GerberParserError::InvalidFileAttribute(attribute_chars.as_str().to_string()))?;
    Ok(attribute_chars.as_str().split(",").map(|el| el.trim()).collect())
} 


pub fn coordinates_from_gerber(
    mut x_as_int: i64, 
    mut y_as_int: i64, 
    fs: CoordinateFormat
) 
    -> Coordinates 
{
    // we have the raw gerber string as int but now have to convert it to nano precision format 
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    Coordinates::new(CoordinateNumber::new(x_as_int), CoordinateNumber::new(y_as_int), fs)
}


pub fn coordinates_offset_from_gerber(
    mut x_as_int: i64, 
    mut y_as_int: i64, 
    fs: CoordinateFormat
) 
    -> CoordinateOffset 
{
    // we have the raw gerber string as int but now have to convert it to nano precision format 
    // (i.e. 6 decimal precision) as this is what CoordinateNumber uses internally
    let factor = (6u8 - fs.decimal) as u32;
    x_as_int *= 10i64.pow(factor);
    y_as_int *= 10i64.pow(factor);
    CoordinateOffset::new(CoordinateNumber::new(x_as_int), CoordinateNumber::new(y_as_int), fs)
}


