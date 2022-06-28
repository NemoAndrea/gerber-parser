use gerber_types::{GCode, FunctionCode, Command, Unit, CoordinateFormat, Aperture, Circle,
     Rectangular, Polygon};
use::std::collections::HashMap;

use gerber_parser::parser::parse_gerber;
use gerber_parser::gerber_doc::empty_gerber;

mod utils;

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
    let gerber_reader = utils::gerber_to_reader(&SAMPLE_GERBER_1);
    let gbr = parse_gerber(gerber_reader);
    println!("{}",&gbr);
    assert_eq!(gbr, empty_gerber());
}

#[test]
fn format_specification() {
    let reader_fs_1 = utils::gerber_to_reader("
    %FSLAX15Y15*%
    %MOMM*%
    M02*        
    ");

    let reader_fs_2 = utils::gerber_to_reader("
    %FSLAX36Y36*%
    %MOIN*%
    G04 Actual apertures and draw commands go here*
    M02*        
    ");

    let sample_gerber_1 = utils::gerber_to_reader(&SAMPLE_GERBER_1);

    assert_eq!(parse_gerber(reader_fs_1).format_specification, Some(CoordinateFormat::new(1, 5)));

    assert_eq!(parse_gerber(reader_fs_2).format_specification, Some(CoordinateFormat::new(3, 6)));

    assert_eq!(parse_gerber(sample_gerber_1).format_specification, Some(CoordinateFormat::new(2, 3)));
}

#[test]
fn units() {
    let reader_mm = utils::gerber_to_reader("
    G04 The next line specifies the precision of the units*
    %FSLAX23Y23*%
    G04 The next line specifies the units (inches or mm)*
    %MOMM*%

    G04 Actual apertures and draw commands go here*
    M02*        
    ");

    let reader_in = utils::gerber_to_reader("
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
    let reader = utils::gerber_to_reader("
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
    let reader = utils::gerber_to_reader("
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
    let reader = utils::gerber_to_reader("
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
fn missing_eof() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    %MOMM*%-

    G04 We should have a MO2 at the end, but what if we forget it?*      
    ");
    parse_gerber(reader);
}

#[test]
#[should_panic]
fn multiple_unit_statements() {
    let reader = utils::gerber_to_reader("
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
fn multiple_fs_statements() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%
    G04 We can only declare the format specification once in a document* 
    %FSLAX46Y46*%
    %MOMM*%
        
    M02*  
    ");
    parse_gerber(reader);
}

#[test]
#[should_panic]
fn nonexistent_aperture_selection() {
    let reader = utils::gerber_to_reader("
    %FSLAX23Y23*%        
    %MOMM*%

    %ADD100P, 0.7X10X16.5*%

    G04 We should not be able to select apertures that are not defined* 
    D
        
    M02*  
    ");
    parse_gerber(reader);
}