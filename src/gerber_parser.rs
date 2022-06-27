use std::{io::{Read, BufReader, BufRead}, convert::TryInto};
use gerber_types::{Command, ExtendedCode, Unit, FunctionCode, GCode, CoordinateFormat, ApertureDefinition, Aperture, Circle, Rectangular, Polygon};
use regex::Regex;


fn parse_gerber<T: Read>(reader: BufReader<T>) -> Vec<Command> {
    let mut gerber_cmds:Vec<Command> = Vec::new();

    // naively define some regex queries
    let re_units = Regex::new(r"%MO(.*)\*%").unwrap();
    let re_comment = Regex::new(r"G04 (.*)\*").unwrap();
    let re_formatspec = Regex::new(r"%FSLAX(.*)Y(.*)\*%").unwrap();
    let re_aperture = Regex::new(r"%ADD([0-9]+)([A-Z]),(.*)\*%").unwrap();

    for (index, line) in reader.lines().enumerate() {
        let line = line.unwrap(); // Ignore errors.
        // Show the line and its number.
        //println!("{}. {}", index + 1, &line);
        if !line.is_empty() {
            // units
            if let Some(regmatch) = re_units.captures(&line) {
                let units_str = regmatch.get(1).unwrap().as_str();
                if units_str == "MM" {
                    gerber_cmds.push(ExtendedCode::Unit(Unit::Millimeters).into());
                } else if units_str == "IN" {
                    gerber_cmds.push(ExtendedCode::Unit(Unit::Inches).into())
                } else { panic!("Incorrect gerber units format")}
            } 

            // Gerber Format Specification
            if let Some(regmatch) = re_formatspec.captures(&line) {
                let mut fs_chars = regmatch.get(1).unwrap().as_str().chars();
                let integer:u8 = fs_chars.next().unwrap().to_digit(10).unwrap() as u8;
                let decimal:u8 = fs_chars.next().unwrap().to_digit(10).unwrap() as u8;

                // the gerber spec states that the integer value can be at most 6
                assert!(integer >= 1 && integer <= 6, "format spec integer value must be between 1 and 6");

                let fs = CoordinateFormat::new(integer, decimal);                  
                gerber_cmds.push(ExtendedCode::CoordinateFormat(fs).into());
            } 

            // comments
            if let Some(regmatch) = re_comment.captures(&line) {
                let comment = regmatch.get(1).unwrap().as_str();
                gerber_cmds.push(FunctionCode::GCode(GCode::Comment(comment.to_string())).into());
            } 

            // aperture definitions
            // TODO: prevent the same aperture code being used twice
            if let Some(regmatch) = re_aperture.captures(&line) {
                let code = regmatch.get(1).unwrap().as_str().parse::<i32>().expect("Failed to parse aperture code");
                assert!(code > 9, "Aperture codes 0-9 cannot be used for custom apertures");

                
                let aperture_type = regmatch.get(2).unwrap().as_str();
                let aperture_args:  Vec<&str> = regmatch.get(3).unwrap().as_str().split("X").collect();

                println!("The code is {}, and the aperture type is {} with params {:?}", code, aperture_type, aperture_args);

                match aperture_type {
                    "C" => gerber_cmds.push(ExtendedCode::ApertureDefinition(ApertureDefinition {
                        code: code,
                        aperture: Aperture::Circle(Circle {
                            diameter: aperture_args[0].trim().parse::<f64>().unwrap(),
                            hole_diameter: if aperture_args.len() > 1 {
                                Some(aperture_args[1].trim().parse::<f64>().unwrap())} else {None}
                        }),
                    }).into()),
                    "R" => gerber_cmds.push(ExtendedCode::ApertureDefinition(ApertureDefinition {
                        code: code,
                        aperture: Aperture::Rectangle(Rectangular {
                            x: aperture_args[0].trim().parse::<f64>().unwrap(),
                            y: aperture_args[1].trim().parse::<f64>().unwrap(),
                            hole_diameter: if aperture_args.len() > 2 {
                                Some(aperture_args[2].trim().parse::<f64>().unwrap())} else {None}
                        }),
                    }).into()),
                    "O" => gerber_cmds.push(ExtendedCode::ApertureDefinition(ApertureDefinition {
                        code: code,
                        aperture: Aperture::Obround(Rectangular {
                            x: aperture_args[0].trim().parse::<f64>().unwrap(),
                            y: aperture_args[1].trim().parse::<f64>().unwrap(),
                            hole_diameter: if aperture_args.len() > 2 {
                                Some(aperture_args[2].trim().parse::<f64>().unwrap())} else {None}
                        }),
                    }).into()),
                    // note that for polygon we have to specify rotation if we want to add a hole
                    "P" => gerber_cmds.push(ExtendedCode::ApertureDefinition(ApertureDefinition {
                        code: code,
                        aperture: Aperture::Polygon(Polygon {
                            diameter: aperture_args[0].trim().parse::<f64>().unwrap(),
                            vertices: aperture_args[1].trim().parse::<u8>().unwrap(),
                            rotation: if aperture_args.len() > 2 {
                                Some(aperture_args[2].trim().parse::<f64>().unwrap())} else {None},
                            hole_diameter: if aperture_args.len() > 3 {
                                Some(aperture_args[3].trim().parse::<f64>().unwrap())} else {None}
                        }),
                    }).into()),
                    
                    
                    _ => println!("ignoring")
                    
                }
                

                


            }
        }
    }


    return gerber_cmds
}




#[cfg(test)]
mod tests {
    use std::io::BufReader;
    use stringreader::StringReader;
    use gerber_types::{GCode, FunctionCode, Command, ExtendedCode, Unit, CoordinateFormat,
        ApertureDefinition, Aperture, Circle, Rectangular, Polygon};

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

        let filter_commands = |cmds:Vec<Command>| -> Vec<Command> {
            cmds.into_iter().filter(|cmd| match cmd {
                 Command::ExtendedCode(ExtendedCode::CoordinateFormat(_)) => true, _ => false}).collect()};

        assert_eq!(filter_commands(parse_gerber(reader_fs_1)), vec![
            Command::ExtendedCode(ExtendedCode::CoordinateFormat(CoordinateFormat::new(1, 5)))]);

        assert_eq!(filter_commands(parse_gerber(reader_fs_2)), vec![
            Command::ExtendedCode(ExtendedCode::CoordinateFormat(CoordinateFormat::new(3, 6)))]);

        assert_eq!(filter_commands(parse_gerber(sample_gerber_1)), vec![
            Command::ExtendedCode(ExtendedCode::CoordinateFormat(CoordinateFormat::new(2, 3)))]);
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

        let filter_commands = |cmds:Vec<Command>| -> Vec<Command> {
            cmds.into_iter().filter(|cmd| match cmd {
                 Command::ExtendedCode(ExtendedCode::Unit(_)) => true, _ => false}).collect()};

        assert_eq!(filter_commands(parse_gerber(reader_mm)), vec![
            Command::ExtendedCode(ExtendedCode::Unit(Unit::Millimeters))]);

        assert_eq!(filter_commands(parse_gerber(reader_in)), vec![
            Command::ExtendedCode(ExtendedCode::Unit(Unit::Inches))]);
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

        assert_eq!(filter_commands(parse_gerber(reader)), vec![
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

        let filter_commands = |cmds:Vec<Command>| -> Vec<Command> {
            cmds.into_iter().filter(|cmd| match cmd {
                 Command::ExtendedCode(ExtendedCode::ApertureDefinition(_)) => true, _ => false}).collect()};

        assert_eq!(filter_commands(parse_gerber(reader)), vec![
            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 999,
                aperture: Aperture::Circle(Circle {diameter: 0.01, hole_diameter: None})})),
            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 22,
                aperture: Aperture::Rectangle(Rectangular{x: 0.01,y: 0.15,hole_diameter: None})})),
            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 23,
                aperture: Aperture::Obround(Rectangular{x: 0.01,y: 0.15,hole_diameter: None})})),
            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 21,
                aperture: Aperture::Polygon(Polygon{diameter: 0.7,vertices: 10,rotation: None, hole_diameter: None})})),
            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 24,
                aperture: Aperture::Polygon(Polygon{diameter: 0.7,vertices: 10,rotation: Some(16.5),hole_diameter: None})})),

            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 123,
                aperture: Aperture::Circle(Circle {diameter: 0.01,hole_diameter: Some(0.003)})})),
            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 124,
                aperture: Aperture::Rectangle(Rectangular {x: 0.1,y: 0.15,hole_diameter: Some(0.00001)})})),
            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 125,
                aperture: Aperture::Obround(Rectangular {x: 0.1,y: 0.15,hole_diameter: Some(0.019)})})),
            Command::ExtendedCode(ExtendedCode::ApertureDefinition(ApertureDefinition {
                code: 126,
                aperture: Aperture::Polygon(Polygon{diameter: 1.0,vertices: 7,rotation: Some(5.5),hole_diameter: Some(0.7)})})),
            ])
    }
}
