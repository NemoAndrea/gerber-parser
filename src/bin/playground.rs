use gerber_parser::parser::parse_gerber;

use std::io::BufReader;
use stringreader::StringReader;
use gerber_types::GerberCode;

use std::str;


fn main() {
    let reader = BufReader::new(StringReader::new("    
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
    X4000Y1500I-3000J-10000D01*

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
    "));

    let mut file = Vec::<u8>::new();
    parse_gerber(reader).to_commands().serialize(&mut file).unwrap();
    println!("GBR >> {}",str::from_utf8(&file).unwrap())
}


