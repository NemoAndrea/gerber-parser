use std::fmt::Formatter;
use gerber_types::Aperture;
use regex::Regex;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum GerberParserError {
    #[error("Document included a line that isn't valid.")]
    UnknownCommand {},
    #[error("Document included a line that isn't supported.")]
    UnsupportedCommand{},
    #[error("Missing M02 statement at end of file")]
    NoEndOfFile,
    #[error("Command was uniquely identified, but did not match regex: {regex}.")]
    NoRegexMatch{
        regex: Regex,
    },
    #[error("Command was uniquely identified, and matched expected regex, \
    but did not contain the expected capture(s).\nRegex: {regex}.")]
    MissingRegexCapture{
        regex: Regex,
    },
    #[error("After gerber doc was already assigned a name, another name command was found.")]
    TriedToSetImageNameTwice{},
    #[error("After gerber doc was already assigned a unit, another unit command was found.")]
    TriedToSetUnitsTwice{},
    #[error("After gerber doc was already assigned a format specification, \
    another format specification command was found.")]
    TriedToFormatTwice{},
    #[error("Set unit command included unrecognized units: {units_str}.")]
    InvalidUnitFormat{
        units_str: String,
    },
    #[error("Error parsing format spec line. Looking for 2 digits but found 1 or none.\n
    expected something like \'%FSLAX23Y23*%\'.")]
    ParseFormatErrorWrongNumDigits{},
    #[error("format spec integer value must be between 1 and 6. Found {digit_found}.")]
    ParseFormatErrorInvalidDigit{
        digit_found: u8,
    },
    #[error("Error parsing char as base 10 digit: '{char_found:?}'.")]
    ParseDigitError{
        char_found: char,
    },
    #[error("tried to parse '{aperture_code_str}' as an aperture code (integer) greater than 9 but failed.")]
    ApertureCodeParseFailed{
        aperture_code_str: String,
    },
    #[error("tried to parse the definition of aperture '{aperture_code}' but failed.")]
    ParseApertureDefinitionBodyError{
        aperture_code: i32,
    },
    #[error("tried to parse the definition of aperture '{aperture_code}' but it already exists.")]
    ApertureDefinedTwice{
        aperture_code: i32,
    },
    #[error("tried to parse the definition of aperture, but it uses an unknown type: '{type_str}'.")]
    UnknownApertureType{
        type_str: String,
    },
    #[error("tried to parse the selection of aperture '{aperture_code}' but it is not defined.")]
    ApertureNotDefined{
        aperture_code: i32,
    },
    #[error("tried to parse coordinate number out of '{coord_num_str}' but failed. \
    This means a coordinate was captured, but could not be parsed as an i64.")]
    FailedToParseCoordinate{
        coord_num_str: String,
    },
    #[error("Operation statement called before format specification.")]
    OperationBeforeFormat{},
    #[error("Unable to parse file attribute (TF).")]
    FileAttributeParseError{},
    #[error("Unsupported Part type '{part_type}' in TF statement.")]
    UnsupportedPartType{
        part_type: String,
    },
    #[error("Unsupported Polarity type '{polarity_type}' in TF statement.")]
    UnsupportedPolarityType{
        polarity_type: String,
    },
    #[error("The AttributeName '{attribute_name}' is currently not supported for File Attributes.")]
    UnsupportedFileAttribute{
        attribute_name: String,
    },
    #[error("The File attribute '{file_attribute}' cannot be parsed.")]
    InvalidFileAttribute{
        file_attribute: String,
    },
    #[error("The Aperture attribute '{aperture_attribute}' cannot be parsed or is mis-formed.")]
    InvalidApertureAttribute{
        aperture_attribute: String,
    },
    #[error("The Aperture attribute '{aperture_attribute}' is not supported, but presumably valid.")]
    UnsupportedApertureAttribute{
        aperture_attribute: String,
    },
    #[error("Failed to parse delete attribute '{delete_attribute}'.")]
    InvalidDeleteAttribute{
        delete_attribute: String,
    },
}


impl GerberParserError{
    pub fn to_with_context(self, line: Option<String>, line_num: Option<usize>) -> GerberParserErrorWithContext{
        GerberParserErrorWithContext{
            error: self,
            line,
            line_num,
        }
    }
    pub fn as_with_context(&self, line: Option<String>, line_num: Option<usize>) -> GerberParserErrorWithContext{
        GerberParserErrorWithContext{
            error: self.clone(),
            line,
            line_num,
        }
    }
}


impl PartialEq for GerberParserError {
    /// Hack to simplify testing. Always returns false.
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(Error, Debug, PartialEq)]
pub struct GerberParserErrorWithContext {
    error: GerberParserError,
    line: Option<String>,
    line_num: Option<usize>,
}

impl std::fmt::Display for GerberParserErrorWithContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match (&self.line, &self.line_num) {
            (Some(line), Some(line_num)) => {
                write!(f, "Error: {}\nLine: '{}: {}'", self.error, line_num, line)
            }
            _ => {
                write!(f, "Error at unspecified line: {}", self.error)
            }
        }
    }
}