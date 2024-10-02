use regex::Regex;
use thiserror::Error;

#[repr(u8)]
#[derive(Error, Debug)]
pub enum GerberParserError {
    #[error("Document included a line that isn't valid: {0}")]
    UnknownCommand(String),
    #[error("Document included a line that isn't supported: {0}")]
    UnsupportedCommand(String),
    #[error("Missing M02 statement at end of file")]
    NoEndOfFile,
    #[error("Command was uniquely identified, but did not match regex: {0}, {1}")]
    NoRegexMatch(String, Regex),
    #[error("Command was uniquely identified, and matched expected regex, but did not contain the expected capture(s): {0}, {1}")]
    MissingRegexCapture(String, Regex),
    #[error("After gerber doc was already assigned a name, another name command was found. Line containing second name set: {0}")]
    TriedToSetImageNameTwice(String),
    #[error("After gerber doc was already assigned a unit, another unit command was found. Line containing second unit set: {0}")]
    TriedToSetUnitsTwice(String),
    #[error("After gerber doc was already assigned a format specification, another format specification command was found. Line containing second format specification set: {0}")]
    TriedToFormatTwice(String),
    #[error("Set unit command included unrecognized units: {0}")]
    InvalidUnitFormat(String),
    #[error("Error parsing format spec line. Looking for 2 digits but found 1 or none: {0}\nexpected something like \'%FSLAX23Y23*%\'")]
    ParseFormatErrorWrongNumDigits(String),
    #[error("format spec integer value must be between 1 and 6. Found {0}.")]
    ParseFormatErrorInvalidDigit(u8),
    #[error("Error parsing char as base 10 digit: '{0}'")]
    ParseDigitError(char),
    #[error("tried to parse '{0}' as an aperture code (integer) greater than 9 but failed")]
    ApertureCodeParseFailed(String),
    #[error("tried to parse the definition of aperture '{0}' but failed. Line: {1}")]
    ParseApertureDefinitionBodyError(i32, String),
    #[error("tried to parse the definition of aperture '{0}' but it already exists. Line: {1}")]
    ApertureDefinedTwice(i32, String),
    #[error("tried to parse the definition of aperture, but it uses an unknown type: '{0}'. Line: {1}")]
    UnknownApertureType(String, String),
    #[error("tried to parse the selection of aperture '{0}' but it is not defined. Line: {1}")]
    ApertureNotDefined(i32, String),
    #[error("tried to parse coordinate out of '{0}' but failed. This means a coordinate was captured, but could not be parsed as an i64")]
    FailedToParseCoordinate(String),
    #[error("Operation statement called before format specification. Line: {0}")]
    OperationBeforeFormat(String),
    #[error("Unable to parse file attribute (TF). Line: {0}")]
    FileAttributeParseError(String),
    #[error("Unsupported Part type '{0}' in TF statement")]
    UnsupportedPartType(String),
    #[error("Unsupported Polarity type '{0}' in TF statement")]
    UnsupportedPolarityType(String),
    #[error("The AttributeName '{0}' is currently not supported for File Attributes")]
    UnsupportedFileAttribute(String),
    #[error("The Attribute '{0}' cannot be parsed")]
    InvalidFileAttribute(String),
}