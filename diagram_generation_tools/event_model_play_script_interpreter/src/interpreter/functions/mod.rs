use std::fs;
pub use super::types::{
    EventPlayScript,
    FileParseError,
    Filter,
    FilterError,
    InputFile,
    InputText,
    OutputText,
    PlayParseError,
    TextParseError,
    ValidEventPlayText,
};

pub fn parse_input_file(input_file: InputFile) -> Result<InputText, FileParseError> {
    match fs::read_to_string(input_file) {
        Ok(file_text) => Ok(file_text),
        Err(error) => Err("File parsing error".to_string())
    }
}

pub fn parse_input_text(input_text: InputText) -> Result<ValidEventPlayText, TextParseError> {
    Err("parse_input_text unimplemented".to_string())
}

pub fn parse_valid_event_play_text(valid_event_play_text: ValidEventPlayText) -> Result<EventPlayScript, PlayParseError> {
    Err("parse_valid_event_play_text unimplemented".to_string())
}

pub fn apply_filter(event_play_script: EventPlayScript, filter: Filter) -> Result<EventPlayScript, FilterError> {
    Err("apply_filter unimplemented".to_string())
}

pub fn get_event_play_script_text(event_play_pcript: EventPlayScript) -> ValidEventPlayText {
    ValidEventPlayText {
        users_section: vec!["".to_string()],
        systems_section: vec!["".to_string()],
        data_types_section: vec!["".to_string()],
        setting_section: vec!["".to_string()],
        scope_section: vec!["".to_string()],
        script_section: vec!["".to_string()]
    }
}

pub fn get_event_play_output_text(valid_event_play_text: ValidEventPlayText) -> OutputText {
    "get_event_play_output_text does not do anything".to_string()
}

#[cfg(test)]
mod parse_input_text_tests {
    super::*;

    #[test]
    fn should_parse_when_given_normal_text() {
        assert_eq!(1 + 1, 2);
    }
}
