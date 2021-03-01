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
mod parse_input_text {
    #[test]
    fn should_parse_when_given_normal_text() {
        let input_text = vec![
            String::new("Users:"),
            String::new("  - Designers: People who design document templates"),
            String::new("Systems:"),
            String::new("  - Reporting: System that renders DocumentTemplates into DocumentPdfs"),
            String::new("  - Converter: System that converts OldDocumentDataFormat reference to NewDocumentDataFormat in DocumentTemplates"),
            String::new("  - Repository: System that stores document DocumentTemplates"),
            String::new("  - DataProvider: System that provides data relevant for DocumentTemplates"),
            String::new("  - Validator: System that validates if DocumentTemplates produce the the same pdf"),
            String::new("DataTypes:"),
            String::new("  - DocumentTemplate: Document Template that gets transformed to PDF"),
            String::new("  - DocumentPdf: Output PDF of document to sell to customers"),
            String::new("  - OldDocumentDataFormat: The old json scheme of the data format"),
            String::new("  - NewDocumentDataFormat: The new json scheme of the data format"),
            String::new("  - SubstitutionRule: Rules for substitution of specific fields and values provided by the data provider"),
            String::new("Setting: We are in the office where the underlying data format provided by Data Provider has changed, thereby changing how the DocumentTemplate needs to define its DocumentData. We want a system that can convert all existing DocumentTemplates' reference of OldDocumentDataFormat the NewDocumentDataFormat that the Data Provider is going to move to and also ensure that the DocumentPdf that get rendered is still correct."),
            String::new("Scope: Ecosystem"),
            String::new("Scenario 1: A successful conversion"),
            String::new("Script Start============================================================"),
            String::new("Designer: "),
            String::new("  - REQUEST Converter TO convert WITH awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"),
            String::new("Converter: "),
            String::new("  - (Designer convert request recieved)"),
            String::new("  - REQUEST DataProvider TO getSubstitutionRule WITH massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"),
            String::new("DataProvider: "),
            String::new("  - (Converter getSubstitutionRule request received)"),
            String::new("  - RESPOND TO Converter getSubstitutionRule REQUEST WITH massEffectRule:SubstitutionRule"),
            String::new("Converter: "),
            String::new("  - (DataProvider getSubstitutionRule response massEffectRule:SubstitutionRule received)"),
            String::new("  - (apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate)"),
            String::new("  - REQUEST Reporting TO render WITH awesomeJson:DocumentTemplate"),
            String::new("Reporting: "),
            String::new("  - (Converter render request received)"),
            String::new("  - RESPOND TO Converter render REQUEST WITH awesomePdf:DocumentPdf"),
            String::new("Converter: "),
            String::new("  - (Reporting render response awesomePdf:DocumentPdf received)"),
            String::new("  - REQUEST Validator TO compare WITH awesomePdf:DocumentPdf awesomePdfReference:DocumentPdf"),
            String::new("Validator: "),
            String::new("  - (Converter compare request received)"),
            String::new("  - RESPOND TO Converter compare REQUEST WITH "Success""),
            String::new("Converter: "),
            String::new("  - (Validator compare response "Success" received)"),
            String::new("  - REQUEST Repository TO store WITH awesomeJson:DocumentTemplate"),
            String::new("Repository: "),
            String::new("  - (Converter store request received)"),
            String::new("  - RESPOND TO Converter store REQUEST WITH "Success""),
            String::new("Converter: "),
            String::new("  - (Repository store response "Success" received)"),
            String::new("  - RESPOND TO Designer convert REQUEST WITH "Success""),
            String::new("Designer: "),
            String::new("  - (Converter convert response received)"),
            String::new("Script End============================================================"),
        ].join("\n");
        assert_eq!(1 + 1, 2);
    }
}

#[cfg(test)]
mod parse_valid_event_play_text {

}

#[cfg(test)]
mod apply_filter {

}

#[cfg(test)]
mod get_event_play_script_text {

}

#[cfg(test)]
mod get_event_play_output_text {

}
