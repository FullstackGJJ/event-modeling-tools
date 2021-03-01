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
    composed_parse_input_file_fn(fs::read_to_string, input_file)
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
        users_section: vec![ String::from("") ],
        systems_section: vec![ String::from("") ],
        data_types_section: vec![ String::from("") ],
        setting_section: String::from(""),
        scope_section: String::from(""),
        scenario_section: String::from(""),
        script_section: vec![ String::from("") ]
    }
}

pub fn get_event_play_output_text(valid_event_play_text: ValidEventPlayText) -> OutputText {
    "get_event_play_output_text does not do anything".to_string()
}

fn composed_parse_input_file_fn(file_reading_fn: fn(String) -> Result<String, std::io::Error>, input_file: InputFile) -> Result<InputText, FileParseError> {
    match file_reading_fn(input_file) {
        Ok(file_text) => Ok(file_text),
        Err(error) => Err(format!("File parsing error: {:?}", error))
    }
}

/*==============================Unit Tests==============================*/

#[cfg(test)]
mod composed_parse_input_file_fn {
    use super::*;
}

#[cfg(test)]
mod parse_input_text {
    extern crate pretty_assertions;

    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn should_parse_when_given_normal_text() {
        let input_text = vec![
            String::from("Users:"),
            String::from("  - Designers: People who design document templates"),
            String::from("Systems:"),
            String::from("  - Reporting: System that renders DocumentTemplates into DocumentPdfs"),
            String::from("  - Converter: System that converts OldDocumentDataFormat reference to NewDocumentDataFormat in DocumentTemplates"),
            String::from("  - Repository: System that stores document DocumentTemplates"),
            String::from("  - DataProvider: System that provides data relevant for DocumentTemplates"),
            String::from("  - Validator: System that validates if DocumentTemplates produce the the same pdf"),
            String::from("DataTypes:"),
            String::from("  - DocumentTemplate: Document Template that gets transformed to PDF"),
            String::from("  - DocumentPdf: Output PDF of document to sell to customers"),
            String::from("  - OldDocumentDataFormat: The old json scheme of the data format"),
            String::from("  - NewDocumentDataFormat: The new json scheme of the data format"),
            String::from("  - SubstitutionRule: Rules for substitution of specific fields and values provided by the data provider"),
            String::from("Setting: We are in the office where the underlying data format provided by Data Provider has changed, thereby changing how the DocumentTemplate needs to define its DocumentData. We want a system that can convert all existing DocumentTemplates' reference of OldDocumentDataFormat the NewDocumentDataFormat that the Data Provider is going to move to and also ensure that the DocumentPdf that get rendered is still correct."),
            String::from("Scope: Ecosystem"),
            String::from("Scenario: A successful conversion"),
            String::from("Script Start============================================================"),
            String::from("Designer: "),
            String::from("  - REQUEST Converter TO convert WITH awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"),
            String::from("Converter: "),
            String::from("  - (Designer convert request recieved)"),
            String::from("  - REQUEST DataProvider TO getSubstitutionRule WITH massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"),
            String::from("DataProvider: "),
            String::from("  - (Converter getSubstitutionRule request received)"),
            String::from("  - RESPOND TO Converter getSubstitutionRule REQUEST WITH massEffectRule:SubstitutionRule"),
            String::from("Converter: "),
            String::from("  - (DataProvider getSubstitutionRule response massEffectRule:SubstitutionRule received)"),
            String::from("  - (apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate)"),
            String::from("  - REQUEST Reporting TO render WITH awesomeJson:DocumentTemplate"),
            String::from("Reporting: "),
            String::from("  - (Converter render request received)"),
            String::from("  - RESPOND TO Converter render REQUEST WITH awesomePdf:DocumentPdf"),
            String::from("Converter: "),
            String::from("  - (Reporting render response awesomePdf:DocumentPdf received)"),
            String::from("  - REQUEST Validator TO compare WITH awesomePdf:DocumentPdf awesomePdfReference:DocumentPdf"),
            String::from("Validator: "),
            String::from("  - (Converter compare request received)"),
            String::from("  - RESPOND TO Converter compare REQUEST WITH 'Success'"),
            String::from("Converter: "),
            String::from("  - (Validator compare response 'Success' received)"),
            String::from("  - REQUEST Repository TO store WITH awesomeJson:DocumentTemplate"),
            String::from("Repository: "),
            String::from("  - (Converter store request received)"),
            String::from("  - RESPOND TO Converter store REQUEST WITH 'Success'"),
            String::from("Converter: "),
            String::from("  - (Repository store response 'Success' received)"),
            String::from("  - RESPOND TO Designer convert REQUEST WITH 'Success'"),
            String::from("Designer: "),
            String::from("  - (Converter convert response received)"),
            String::from("Script End============================================================"),
        ].join("\n");

        let expected_result = ValidEventPlayText {
            users_section: vec![
                String::from("Designers: People who design document templates"),
            ],
            systems_section: vec![
                String::from("Reporting: System that renders DocumentTemplates into DocumentPdfs"),
                String::from("Converter: System that converts OldDocumentDataFormat reference to NewDocumentDataFormat in DocumentTemplates"),
                String::from("Repository: System that stores document DocumentTemplates"),
                String::from("DataProvider: System that provides data relevant for DocumentTemplates"),
                String::from("Validator: System that validates if DocumentTemplates produce the the same pdf"),
            ],
            data_types_section: vec![
                String::from("DocumentTemplate: Document Template that gets transformed to PDF"),
                String::from("DocumentPdf: Output PDF of document to sell to customers"),
                String::from("OldDocumentDataFormat: The old json scheme of the data format"),
                String::from("NewDocumentDataFormat: The new json scheme of the data format"),
                String::from("SubstitutionRule: Rules for substitution of specific fields and values provided by the data provider"),
            ],
            setting_section: String::from("We are in the office where the underlying data format provided by Data Provider has changed, thereby changing how the DocumentTemplate needs to define its DocumentData. We want a system that can convert all existing DocumentTemplates' reference of OldDocumentDataFormat the NewDocumentDataFormat that the Data Provider is going to move to and also ensure that the DocumentPdf that get rendered is still correct."),
            scope_section: String::from("Ecosystem"),
            scenario_section: String::from("A successful conversion"),
            script_section: vec![
                String::from("Designer REQUEST Converter TO convert WITH awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"),
                String::from("Converter (Designer convert request recieved)"),
                String::from("Converter REQUEST DataProvider TO getSubstitutionRule WITH massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"),
                String::from("DataProvider (Converter getSubstitutionRule request received)"),
                String::from("DataProvider RESPOND TO Converter getSubstitutionRule REQUEST WITH massEffectRule:SubstitutionRule"),
                String::from("Converter (DataProvider getSubstitutionRule response massEffectRule:SubstitutionRule received)"),
                String::from("Converter (apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate)"),
                String::from("Converter REQUEST Reporting TO render WITH awesomeJson:DocumentTemplate"),
                String::from("Reporting (Converter render request received)"),
                String::from("Reporting RESPOND TO Converter render REQUEST WITH awesomePdf:DocumentPdf"),
                String::from("Converter (Reporting render response awesomePdf:DocumentPdf received)"),
                String::from("Converter REQUEST Validator TO compare WITH awesomePdf:DocumentPdf awesomePdfReference:DocumentPdf"),
                String::from("Validator (Converter compare request received)"),
                String::from("Validator RESPOND TO Converter compare REQUEST WITH 'Success'"),
                String::from("Converter (Validator compare response 'Success' received)"),
                String::from("Converter REQUEST Repository TO store WITH awesomeJson:DocumentTemplate"),
                String::from("Repository (Converter store request received)"),
                String::from("Repository RESPOND TO Converter store REQUEST WITH 'Success'"),
                String::from("Converter (Repository store response 'Success' received)"),
                String::from("Converter RESPOND TO Designer convert REQUEST WITH 'Success'"),
                String::from("Designer (Converter convert response received)"),
            ]
        };

        let test_result = parse_input_text(input_text);
        match test_result {
            Ok(valid_event_play_text) => assert_eq!(valid_event_play_text, expected_result),
            Err(error) => panic!("parse_input_text did not succeed in parsing")
        }
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
