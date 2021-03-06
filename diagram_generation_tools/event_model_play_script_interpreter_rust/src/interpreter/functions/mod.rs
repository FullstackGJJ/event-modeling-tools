use std::fs;
pub use super::types::{
    ActorScriptLine,
    Definition,
    EventPlayScript,
    EventPlayValidationError,
    FileParseError,
    Filter,
    FilterError,
    InputFile,
    InputText,
    OutputText,
    PlayParseError,
    RawEventPlayText,
    TextParseError,
    ValidEventPlayText,
    User,
    System,
    ScriptLine,
    DataType,
};

/*Public Functions*/

pub fn parse_input_file(input_file: InputFile) -> Result<InputText, FileParseError> {
    composed_parse_input_file_fn(fs::read_to_string, input_file)
}

pub fn parse_input_text(input_text: InputText) -> Result<RawEventPlayText, TextParseError> {
    let input_text = input_text
        .lines()
        .map(|x| x.trim().to_string())
        .collect::<Vec<String>>(); 

    Ok(RawEventPlayText {
        users_section: parse_declaration_section(&input_text, "Users:"),
        systems_section: parse_declaration_section(&input_text, "Systems:"),
        data_types_section: parse_declaration_section(&input_text, "DataTypes:"),
        setting_section: parse_description_section(&input_text, "Setting:"),
        scope_section: parse_description_section(&input_text, "Scope:"),
        scenario_section: parse_description_section(&input_text, "Scenario:"),
        script_section: parse_script_section(&input_text)
    })
}

pub fn validate_raw_event_play_text(raw_event_play_text: RawEventPlayText) -> Result<ValidEventPlayText, EventPlayValidationError> {
    let mut users_and_systems_definition: Vec<String> = vec![];
    users_and_systems_definition.append(&mut (raw_event_play_text.users_section.clone()));
    users_and_systems_definition.append(&mut (raw_event_play_text.systems_section.clone()));

    let undeclared_user_or_system_exists = raw_event_play_text.script_section.iter().any(|script_line| {
        let first_word = script_line.split(" ").next();
        match first_word {
            Some(word) => {
                if users_and_systems_definition.iter().any(|def| def.contains(&word.to_string())) {
                    false
                } else {
                    true
                }
            },
            None => true
        }
    });

    if undeclared_user_or_system_exists {
        Err("validate_raw_event_play_text unimplemented".to_string())
    } else {
        Ok(ValidEventPlayText {
            users_section: raw_event_play_text.users_section,
            systems_section: raw_event_play_text.systems_section,
            data_types_section: raw_event_play_text.data_types_section,
            setting_section: raw_event_play_text.setting_section,
            scope_section: raw_event_play_text.scope_section,
            scenario_section: raw_event_play_text.scenario_section,
            script_section: raw_event_play_text.script_section
        })
    }

}

pub fn parse_valid_event_play_text(valid_event_play_text: ValidEventPlayText) -> Result<EventPlayScript, PlayParseError> {
    let script: Vec<ActorScriptLine> = valid_event_play_text.script_section
        .iter()
        .map(parse_actor_script_line)
        .collect();

    Ok(EventPlayScript {
        users: distill_definitions(&valid_event_play_text.users_section),
        systems: distill_definitions(&valid_event_play_text.systems_section),
        data_types: distill_definitions(&valid_event_play_text.data_types_section),
        setting: valid_event_play_text.setting_section,
        scope: valid_event_play_text.scope_section,
        scenario: valid_event_play_text.scenario_section,
        script
    })
}

pub fn apply_filter(event_play_script: EventPlayScript, filter: Filter) -> Result<EventPlayScript, FilterError> {
    Err("apply_filter unimplemented".to_string())
}

pub fn get_event_play_script_text(event_play_script: EventPlayScript) -> ValidEventPlayText {
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

/*Private Functions*/

fn parse_description_section(input_text: &Vec<String>, description: &str) -> String {
    input_text.iter().filter(|line| line.starts_with(description))
        .fold(String::new(), |acc, line| {
            let mut acc = acc;
            acc.push_str(line.replace(description, "").trim());
            acc
        })
}

fn parse_declaration_section(input_text: &Vec<String>, section_filter: &str) -> Vec<String> {
    let (return_vec, _) = input_text
        .iter()
        .fold((Vec::<String>::new(), false), |acc, line| {
            let (vec, in_user_section_block) = acc;
            match (line.as_str(), in_user_section_block) {
                (possible_section, false) if section_filter == possible_section => (vec, true),
                (_, true) => {
                    match line.chars().next() {
                        Some('-') => {
                            let mut vec: Vec<String> = vec;
                            vec.push(line.replace("-", "").trim().to_string());
                            (vec, true)
                        },
                        _ => (vec, false)
                    }
                },
                (_, false) => (vec, false)
            }
        });
    return_vec
}

fn parse_script_section(input_text: &Vec<String>) -> Vec<String> {
    let (script_section, _, _) = input_text
        .iter()
        .fold((Vec::<String>::new(), String::from(""), false), |acc, line| {
            let (vec, current_actor, in_script_section_block) = acc;
            match (line.as_str(), in_script_section_block) {
                ("Script Start============================================================", _) => (vec, current_actor, true),
                ("Script End============================================================", _) => (vec, current_actor, false),
                (_, true) => {
                    match line.chars().next() {
                        Some('-') => {
                            let mut vec = vec;
                            vec.push(line.replace("-", current_actor.as_str()).trim().to_string());
                            (vec, current_actor, true)
                        },
                        Some(_) if line.contains(":") => {
                            (vec, line.replace(":", "").trim().to_string(), true)
                        },
                        Some(_) | None => (vec, current_actor, in_script_section_block)
                        //None => (vec, current_actor, in_script_section_block)
                    }
                },
                (_, false) => (vec, current_actor, in_script_section_block)
            }
        });

    script_section
}


fn composed_parse_input_file_fn(file_reading_fn: fn(String) -> Result<String, std::io::Error>, input_file: InputFile) -> Result<InputText, FileParseError> {
    match file_reading_fn(input_file) {
        Ok(file_text) => Ok(file_text),
        Err(error) => Err(format!("File parsing error: {:?}", error))
    }
}

fn parse_actor_script_line(line: &String) -> ActorScriptLine {
    let line = line.split(" ").collect::<Vec<&str>>();
    let (actor_vec, remaining_vec) = line.split_at(1);
    let script_line = remaining_vec.join(" ");
    let actor = (&actor_vec[0]).to_string();
    match script_line.chars().next() {
        Some('(') => ActorScriptLine { actor, script_line: ScriptLine::Event(script_line.replace("(", "").replace(")", "").to_string()) },
        Some(_) => ActorScriptLine { actor, script_line: ScriptLine::Action(script_line.to_string()) },
        None => ActorScriptLine { actor, script_line: ScriptLine::Event(String::new()) }
    }
}

fn distill_definitions(section: &Vec<String>) -> Vec<Definition> {
    section.iter().map(|line| {
            let mut split_by_colon_iter = line.split(":");
            match split_by_colon_iter.next() {
                Some(first_word) => {
                    match split_by_colon_iter.next() {
                        Some(second_word) => Definition { name: first_word.trim().to_string(), description: second_word.trim().to_string() },
                        None => Definition { name: first_word.to_string(), description: String::new() }
                    }
                },
                None => Definition { name: String::new(), description: String::new() }
            }
        })
        .collect()
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

        let expected_result = RawEventPlayText {
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
            Ok(raw_event_play_text) => assert_eq!(raw_event_play_text, expected_result),
            Err(error) => panic!("parse_input_text did not succeed in parsing")
        }
    }
}

#[cfg(test)]
mod validate_raw_event_play_text {
    extern crate pretty_assertions;

    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn should_correctly_return_valid_event_play_text_when_given_valid_raw_event_play_text() {
        let input = RawEventPlayText {
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

        let test_result = validate_raw_event_play_text(input);
        match test_result {
            Ok(event_play_script) => assert_eq!(event_play_script, expected_result),
            Err(error) => panic!("validate_raw_event_play_text detected an error: {}", error)
        }
    }
}

#[cfg(test)]
mod parse_valid_event_play_text {
    extern crate pretty_assertions;

    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn should_correctly_return_event_play_script_when_given_valid_event_play_text() {
        let input = ValidEventPlayText {
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

        let expected_result = EventPlayScript {
            users: vec![
                User { name: String::from("Designers") , description :String::from("People who design document templates") }
            ],
            systems: vec![
                System { name: String::from("Reporting"), description: String::from("System that renders DocumentTemplates into DocumentPdfs") },
                System { name: String::from("Converter"), description: String::from("System that converts OldDocumentDataFormat reference to NewDocumentDataFormat in DocumentTemplates") },
                System { name: String::from("Repository"), description: String::from("System that stores document DocumentTemplates") },
                System { name: String::from("DataProvider"), description: String::from("System that provides data relevant for DocumentTemplates") },
                System { name: String::from("Validator"), description: String::from("System that validates if DocumentTemplates produce the the same pdf") }
            ],
            data_types: vec![
                DataType { name: String::from("DocumentTemplate"), description: String::from("Document Template that gets transformed to PDF") },
                DataType { name: String::from("DocumentPdf"), description: String::from("Output PDF of document to sell to customers") },
                DataType { name: String::from("OldDocumentDataFormat"), description: String::from("The old json scheme of the data format") },
                DataType { name: String::from("NewDocumentDataFormat"), description: String::from("The new json scheme of the data format") },
                DataType { name: String::from("SubstitutionRule"), description: String::from("Rules for substitution of specific fields and values provided by the data provider") },
            ],
            setting: String::from("We are in the office where the underlying data format provided by Data Provider has changed, thereby changing how the DocumentTemplate needs to define its DocumentData. We want a system that can convert all existing DocumentTemplates' reference of OldDocumentDataFormat the NewDocumentDataFormat that the Data Provider is going to move to and also ensure that the DocumentPdf that get rendered is still correct."),
            scope: String::from("Ecosystem"),
            scenario: String::from("A successful conversion"),
            script: vec![
                ActorScriptLine { 
                    actor: String::from("Designer"),
                    script_line: ScriptLine::Action(String::from("REQUEST Converter TO convert WITH awesomeJson:DocumentTemplate massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat"))
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Event(String::from("Designer convert request recieved")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Action(String::from("REQUEST DataProvider TO getSubstitutionRule WITH massEffect1:OldDocumentDataFormat massEffect2:NewDocumentDataFormat")) 
                },
                ActorScriptLine { 
                    actor: String::from("DataProvider"),
                    script_line: ScriptLine::Event(String::from("Converter getSubstitutionRule request received")) 
                },
                ActorScriptLine { 
                    actor: String::from("DataProvider"),
                    script_line: ScriptLine::Action(String::from("RESPOND TO Converter getSubstitutionRule REQUEST WITH massEffectRule:SubstitutionRule")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Event(String::from("DataProvider getSubstitutionRule response massEffectRule:SubstitutionRule received")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Event(String::from("apply massEffectRule:SubstitutionRule to awesomeJson:DocumentTemplate")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Action(String::from("REQUEST Reporting TO render WITH awesomeJson:DocumentTemplate")) 
                },
                ActorScriptLine { 
                    actor: String::from("Reporting"),
                    script_line: ScriptLine::Event(String::from("Converter render request received")) 
                },
                ActorScriptLine { 
                    actor: String::from("Reporting"),
                    script_line: ScriptLine::Action(String::from("RESPOND TO Converter render REQUEST WITH awesomePdf:DocumentPdf")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Event(String::from("Reporting render response awesomePdf:DocumentPdf received")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Action(String::from("REQUEST Validator TO compare WITH awesomePdf:DocumentPdf awesomePdfReference:DocumentPdf")) 
                },
                ActorScriptLine { 
                    actor: String::from("Validator"),
                    script_line: ScriptLine::Event(String::from("Converter compare request received")) 
                },
                ActorScriptLine { 
                    actor: String::from("Validator"),
                    script_line: ScriptLine::Action(String::from("RESPOND TO Converter compare REQUEST WITH 'Success'")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Event(String::from("Validator compare response 'Success' received")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Action(String::from("REQUEST Repository TO store WITH awesomeJson:DocumentTemplate")) 
                },
                ActorScriptLine { 
                    actor: String::from("Repository"),
                    script_line: ScriptLine::Event(String::from("Converter store request received")) 
                },
                ActorScriptLine { 
                    actor: String::from("Repository"),
                    script_line: ScriptLine::Action(String::from("RESPOND TO Converter store REQUEST WITH 'Success'")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Event(String::from("Repository store response 'Success' received")) 
                },
                ActorScriptLine { 
                    actor: String::from("Converter"),
                    script_line: ScriptLine::Action(String::from("RESPOND TO Designer convert REQUEST WITH 'Success'")) 
                },
                ActorScriptLine { 
                    actor: String::from("Designer"),
                    script_line: ScriptLine::Event(String::from("Converter convert response received")) 
                },
            ]
        };

        let test_result = parse_valid_event_play_text(input);
        match test_result {
            Ok(event_play_script) => assert_eq!(event_play_script, expected_result),
            Err(error) => panic!("parse_valid_event_play_text detected an error: {}", error)
        }
    }
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
