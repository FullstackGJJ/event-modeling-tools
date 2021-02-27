struct Definition {
    name: String,
    description: String
}

type User = Definition;

type System = Definition;

type DataType = Definition;

type Setting = String;

type Scope = String;

type Actor = String;

enum ScriptLine {
    Event,
    Action
}

struct ActorScriptLine {
    actor: Actor,
    script_line: ScriptLine
}

type Script = Vec<ActorScriptLine>;

struct EventPlayScript {
    users: Vec<User>,
    systems: Vec<System>,
    data_types: Vec<DataType>,
    setting: Setting,
    scope: Scope,
    script: Script
}

type InputFile = String;

type Filter = String;

type UsersSection = Vec<String>;

type SystemsSection = Vec<String>;

type DataTypesSection = Vec<String>;

type SettingSection = Vec<String>;

type ScopeSection = Vec<String>;

type ScriptSection = Vec<String>;

type FileParseError = String;

type PlayParseError = String;

type FilterError = String;

struct ValidEventPlayText {
    users_section = UsersSection,
    systems_section = SystemsSection,
    data_types_section = DataTypesSection,
    setting_section = SettingSection,
    scope_section = ScopeSection,
    script_section = ScriptSection
}

pub fn parse_input_file(inputFile: InputFile) -> Result<ValidEventPlayText, FileParseError> {

}

pub fn parse_valid_event_play_text :: ValidEventPlayText -> Result(EventPlayScript, PlayParseError) {

}

pub fn apply_filter :: EventPlayScript -> Filter -> Result(EventPlayScript, FilterError) {

}

pub fn get_event_play_script_text :: EventPlayScript -> ValidEventPlayText {

}
