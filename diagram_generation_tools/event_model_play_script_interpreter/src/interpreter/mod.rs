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

pub fn parse_input_file() {

}
