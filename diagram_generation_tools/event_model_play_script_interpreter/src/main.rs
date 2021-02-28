use quicli::prelude::*;
use structopt::StructOpt;

use event_model_play_script_interpreter::interpreter::functions as interpreter;


/// Load event model play script to generate specified output
#[derive(Debug, StructOpt)]
struct Cli {

    /// The file to read
    file: String,

    /// Which perspectives to filter
    #[structopt(long = "filters", short = "f", default_value = "")]
    filter: String,

    #[structopt(flatten)]
    verbosity: Verbosity,
}

fn main() -> CliResult {
    let args = Cli::from_args();
    let get_filtered_play_script_result = interpreter::parse_input_file(args.file.to_string())
        .and_then(|input_text| interpreter::parse_input_text(input_text))
        .and_then(|valid_play_text| interpreter::parse_valid_event_play_text(valid_play_text))
        .and_then(|event_play_script| interpreter::apply_filter(event_play_script, args.filter.to_string()));

    match get_filtered_play_script_result {
        Ok(filtered_event_play_script) => {
            let script_text = interpreter::get_event_play_script_text(filtered_event_play_script);
            interpreter::get_event_play_output_text(script_text)
                .lines()
                .for_each(|line| println!("{}", line));
        },
        Err(err) => println!("{}", err)
    };

    Ok(())
}
