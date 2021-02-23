use quicli::prelude::*;
use structopt::StructOpt;


/// Load event model play script to generate specified output
#[derive(Debug, StructOpt)]
struct Cli {

    /// The file to read
    file: String,

    /// Which perspective to filter
    #[structopt(long = "filter", short = "f", default_value = "")]
    filter: String,

    #[structopt(flatten)]
    verbosity: Verbosity,
}

fn main() -> CliResult {
    let args = Cli::from_args();
    read_file(&args.file)?
        .lines()
        .for_each(|line| println!("{}", line));
    Ok(())
}
