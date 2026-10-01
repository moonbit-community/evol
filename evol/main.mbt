///|
async fn main {
  let matches = cli_command().parse() catch {
    error => {
      fail(@debug.to_string(error))
      return
    }
  }
  let top_level = !matches.flags.get("no-toplevel").unwrap_or(false)
  let internal_properties = requested_properties(
    matches.values.get("mangle-props").unwrap_or([]),
  )
  let source = read_source(matches)
  let output = @minifier.minify_with(source, top_level, internal_properties~) catch {
    error => {
      fail(Show::to_string(error))
      return
    }
  }
  @stdio.stdout.write(output)
}

///|
fn cli_command() -> @argparse.Command {
  @argparse.Command::Command(
    "evol",
    about="Minify a JavaScript file",
    version="0.1.0",
    flags=[
      @argparse.FlagArg::FlagArg(
        "no-toplevel",
        long="no-toplevel",
        about="keep top-level binding names",
      ),
    ],
    options=[
      @argparse.OptionArg::OptionArg(
        "mangle-props",
        long="mangle-props",
        action=@argparse.OptionAction::Append,
        about="comma-separated internal ASCII property names to rename",
      ),
    ],
    positionals=[
      @argparse.PositionArg::PositionArg(
        "input",
        about="input file (reads stdin when omitted)",
        num_args=@argparse.ValueRange::ValueRange(lower=0, upper=1),
      ),
    ],
  )
}

///|
fn requested_properties(groups : Array[String]) -> Array[String] {
  let properties : Array[String] = []
  for group in groups {
    for name in group.split(",") {
      properties.push(name.trim().to_owned())
    }
  }
  properties
}

///|
async fn read_source(matches : @argparse.Matches) -> String {
  guard matches.values.get("input") is Some(paths) && paths.length() > 0 else {
    return @stdio.stdin.read_all().text()
  }
  @fs.read_file(paths[0]).text()
}

///|
async fn fail(message : String) -> Unit {
  @stdio.stderr.write("evol: \{message}\n")
  @sys.exit(1)
}
