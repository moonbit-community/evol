///|
async fn main {
  let command = @argparse.Command::Command(
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
  let matches = command.parse() catch {
    error => {
      fail(@debug.to_string(error))
      return
    }
  }
  let top_level = !matches.flags.get("no-toplevel").unwrap_or(false)
  let internal_properties : Array[String] = []
  for group in matches.values.get("mangle-props").unwrap_or([]) {
    for name in group.split(",") {
      internal_properties.push(name.trim().to_owned())
    }
  }
  let source = match matches.values.get("input") {
    Some(paths) if paths.length() > 0 => @fs.read_file(paths[0]).text()
    _ => @stdio.stdin.read_all().text()
  }
  let output = try {
    if top_level {
      @minifier.minify(source, internal_properties~)
    } else {
      @minifier.minify_with(source, false, internal_properties~)
    }
  } catch {
    error => {
      fail(Show::to_string(error))
      return
    }
  }
  @stdio.stdout.write(output)
}

///|
async fn fail(message : String) -> Unit {
  @stdio.stderr.write("evol: \{message}\n")
  @sys.exit(1)
}
