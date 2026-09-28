///|
async fn main {
  let command = @argparse.Command::Command(
    "evol",
    about="Minify a JavaScript file",
    version="0.1.0",
    flags=[
      @argparse.FlagArg::FlagArg(
        "toplevel",
        long="toplevel",
        about="also rename top-level bindings",
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
  let top_level = matches.flags.get("toplevel").unwrap_or(false)
  let source = match matches.values.get("input") {
    Some(paths) if paths.length() > 0 => @fs.read_file(paths[0]).text()
    _ => @stdio.stdin.read_all().text()
  }
  let output = try {
    if top_level {
      @minifier.minify_with(source, true)
    } else {
      @minifier.minify(source)
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
