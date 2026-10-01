///|
async fn main {
  let matches = cli_command().parse() catch {
    error => return fail(@debug.to_string(error))
  }
  let rename_top_level = !matches.flags.get("top").unwrap_or(false)
  let source = read_source(matches)
  let output = @minifier.minify_with(source, rename_top_level) catch {
    error => return fail(Show::to_string(error))
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
        "top",
        long="top",
        about="keep top-level binding names",
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
