///|
async fn main {
  let matches = cli_command().parse() catch {
    error => return fail(@debug.to_string(error))
  }
  guard output_path(matches) is Some(path) else {
    return fail("specify --output when reading stdin")
  }
  let rename_top_level = !matches.flags.get("top").unwrap_or(false)
  let source = read_source(matches)
  let output = @minifier.minify_with(source, rename_top_level) catch {
    error => return fail(Show::to_string(error))
  }
  guard path != "-" else { return @stdio.stdout.write(output) }
  @fs.write_file(path, output)
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
    options=[
      @argparse.OptionArg::OptionArg(
        "output",
        long="output",
        about="output file (- for stdout)",
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
fn output_path(matches : @argparse.Matches) -> String? {
  guard matches.values.get("output") is Some(paths) &&
    paths.length() > 0 &&
    paths[0].length() > 0 else {
    guard matches.values.get("input") is Some(inputs) && inputs.length() > 0 else {
      return None
    }
    return Some(minified_path(inputs[0]))
  }
  Some(paths[0])
}

///|
fn minified_path(input : String) -> String {
  lexmatch input {
    re"^" + (re".+" as stem) + re"\.[^./\\]+$" => "\{stem}.min.js"
    _ => "\{input}.min.js"
  }
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
