///|
async fn main {
  let matches = cli_command().parse()
  let input = matches.values.get("input").bind(paths => paths.get(0))
  let requested_output = matches.values
    .get("output")
    .bind(paths => paths.get(0))
  guard output_path(input, requested_output) is Some(path) else {
    return fail("specify --output when reading stdin")
  }
  let rename_top_level = !matches.flags.get("top").unwrap_or(false)
  let source = read_source(input)
  let output = @minifier.minify_with(source, rename_top_level)
  guard path != "-" else { return @stdio.stdout.write(output) }
  @fs.write_file(path, output)
}

///|
fn cli_command() -> @argparse.Command {
  @argparse.Command::Command(
    "evol",
    about="Minify a JavaScript file",
    version="0.2.0",
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
fn output_path(input : String?, requested : String?) -> String? {
  guard requested is Some(path) && path.length() > 0 else {
    return input.map(minified_path)
  }
  Some(path)
}

///|
fn minified_path(input : String) -> String {
  lexmatch input {
    re"^" + (re".+" as stem) + re"\.[^./\\]+$" => "\{stem}.min.js"
    _ => "\{input}.min.js"
  }
}

///|
async fn read_source(input : String?) -> String {
  guard input is Some(path) else { return @stdio.stdin.read_all().text() }
  @fs.read_file(path).text()
}
