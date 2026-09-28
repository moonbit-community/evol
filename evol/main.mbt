///|
async fn main {
  let cmd = @argparse.Command::Command(
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
  let matches = try cmd.parse() catch {
    err => {
      @stdio.stderr.write("evol: \{@debug.to_string(err)}\n")
      @sys.exit(1)
      return
    }
  } noraise {
    m => m
  }
  let toplevel = matches.flags.get("toplevel").unwrap_or(false)
  let src = match matches.values.get("input") {
    Some(paths) if paths.length() > 0 => @fs.read_file(paths[0]).text()
    _ => @stdio.stdin.read_all().text()
  }
  let out = try {
    if toplevel {
      @minifier.minify_with(src, true)
    } else {
      @minifier.minify(src)
    }
  } catch {
    @minifier.ParseError::Unexpected(msg) => {
      @stdio.stderr.write("evol: \{msg}\n")
      @sys.exit(1)
      return
    }
  } noraise {
    s => s
  }
  @stdio.stdout.write(out)
}
