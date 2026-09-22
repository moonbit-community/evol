///|
fn name_from_index(n : Int) -> String {
  let first = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"
  let rest = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$0123456789"
  let buf : Array[Char] = []
  buf.push(first.get_char(n % 54).unwrap())
  let mut m = n / 54
  while m > 0 {
    buf.push(rest.get_char((m - 1) % 64).unwrap())
    m = (m - 1) / 64
  }
  let sb = StringBuilder()
  let mut i = buf.length() - 1
  while i >= 0 {
    sb.write_char(buf[i])
    i = i - 1
  }
  sb.to_string()
}

///|
fn reserved_words() -> Array[String] {
  [
    "break", "case", "catch", "class", "const", "continue", "debugger", "default",
    "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for",
    "function", "if", "import", "in", "instanceof", "new", "null", "return", "super",
    "switch", "this", "throw", "true", "try", "typeof", "var", "void", "while", "with",
    "yield", "let", "static", "await", "async", "get", "set", "of", "as", "from",
    "arguments", "eval", "undefined", "NaN", "Infinity",
  ]
}

///|
fn sym_name(prog : Program, sym : Int) -> String {
  let s = prog.symbols[sym]
  prog.src_span(s.name_start, s.name_end)
}

///|
fn mangle(prog : Program, rename_top_level : Bool) -> Unit {
  for node in prog.nodes {
    if node.kind == With {
      return
    }
  }
  let reserved : Map[String, Bool] = Map([])
  for kw in reserved_words() {
    reserved[kw] = true
  }
  for r in prog.refs {
    if r.sym < 0 {
      reserved[prog.src_span(r.name_start, r.name_end)] = true
    }
  }
  for i in 0..<prog.symbols.length() {
    let s = prog.symbols[i]
    if (s.flags & sym_must_not_rename) != 0 {
      reserved[sym_name(prog, i)] = true
    }
    if s.scope == 0 && !rename_top_level {
      reserved[sym_name(prog, i)] = true
    }
  }
  let targets : Array[Int] = []
  for i in 0..<prog.symbols.length() {
    let s = prog.symbols[i]
    if (s.flags & sym_must_not_rename) != 0 {
      continue
    }
    if s.scope == 0 && !rename_top_level {
      continue
    }
    targets.push(i)
  }
  targets.sort_by((a, b) => {
    prog.symbols[b].use_count - prog.symbols[a].use_count
  })
  let mut counter = 0
  for idx in targets {
    let mut candidate = name_from_index(counter)
    counter = counter + 1
    while reserved.contains(candidate) {
      candidate = name_from_index(counter)
      counter = counter + 1
    }
    prog.symbols[idx].new_name = candidate
    reserved[candidate] = true
  }
}
