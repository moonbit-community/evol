///|
fn name_from_index(n : Int) -> String {
  let first = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"
  let rest = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$0123456789"
  let sb = StringBuilder()
  sb.write_char(first.get_char(n % 54).unwrap())
  for remaining = n / 54; remaining > 0; remaining = (remaining - 1) / 64 {
    sb.write_char(rest.get_char((remaining - 1) % 64).unwrap())
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
    "arguments", "eval", "undefined", "NaN", "Infinity", "implements", "interface",
    "package", "private", "protected", "public",
  ]
}

///|
fn sym_name(prog : Program, sym : Int) -> String {
  prog.names[prog.symbols[sym].name]
}

///|
fn mangle(prog : Program, rename_top_level : Bool) -> Unit {
  guard !prog.has_direct_eval else { return }
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
      reserved[prog.names[r.name]] = true
    }
  }
  for i in 0..<prog.symbols.length() {
    let s = prog.symbols[i]
    let root_index = prog.symbol_root(i)
    let root = prog.symbols[root_index]
    let flags = if (s.flags & sym_must_not_rename) != 0 ||
      s.name == name_arguments ||
      prog.preserved_names.contains(s.name) ||
      (s.scope == 0 && !rename_top_level) {
      root.flags | sym_must_not_rename
    } else {
      root.flags
    }
    if flags != root.flags {
      prog.symbols[root_index] = { ..root, flags, }
    }
    if (flags & sym_must_not_rename) != 0 {
      reserved[sym_name(prog, i)] = true
    }
  }
  let declarations : Array[Array[Int]] = Array::makei(prog.scopes.length(), _ => {
    []
  })
  for index in 0..<prog.symbols.length() {
    let symbol = prog.symbols[index]
    guard symbol.link < 0 && (symbol.flags & sym_must_not_rename) == 0 else {
      continue
    }
    declarations[symbol.scope].push(index)
  }
  let captured : Array[Map[Int, Bool]] = Array::makei(prog.scopes.length(), _ => {})
  for reference in prog.refs {
    if reference.sym >= 0 {
      reserve_capture(prog, captured, reference.scope, reference.sym)
    }
  }
  // Parameters copied into a function-body environment and linked catch/var
  // declarations also constrain names even when the binding is never read.
  for scope in 0..<prog.scopes.length() {
    for _, symbol in prog.scope_members[scope] {
      reserve_capture(prog, captured, scope, prog.symbol_root(symbol))
    }
  }
  let candidates = prog.generated_names
  let blocked_in_scope : Array[Int] = []
  let mut candidate_index = 0
  // Scope allocation is preorder; ancestors receive names before descendants.
  for scope in 0..<prog.scopes.length() {
    let targets = declarations[scope]
    guard targets.length() > 0 else { continue }
    targets.sort_by((left, right) => {
      let frequency = prog.symbols[right].use_count -
        prog.symbols[left].use_count
      if frequency == 0 {
        left - right
      } else {
        frequency
      }
    })
    for symbol, _ in captured[scope] {
      let slot = prog.symbols[symbol].name_slot
      if slot >= 0 {
        blocked_in_scope[slot] = scope
      }
    }
    let mut slot = 0
    for index in targets {
      while slot < blocked_in_scope.length() && blocked_in_scope[slot] == scope {
        slot += 1
      }
      while slot >= candidates.length() {
        let candidate = name_from_index(candidate_index)
        candidate_index += 1
        if !reserved.contains(candidate) {
          candidates.push(candidate)
          blocked_in_scope.push(-1)
        }
      }
      prog.symbols[index] = { ..prog.symbols[index], name_slot: slot, }
      slot += 1
    }
  }
}

///|
/// A binding's name must remain visible on the path from a use or declaration
/// occurrence to its owning scope. Disjoint subtrees may reuse that name.
fn reserve_capture(
  prog : Program,
  captured : Array[Map[Int, Bool]],
  scope : Int,
  symbol : Int,
) -> Unit {
  let binding = prog.symbols[symbol]
  guard (binding.flags & sym_must_not_rename) == 0 else { return }
  for current = scope {
    guard current >= 0 && current != binding.scope else { break }
    guard !captured[current].contains(symbol) else { break }
    captured[current][symbol] = true
    continue prog.scopes[current].parent
  }
}
