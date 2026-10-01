///|
fn short_name(n : Int, alphabet : NameAlphabet) -> String {
  let builder = StringBuilder()
  builder.write_char(alphabet.first.get_char(n % 54).unwrap())
  for remaining = n / 54; remaining > 0; remaining = (remaining - 1) / 64 {
    builder.write_char(alphabet.rest.get_char((remaining - 1) % 64).unwrap())
  }
  builder.to_string()
}

///|
let keywords : @hashset.HashSet[String] = @hashset.HashSet([
  "break", "case", "catch", "class", "const", "continue", "debugger", "default",
  "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for",
  "function", "if", "import", "in", "instanceof", "new", "null", "return", "super",
  "switch", "this", "throw", "true", "try", "typeof", "var", "void", "while", "with",
  "yield", "let", "static", "await", "async", "get", "set", "of", "as", "from", "arguments",
  "eval", "undefined", "NaN", "Infinity", "implements", "interface", "package", "private",
  "protected", "public",
])

///|
fn symbol_name(program : Program, symbol : Int) -> String {
  program.names[program.symbols[symbol].name]
}

///|
fn mangle(program : Program, rename_top_level : Bool) -> Unit {
  guard !program.has_dynamic_scope else { return }
  let reserved = keywords.copy()
  for r in program.references {
    if r.symbol < 0 {
      reserved.add(program.names[r.name])
    }
  }
  for i in 0..<program.symbols.length() {
    let s = program.symbols[i]
    let root_index = program.symbol_root(i)
    let root = program.symbols[root_index]
    let flags = if (s.flags & symbol_preserved) != 0 ||
      s.name == name_arguments ||
      program.preserved_names.contains(s.name) ||
      (s.scope == 0 && !rename_top_level) {
      root.flags | symbol_preserved
    } else {
      root.flags
    }
    if flags != root.flags {
      program.symbols[root_index] = { ..root, flags, }
    }
    if (flags & symbol_preserved) != 0 {
      reserved.add(symbol_name(program, i))
    }
  }
  program.alphabet = name_alphabet(program)
  let declarations : Array[Array[Int]] = Array::makei(program.scopes.length(), _ => {
    []
  })
  for index in 0..<program.symbols.length() {
    let symbol = program.symbols[index]
    guard symbol.link < 0 && (symbol.flags & symbol_preserved) == 0 else {
      continue
    }
    declarations[symbol.scope].push(index)
  }
  let captured : Array[@hashset.HashSet[Int]] = Array::makei(
    program.scopes.length(),
    _ => @hashset.HashSet([]),
  )
  for reference in program.references {
    if reference.symbol >= 0 {
      reserve_capture(program, captured, reference.scope, reference.symbol)
    }
  }
  // Parameters copied into a function-body environment and linked catch/var
  // declarations also constrain names even when the binding is never read.
  for scope in 0..<program.scopes.length() {
    for _, symbol in program.scope_members[scope] {
      reserve_capture(program, captured, scope, program.symbol_root(symbol))
    }
  }
  let candidates = program.generated_names
  let blocked : Array[Int] = []
  let generator = NameGenerator::new(program.alphabet, reserved)
  // Scope allocation is preorder; ancestors receive names before descendants.
  for scope in 0..<program.scopes.length() {
    let targets = declarations[scope]
    guard targets.length() > 0 else { continue }
    targets.sort_by((left, right) => {
      let frequency = program.symbols[right].use_count -
        program.symbols[left].use_count
      if frequency == 0 {
        left - right
      } else {
        frequency
      }
    })
    for symbol in captured[scope] {
      let slot = program.symbols[symbol].name_slot
      if slot >= 0 {
        blocked[slot] = scope
      }
    }
    let mut slot = 0
    for index in targets {
      while slot < blocked.length() && blocked[slot] == scope {
        slot += 1
      }
      while slot >= candidates.length() {
        candidates.push(generator.next_name())
        blocked.push(-1)
      }
      program.symbols[index] = { ..program.symbols[index], name_slot: slot, }
      slot += 1
    }
  }
}

///|
/// A binding's name must remain visible on the path from a use or declaration
/// occurrence to its owning scope. Disjoint subtrees may reuse that name.
fn reserve_capture(
  program : Program,
  captured : Array[@hashset.HashSet[Int]],
  scope : Int,
  symbol : Int,
) -> Unit {
  let binding = program.symbols[symbol]
  guard (binding.flags & symbol_preserved) == 0 else { return }
  for current = scope {
    guard current >= 0 && current != binding.scope else { break }
    guard captured[current].add_and_check(symbol) else { break }
    continue program.scopes[current].parent
  }
}
