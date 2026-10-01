///|
fn short_name(ordinal : Int, alphabet : NameAlphabet) -> String {
  let builder = StringBuilder()
  let first_radix = alphabet.first.length()
  let rest_radix = alphabet.rest.length()
  builder.write_char(alphabet.first.get_char(ordinal % first_radix).unwrap())
  for remaining = ordinal / first_radix
      remaining > 0
      remaining = (remaining - 1) / rest_radix {
    builder.write_char(
      alphabet.rest.get_char((remaining - 1) % rest_radix).unwrap(),
    )
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
  let reserved = program.reserve_symbol_names(rename_top_level)
  program.alphabet = name_alphabet(program)
  let declarations = program.renameable_declarations()
  let captured = program.captured_symbols()
  program.assign_symbol_names(reserved, declarations, captured)
}

///|
fn Program::reserve_symbol_names(
  self : Program,
  rename_top_level : Bool,
) -> @hashset.HashSet[String] {
  let reserved = keywords.copy()
  for reference in self.references {
    if reference.symbol < 0 {
      reserved.add(self.names[reference.name])
    }
  }
  for index in 0..<self.symbols.length() {
    let symbol = self.symbols[index]
    let root_index = self.symbol_root(index)
    let root = self.symbols[root_index]
    let must_preserve = (symbol.flags & symbol_preserved) != 0 ||
      symbol.name == name_arguments ||
      self.preserved_names.contains(symbol.name) ||
      (symbol.scope == 0 && !rename_top_level)
    let flags = if must_preserve {
      root.flags | symbol_preserved
    } else {
      root.flags
    }
    if flags != root.flags {
      self.symbols[root_index] = { ..root, flags, }
    }
    if (flags & symbol_preserved) != 0 {
      reserved.add(symbol_name(self, index))
    }
  }
  reserved
}

///|
fn Program::renameable_declarations(self : Program) -> Array[Array[Int]] {
  let declarations : Array[Array[Int]] = Array::makei(self.scopes.length(), _ => {
    []
  })
  for index in 0..<self.symbols.length() {
    let symbol = self.symbols[index]
    guard symbol.link < 0 && (symbol.flags & symbol_preserved) == 0 else {
      continue
    }
    declarations[symbol.scope].push(index)
  }
  declarations
}

///|
fn Program::captured_symbols(self : Program) -> Array[@hashset.HashSet[Int]] {
  let captured : Array[@hashset.HashSet[Int]] = Array::makei(
    self.scopes.length(),
    _ => @hashset.HashSet([]),
  )
  for reference in self.references {
    if reference.symbol >= 0 {
      reserve_capture(self, captured, reference.scope, reference.symbol)
    }
  }
  // Parameters copied into a function-body environment and linked catch/var
  // declarations also constrain names even when the binding is never read.
  for scope in 0..<self.scopes.length() {
    for _, symbol in self.scope_members[scope] {
      reserve_capture(self, captured, scope, self.symbol_root(symbol))
    }
  }
  captured
}

///|
fn Program::assign_symbol_names(
  self : Program,
  reserved : @hashset.HashSet[String],
  declarations : Array[Array[Int]],
  captured : Array[@hashset.HashSet[Int]],
) -> Unit {
  let candidates = self.generated_names
  let blocked : Array[Int] = []
  let generator = NameGenerator::new(self.alphabet, reserved)
  // Scope allocation is preorder; ancestors receive names before descendants.
  for scope in 0..<self.scopes.length() {
    let targets = declarations[scope]
    guard targets.length() > 0 else { continue }
    targets.sort_by((left, right) => {
      let frequency = self.symbols[right].use_count -
        self.symbols[left].use_count
      if frequency == 0 {
        left - right
      } else {
        frequency
      }
    })
    for symbol in captured[scope] {
      let slot = self.symbols[symbol].name_slot
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
      self.symbols[index] = { ..self.symbols[index], name_slot: slot, }
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
