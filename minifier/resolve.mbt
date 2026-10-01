///|
fn resolve(program : Program) -> Unit {
  program.collapse_transparent_scopes()
  for index in 0..<program.references.length() {
    let reference = program.references[index]
    let scope = program.reference_scope(reference.scope)
    let binding = if reference.symbol >= 0 {
      reference.symbol
    } else {
      program.find_binding(scope, reference.name)
    }
    let symbol = if binding >= 0 {
      program.symbol_root(binding)
    } else {
      binding
    }
    if symbol >= 0 {
      let resolved = program.symbols[symbol]
      program.symbols[symbol] = {
        ..resolved,
        use_count: resolved.use_count + 1,
      }
    }
    program.references[index] = { ..reference, scope, symbol, }
  }
}

///|
fn Program::collapse_transparent_scopes(self : Program) -> Unit {
  // Parent scopes are allocated before children. Collapse provisional scopes
  // in one forward pass, avoiding repeated reparenting of nested expressions.
  for index in 0..<self.scopes.length() {
    let scope = self.scopes[index]
    if scope.parent >= 0 && self.scopes[scope.parent].kind == scope_transparent {
      self.scopes[index] = {
        ..scope,
        parent: self.scopes[scope.parent].parent,
      }
    }
  }
}

///|
fn Program::reference_scope(self : Program, scope : Int) -> Int {
  if self.scopes[scope].kind == scope_transparent {
    self.scopes[scope].parent
  } else {
    scope
  }
}

///|
fn Program::find_binding(self : Program, start_scope : Int, name : Int) -> Int {
  for scope = start_scope {
    guard scope >= 0 else { break -1 }
    let environment = self.scopes[scope]
    if self.scope_members[scope].get(name) is Some(binding) {
      break binding
    }
    // Ordinary functions introduce arguments; arrows inherit it.
    guard name != name_arguments || environment.kind != scope_parameters else {
      break -1
    }
    continue environment.parent
  }
}
