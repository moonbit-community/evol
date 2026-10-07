///|
fn Program::resolve(self : Program) -> Unit {
  self.collapse_transparent_scopes()
  for index in 0..<self.references.length() {
    let reference = self.references[index]
    let scope = self.reference_scope(reference.scope)
    let binding = if reference.symbol >= 0 {
      reference.symbol
    } else {
      self.find_binding(scope, reference.name)
    }
    let symbol = if binding >= 0 { self.symbol_root(binding) } else { binding }
    if symbol >= 0 {
      let resolved = self.symbols[symbol]
      self.symbols[symbol] = { ..resolved, use_count: resolved.use_count + 1, }
    }
    self.references[index] = { ..reference, scope, symbol, }
  }
}

///|
fn Program::collapse_transparent_scopes(self : Program) -> Unit {
  // Parent scopes are allocated before children. Collapse provisional scopes
  // in one forward pass, avoiding repeated reparenting of nested expressions.
  for index in 0..<self.scopes.length() {
    let scope = self.scopes[index]
    guard scope.parent >= 0 &&
      self.scopes[scope.parent].kind == SCOPE_TRANSPARENT else {
      continue
    }
    self.scopes[index] = { ..scope, parent: self.scopes[scope.parent].parent, }
  }
}

///|
fn Program::reference_scope(self : Program, scope : Int) -> Int {
  guard self.scopes[scope].kind == SCOPE_TRANSPARENT else { return scope }
  self.scopes[scope].parent
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
    guard name != NAME_ARGUMENTS || environment.kind != SCOPE_PARAMETERS else {
      break -1
    }
    continue environment.parent
  }
}
