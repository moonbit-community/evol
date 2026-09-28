///|
fn resolve(program : Program) -> Unit {
  // Parent scopes are allocated before children. Collapse provisional scopes
  // in one forward pass, avoiding repeated reparenting of nested expressions.
  for index in 0..<program.scopes.length() {
    let scope = program.scopes[index]
    if scope.parent >= 0 &&
      program.scopes[scope.parent].kind == scope_transparent {
      program.scopes[index] = {
        ..scope,
        parent: program.scopes[scope.parent].parent,
      }
    }
  }
  for index in 0..<program.references.length() {
    let reference = program.references[index]
    let start_scope = if program.scopes[reference.scope].kind ==
      scope_transparent {
      program.scopes[reference.scope].parent
    } else {
      reference.scope
    }
    let mut symbol = reference.symbol
    if symbol < 0 {
      let mut scope = start_scope
      while scope >= 0 {
        let environment = program.scopes[scope]
        if program.scope_members[scope].get(reference.name) is Some(binding) {
          symbol = binding
          break
        }
        // Ordinary functions introduce arguments; arrows inherit it.
        if reference.name == name_arguments &&
          environment.kind == scope_parameters {
          break
        }
        scope = environment.parent
      }
    }
    if symbol >= 0 {
      symbol = program.symbol_root(symbol)
      let binding = program.symbols[symbol]
      program.symbols[symbol] = { ..binding, use_count: binding.use_count + 1, }
    }
    program.references[index] = { ..reference, scope: start_scope, symbol, }
  }
}
