///|
fn resolve(prog : Program) -> Unit {
  // Parent scopes are allocated before children. Collapse provisional scopes
  // in one forward pass, avoiding repeated reparenting of nested expressions.
  for index in 0..<prog.scopes.length() {
    let scope = prog.scopes[index]
    if scope.parent >= 0 && prog.scopes[scope.parent].kind == scope_transparent {
      prog.scopes[index] = {
        ..scope,
        parent: prog.scopes[scope.parent].parent,
      }
    }
  }
  for index in 0..<prog.refs.length() {
    let reference = prog.refs[index]
    let start_scope = if prog.scopes[reference.scope].kind == scope_transparent {
      prog.scopes[reference.scope].parent
    } else {
      reference.scope
    }
    let mut symbol = reference.sym
    if symbol < 0 {
      let mut scope = start_scope
      while scope >= 0 {
        let environment = prog.scopes[scope]
        if prog.scope_members[scope].get(reference.name) is Some(binding) {
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
      symbol = prog.symbol_root(symbol)
      let binding = prog.symbols[symbol]
      prog.symbols[symbol] = { ..binding, use_count: binding.use_count + 1, }
    }
    prog.refs[index] = { ..reference, scope: start_scope, sym: symbol, }
  }
}
