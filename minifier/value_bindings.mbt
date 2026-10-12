///|
priv struct ValueBindings {
  program : Program
  rename_top_level : Bool
  declarations : Array[Int]
  functions : Array[Bool]
  has_writes : Array[Bool]
  alias_declarations : Array[Int]
  function_value_declarations : Array[Int]
  call_write_targets : Array[Int]
}

///|
fn ValueBindings::new(
  program : Program,
  rename_top_level : Bool,
) -> ValueBindings {
  let bindings = {
    program,
    rename_top_level,
    declarations: Array::make(program.symbols.length(), -1),
    functions: Array::make(program.symbols.length(), false),
    has_writes: Array::make(program.symbols.length(), false),
    alias_declarations: [],
    function_value_declarations: [],
    call_write_targets: [],
  }
  for id in 0..<program.nodes.length() {
    let node = program.nodes[id]
    match node.kind {
      VariableDeclaration =>
        for index in 0..<node.c {
          let declaration = program.nodes[program.edges[node.b + index]]
          let symbol = program.identifier_symbol(declaration.a)
          guard symbol >= 0 else {
            bindings.mark_written(declaration.a)
            continue
          }
          bindings.declarations[symbol] = id
          if node.a == DECLARATION_CONST &&
            node.c == 1 &&
            declaration.b >= 0 &&
            program.nodes[program.unwrapped_expression(declaration.b)].kind
            is Identifier {
            bindings.alias_declarations.push(id)
          }
          if node.a == DECLARATION_VAR {
            bindings.has_writes[symbol] = true
          }
        }
      FunctionDeclaration => {
        let name = program.functions[node.a].name
        guard name >= 0 else { continue }
        let symbol = program.references[name].symbol
        if bindings.declarations[symbol] >= 0 {
          bindings.has_writes[symbol] = true
        }
        bindings.declarations[symbol] = id
        let entry = program.symbols[symbol]
        bindings.functions[symbol] = entry.flags == 0 &&
          entry.link < 0 &&
          (
            program.scopes[entry.scope].kind == SCOPE_FUNCTION ||
            (entry.scope == 0 && rename_top_level)
          )
        if bindings.functions[symbol] && entry.use_count == 2 {
          bindings.function_value_declarations.push(id)
        }
      }
      Assignment => bindings.mark_written(node.b)
      Update => bindings.mark_written(node.c)
      Unary if program.operators[node.a] is Delete =>
        bindings.mark_written(node.b)
      ForIn | ForOf => bindings.mark_written(node.a)
      If => {
        bindings.mark_conditional_function(node.b)
        bindings.mark_conditional_function(node.c)
      }
      While | With => bindings.mark_conditional_function(node.b)
      DoWhile => bindings.mark_conditional_function(node.a)
      For => bindings.mark_conditional_function(node.d)
      Labeled => bindings.mark_conditional_function(node.c)
      _ => ()
    }
  }
  // A named default export escapes without an ordinary Identifier reference.
  // Its declaration must survive even when every direct local call is folded.
  for statement in program.modules {
    guard statement is ExportDefault(body) else { continue }
    let declaration = program.nodes[body]
    guard declaration.kind is FunctionDeclaration else { continue }
    let name = program.functions[declaration.a].name
    if name >= 0 {
      bindings.functions[program.references[name].symbol] = false
    }
  }
  bindings
}

///|
fn ValueBindings::mark_conditional_function(
  self : ValueBindings,
  id : Int,
) -> Unit {
  guard id >= 0 else { return }
  let declaration = self.program.nodes[id]
  guard declaration.kind is FunctionDeclaration else { return }
  let name = self.program.functions[declaration.a].name
  guard name >= 0 else { return }
  let symbol = self.program.references[name].symbol
  self.has_writes[symbol] = true
  self.functions[symbol] = false
}

///|
fn ValueBindings::initialized_read(
  self : ValueBindings,
  id : Int,
  before : Int,
  scope : Int,
) -> Bool {
  let program = self.program
  let symbol = program.identifier_symbol(id)
  guard symbol >= 0 else { return self.stable_prefix(id, before, scope) }
  if self.functions[symbol] {
    return self.stable_prefix(id, before, scope)
  }
  let entry = program.symbols[symbol]
  let environment = program.scopes[entry.scope]
  // A later case can be entered without executing earlier declarations in
  // this shared lexical environment. Source order does not prove readiness.
  guard environment.kind != SCOPE_SWITCH else { return false }
  let is_parameter = environment.kind == SCOPE_PARAMETERS ||
    environment.kind == SCOPE_ARROW_PARAMETERS
  let declaration = self.declarations[symbol]
  if entry.scope == 0 && declaration >= 0 {
    let binding = program.nodes[declaration]
    if binding.kind is VariableDeclaration && binding.a == DECLARATION_VAR {
      return false
    }
  }
  guard is_parameter || (declaration >= 0 && declaration < before) else {
    return self.stable_prefix(id, before, scope)
  }
  for current = scope {
    guard current >= 0 else { break false }
    if current == entry.scope {
      break true
    }
    let ancestor = program.scopes[current]
    guard !(ancestor.kind is (SCOPE_PARAMETERS | SCOPE_ARROW_PARAMETERS)) else {
      break false
    }
    continue ancestor.parent
  }
}

///|
/// These expressions may read or throw, but cannot change preceding values.
/// Member access and coercing operators are excluded because they can invoke
/// user code. Allocation alone does not make an object literal observable.
fn ValueBindings::has_no_side_effects(self : ValueBindings, id : Int) -> Bool {
  let program = self.program
  guard id >= 0 else { return true }
  let node = program.nodes[id]
  match node.kind {
    Identifier => {
      let symbol = program.references[node.a].symbol
      guard symbol >= 0 else { return program.is_undefined_value(id) }
      if program.symbols[symbol].scope > 0 || program.modules.length() > 0 {
        return true
      }
      let declaration = self.declarations[symbol]
      guard declaration >= 0 else { return false }
      let binding = program.nodes[declaration]
      binding.kind is VariableDeclaration && binding.a != DECLARATION_VAR
    }
    NumberLiteral
    | StringLiteral
    | BooleanLiteral
    | NullLiteral
    | RegexLiteral
    | TemplateNoSubstitution
    | FunctionExpression
    | ArrowExpression => true
    Parenthesized | EvaluatedValue => self.has_no_side_effects(node.a)
    Unary if program.operators[node.a] is (LogicalNot | TypeOf | Void) =>
      self.has_no_side_effects(node.b)
    Binary if program.operators[node.a] is (StrictlyEqual | StrictlyNotEqual) =>
      self.has_no_side_effects(node.b) && self.has_no_side_effects(node.c)
    Logical =>
      self.has_no_side_effects(node.b) && self.has_no_side_effects(node.c)
    Conditional =>
      self.has_no_side_effects(node.a) &&
      self.has_no_side_effects(node.b) &&
      self.has_no_side_effects(node.c)
    ArrayLiteral | Sequence => {
      for index in 0..<node.b {
        guard self.has_no_side_effects(program.edges[node.a + index]) else {
          return false
        }
      }
      true
    }
    ObjectLiteral => {
      for index in 0..<node.b {
        let property = program.nodes[program.edges[node.a + index]]
        guard property.kind is Property &&
          property.d != PROPERTY_COMPUTED &&
          self.has_no_side_effects(property.b) else {
          return false
        }
      }
      true
    }
    _ => false
  }
}

///|
/// These initializers neither change state nor throw. They may therefore move
/// after an effect-free read even when that read could throw for an outer TDZ.
fn ValueBindings::is_inert_initializer(
  self : ValueBindings,
  id : Int,
  before : Int,
  scope : Int,
) -> Bool {
  guard id >= 0 else { return false }
  let node = self.program.nodes[id]
  match node.kind {
    Identifier => self.initialized_read(id, before, scope)
    NumberLiteral
    | ZeroLiteral
    | StringLiteral
    | BooleanLiteral
    | NullLiteral
    | RegexLiteral
    | TemplateNoSubstitution
    | FunctionExpression
    | ArrowExpression => true
    Parenthesized | EvaluatedValue =>
      self.is_inert_initializer(node.a, before, scope)
    _ => false
  }
}

///|
fn ValueBindings::mark_written(self : ValueBindings, id : Int) -> Unit {
  guard id >= 0 else { return }
  let program = self.program
  let node = program.nodes[id]
  match node.kind {
    Call => self.call_write_targets.push(id)
    Identifier | PatternIdentifier => {
      let symbol = program.references[node.a].symbol
      if symbol >= 0 {
        self.has_writes[symbol] = true
      }
    }
    VariableDeclaration =>
      for index in 0..<node.c {
        self.mark_written(program.nodes[program.edges[node.b + index]].a)
      }
    Parenthesized | PatternDefault | PatternRest => self.mark_written(node.a)
    PatternProperty => self.mark_written(node.b)
    PatternArray | PatternObject =>
      for index in 0..<node.b {
        self.mark_written(program.edges[node.a + index])
      }
    _ => ()
  }
}

///|
/// A prefix may be crossed only if it cannot throw and its value cannot be
/// changed by the initializer being moved. Function bindings are hoisted;
/// lexical constants must already be initialized in this execution frame.
fn ValueBindings::stable_prefix(
  self : ValueBindings,
  id : Int,
  before : Int,
  scope : Int,
) -> Bool {
  let program = self.program
  let node = program.nodes[program.unwrapped_expression(id)]
  match node.kind {
    NumberLiteral | StringLiteral | BooleanLiteral | NullLiteral => true
    Identifier => {
      let symbol = program.references[node.a].symbol
      guard symbol >= 0 && !self.has_writes[symbol] else { return false }
      let entry = program.symbols[symbol]
      guard entry.flags == 0 && entry.link < 0 else { return false }
      guard program.scopes[entry.scope].kind != SCOPE_SWITCH else {
        return false
      }
      if self.functions[symbol] {
        return true
      }
      let declaration = self.declarations[symbol]
      guard declaration >= 0 && declaration < before else { return false }
      guard program.nodes[declaration].kind is VariableDeclaration else {
        return false
      }
      for current = scope {
        guard current >= 0 else { break false }
        if current == entry.scope {
          break true
        }
        let environment = program.scopes[current]
        guard !(environment.kind is (SCOPE_PARAMETERS | SCOPE_ARROW_PARAMETERS)) else {
          break false
        }
        continue environment.parent
      }
    }
    _ => false
  }
}

///|
fn ValueBindings::inline_aliases(
  self : ValueBindings,
  rename_top_level : Bool,
) -> Unit {
  let program = self.program
  let aliases = Array::make(program.symbols.length(), -1)
  let removed : Array[Int] = []
  for id in self.alias_declarations {
    let declaration = program.nodes[id]
    guard declaration.kind is VariableDeclaration else { continue }
    let binding = program.nodes[program.edges[declaration.b]]
    let symbol = program.identifier_symbol(binding.a)
    guard symbol >= 0 && binding.b >= 0 && !self.has_writes[symbol] else {
      continue
    }
    let entry = program.symbols[symbol]
    guard entry.scope > 0 &&
      entry.flags == 0 &&
      entry.link < 0 &&
      !program.preserved_names.contains(entry.name) else {
      continue
    }
    let source = program.identifier_symbol(binding.b)
    guard source >= 0 &&
      source != symbol &&
      self.stable_prefix(binding.b, id, entry.scope) else {
      continue
    }
    let target = if aliases[source] >= 0 { aliases[source] } else { source }
    let original = program.symbols[target]
    guard (original.scope > 0 || rename_top_level) &&
      original.flags == 0 &&
      original.link < 0 &&
      original.name != NAME_ARGUMENTS &&
      !program.preserved_names.contains(original.name) else {
      continue
    }
    aliases[symbol] = target
    removed.push(id)
  }
  guard removed.length() > 0 else { return }
  for id in removed {
    let declaration = program.nodes[id]
    let binding = program.nodes[program.edges[declaration.b]]
    let symbol = program.identifier_symbol(binding.a)
    let entry = program.symbols[symbol]
    let target = aliases[symbol]
    let original = program.symbols[target]
    program.symbols[target] = {
      ..original,
      use_count: original.use_count + entry.use_count - 2,
    }
    program.symbols[symbol] = { ..entry, flags: entry.flags | SYMBOL_ELIDED, }
    program.nodes[id] = Node::new(Elided, 0, 0, 0, 0)
  }
  for index in 0..<program.references.length() {
    let reference = program.references[index]
    guard reference.symbol >= 0 else { continue }
    let target = aliases[reference.symbol]
    if target >= 0 {
      program.references[index] = { ..reference, symbol: target, }
    }
  }
}

///|
priv struct ValueMovement {
  bindings : ValueBindings
  symbol : Int
  before : Int
  scope : Int
  initializer_has_no_effects : Bool
  initializer_is_inert : Bool
  mut detach_receiver : Bool
}

///|
fn ValueMovement::new(
  bindings : ValueBindings,
  symbol : Int,
  before : Int,
  initializer : Int,
) -> ValueMovement {
  let scope = bindings.program.symbols[symbol].scope
  {
    bindings,
    symbol,
    before,
    scope,
    initializer_has_no_effects: initializer >= 0 &&
    bindings.has_no_side_effects(initializer),
    initializer_is_inert: bindings.is_inert_initializer(
      initializer, before, scope,
    ),
    detach_receiver: false,
  }
}

///|
fn ValueMovement::can_cross_read(self : ValueMovement, id : Int) -> Bool {
  if self.initializer_is_inert && self.bindings.has_no_side_effects(id) {
    return true
  }
  if self.initializer_has_no_effects {
    self.bindings.initialized_read(id, self.before, self.scope)
  } else {
    self.bindings.stable_prefix(id, self.before, self.scope)
  }
}
