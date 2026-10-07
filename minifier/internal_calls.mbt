///|
priv struct InternalCalls {
  program : Program
  functions : Array[Int]
  direct_uses : Array[Int]
  argument_observers : @hashset.HashSet[Int]
  trailing_undefined_calls : Array[Int]
  is_module : Bool
  rename_top_level : Bool
  strict_top_level : Bool
}

///|
/// A private binding is callable only when its declaration and direct calls
/// account for every reference. Writes, exports and escaping function values
/// therefore exclude it without a separate assignment analysis.
fn InternalCalls::new(
  program : Program,
  rename_top_level : Bool,
) -> InternalCalls {
  let is_module = program.modules.length() > 0
  {
    program,
    functions: Array::make(program.symbols.length(), -1),
    direct_uses: Array::make(program.symbols.length(), 0),
    argument_observers: @hashset.HashSet([]),
    trailing_undefined_calls: [],
    is_module,
    rename_top_level,
    strict_top_level: is_module || program.has_strict_directive(program.root),
  }
}

///|
fn InternalCalls::collect_node(
  self : InternalCalls,
  id : Int,
  node : Node,
) -> Unit {
  let program = self.program
  match node.kind {
    FunctionDeclaration => {
      let declaration = program.functions[node.a]
      guard declaration.name >= 0 else { return }
      let symbol = program.references[declaration.name].symbol
      let scope = program.symbols[symbol].scope
      // Block functions have additional Annex B binding behavior. Script
      // globals are private only under the existing top-level-mangle contract.
      guard program.scopes[scope].kind == SCOPE_FUNCTION ||
        (scope == 0 && (self.is_module || self.rename_top_level)) else {
        return
      }
      self.register_function(symbol, node.a)
    }
    VariableDeclaration if node.a == DECLARATION_CONST =>
      for index in 0..<node.c {
        let declaration = program.nodes[program.edges[node.b + index]]
        let pattern = program.nodes[declaration.a]
        guard pattern.kind == PatternIdentifier && declaration.b >= 0 else {
          continue
        }
        let value = program.nodes[program.unwrapped_expression(declaration.b)]
        guard value.kind == FunctionExpression || value.kind == ArrowExpression else {
          continue
        }
        // A named expression has another binding for its own function object.
        // Checking only the outer const would miss innerName.arguments reads.
        guard program.functions[value.a].name < 0 else { continue }
        self.register_function(program.references[pattern.a].symbol, value.a)
      }
    Identifier if program.references[node.a].name == NAME_ARGUMENTS =>
      self.observe_arguments(program.references[node.a].scope)
    Call => self.collect_call(id, node)
    _ => ()
  }
}

///|
fn InternalCalls::finish(self : InternalCalls) -> Unit {
  let program = self.program
  for statement in program.modules {
    guard statement is ExportDefault(body) else { continue }
    let declaration = program.nodes[body]
    guard declaration.kind == FunctionDeclaration else { continue }
    let name = program.functions[declaration.a].name
    guard name >= 0 else { continue }
    self.functions[program.references[name].symbol] = -1
  }
  for id in self.trailing_undefined_calls {
    self.trim_call(id)
  }
}

///|
fn InternalCalls::register_function(
  self : InternalCalls,
  symbol : Int,
  function_id : Int,
) -> Unit {
  let program = self.program
  let function = program.functions[function_id]
  // A sloppy callee can expose its argument count through an external
  // function's .caller.arguments, without an arguments reference in this file.
  guard self.strict_top_level ||
    (function.flags & FUNCTION_ARROW) != 0 ||
    program.has_strict_directive(function.body) else {
    return
  }
  let binding = program.symbols[symbol]
  guard (binding.flags & SYMBOL_PRESERVED) == 0 &&
    !self.program.preserved_names.contains(binding.name) else {
    return
  }
  self.functions[symbol] = function_id
}

///|
fn Program::has_strict_directive(self : Program, id : Int) -> Bool {
  let body = self.nodes[id]
  guard body.kind == Block else { return false }
  for index in 0..<body.b {
    let statement = self.nodes[self.edges[body.a + index]]
    guard statement.kind == ExpressionStatement else { return false }
    let value = self.nodes[statement.a]
    guard value.kind == StringLiteral else { return false }
    let spelling = self.source[value.a:value.b]
    if spelling == "\"use strict\"" || spelling == "'use strict'" {
      return true
    }
  }
  false
}

///|
fn InternalCalls::collect_call(
  self : InternalCalls,
  id : Int,
  call : Node,
) -> Unit {
  let program = self.program
  let callee = program.nodes[program.unwrapped_expression(call.a)]
  guard callee.kind == Identifier else { return }
  let symbol = program.references[callee.a].symbol
  guard symbol >= 0 else { return }
  self.direct_uses[symbol] += 1
  guard call.c > 0 &&
    program.is_undefined_value(program.edges[call.b + call.c - 1]) else {
    return
  }
  self.trailing_undefined_calls.push(id)
}

///|
/// Ordinary functions own arguments; nested arrows inherit that same object.
fn InternalCalls::observe_arguments(self : InternalCalls, scope : Int) -> Unit {
  for current = scope {
    guard current >= 0 else { break }
    let environment = self.program.scopes[current]
    if environment.kind == SCOPE_PARAMETERS {
      self.argument_observers.add(current)
      break
    }
    continue environment.parent
  }
}

///|
fn InternalCalls::ignores_argument_count(
  self : InternalCalls,
  function_id : Int,
) -> Bool {
  let program = self.program
  let function = program.functions[function_id]
  let is_arrow = (function.flags & FUNCTION_ARROW) != 0
  // MoonBit uses simple parameters. Keep destructuring/rest outside this rule;
  // simple defaults still receive undefined when their argument is omitted.
  let mut parameter_scope = -1
  for index in 0..<function.parameters_count {
    let parameter = program.nodes[program.edges[function.parameters_offset +
      index]]
    let binding = if parameter.kind == PatternDefault {
      program.nodes[parameter.a]
    } else {
      parameter
    }
    guard binding.kind == PatternIdentifier else { return false }
    parameter_scope = program.references[binding.a].scope
  }
  is_arrow ||
  (parameter_scope >= 0 && !self.argument_observers.contains(parameter_scope))
}

///|
fn InternalCalls::trim_call(self : InternalCalls, id : Int) -> Unit {
  let program = self.program
  let call = program.nodes[id]
  let callee = program.nodes[program.unwrapped_expression(call.a)]
  let symbol = program.references[callee.a].symbol
  let function_id = self.functions[symbol]
  guard function_id >= 0 &&
    program.symbols[symbol].use_count == self.direct_uses[symbol] + 1 &&
    self.ignores_argument_count(function_id) else {
    return
  }
  for index in 0..<call.c {
    guard program.nodes[program.edges[call.b + index]].kind != Spread else {
      return
    }
  }
  let count = for remaining = call.c {
    guard remaining > 0 &&
      program.is_undefined_value(program.edges[call.b + remaining - 1]) else {
      break remaining
    }
    continue remaining - 1
  }
  program.nodes[id] = { ..call, c: count, }
}
