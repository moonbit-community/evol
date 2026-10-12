///|
fn Operator::text(operator : Operator) -> String {
  match operator {
    Assignment => "="
    Add => "+"
    Subtract => "-"
    Multiply => "*"
    Divide => "/"
    Modulo => "%"
    Exponent => "**"
    ShiftLeft => "<<"
    ShiftRight => ">>"
    UnsignedShiftRight => ">>>"
    LessThan => "<"
    GreaterThan => ">"
    LessThanOrEqual => "<="
    GreaterThanOrEqual => ">="
    Equal => "=="
    NotEqual => "!="
    StrictlyEqual => "==="
    StrictlyNotEqual => "!=="
    BitwiseAnd => "&"
    BitwiseOr => "|"
    BitwiseXor => "^"
    LogicalAnd => "&&"
    LogicalOr => "||"
    NullishCoalescing => "??"
    In => "in"
    InstanceOf => "instanceof"
    LogicalNot => "!"
    BitwiseNot => "~"
    Plus => "+"
    Minus => "-"
    TypeOf => "typeof"
    Void => "void"
    Delete => "delete"
    Await => "await"
    Yield => "yield"
    YieldStar => "yield*"
    Increment => "++"
    Decrement => "--"
  }
}

///|
fn Operator::is_word(operator : Operator) -> Bool {
  match operator {
    In | InstanceOf | TypeOf | Void | Delete | Await | Yield | YieldStar => true
    _ => false
  }
}

///|
priv struct Printer {
  program : Program
  output : StringBuilder
  mut previous_is_word : Bool
  mut last_character : UInt16
  mut pending_semicolon : Bool
  labels : LabelNames
  decimal_spellings : Map[StringView, String?]
}

///|
fn Printer::new(program : Program) -> Printer {
  {
    program,
    output: StringBuilder(),
    previous_is_word: false,
    last_character: 0,
    pending_semicolon: false,
    labels: LabelNames::new(program.alphabet),
    decimal_spellings: {},
  }
}

///|
fn Printer::flush_semicolon(self : Printer) -> Unit {
  guard self.pending_semicolon else { return }
  self.output.write_string(";")
  self.previous_is_word = false
  self.last_character = ';'
  self.pending_semicolon = false
}

///|
fn Printer::needs_separator(
  self : Printer,
  first_character : UInt16,
  is_word : Bool,
) -> Bool {
  (self.previous_is_word && is_word) ||
  (self.last_character == '+' && first_character == '+') ||
  (self.last_character == '-' && first_character == '-') ||
  (self.last_character == '!' && first_character == '-') ||
  (
    self.last_character == '/' &&
    (first_character == '/' || first_character == '*')
  )
}

///|
fn Printer::text(self : Printer, text : String, is_word : Bool) -> Unit {
  guard text.length() > 0 else { return }
  self.flush_semicolon()
  if self.needs_separator(text[0], is_word) {
    self.output.write_string(" ")
  }
  self.output.write_string(text)
  self.last_character = text[text.length() - 1]
  self.previous_is_word = is_word
}

///|
fn Printer::span(
  self : Printer,
  start : Int,
  end : Int,
  is_word : Bool,
) -> Unit {
  guard end > start else { return }
  self.flush_semicolon()
  let source = self.program.source
  if self.needs_separator(source[start], is_word) {
    self.output.write_string(" ")
  }
  self.output.write_substring(source, start, end - start)
  self.last_character = source[end - 1]
  self.previous_is_word = is_word
}

///|
fn Printer::reference(self : Printer, id : Int) -> Unit {
  let reference = self.program.references[id]
  let slot = if reference.symbol >= 0 {
    self.program.symbols[reference.symbol].name_slot
  } else {
    -1
  }
  guard slot >= 0 else {
    self.span(reference.name_start, reference.name_end, true)
    return
  }
  self.text(self.program.generated_names[slot], true)
}

///|
fn Printer::statements(self : Printer, offset : Int, count : Int) -> Unit {
  let mut previous_declaration_kind = -1
  for i in 0..<count {
    let id = self.program.edges[offset + i]
    let node = self.program.nodes[id]
    if node.kind == VariableDeclaration {
      if node.a == previous_declaration_kind {
        self.pending_semicolon = false
        self.text(",", false)
        self.declarators(node.b, node.c)
        self.pending_semicolon = true
      } else {
        self.statement(id)
      }
      previous_declaration_kind = node.a
    } else {
      if node.kind is Block && self.program.can_flatten_block(id) {
        if node.b == 0 {
          self.text(";", false)
        } else {
          self.statements(node.a, node.b)
        }
      } else if node.kind is If &&
        node.c >= 0 &&
        !(self.program.nodes[node.c].kind is FunctionDeclaration) &&
        self.program.always_exits(node.b) {
        self.if_statement(node.a, node.b, -1)
        self.statement_body(node.c, false)
      } else if node.kind is If &&
        node.c >= 0 &&
        !(self.program.nodes[node.b].kind is FunctionDeclaration) &&
        self.program.always_exits(node.c) {
        self.if_statement(self.program.negated_value(node.a), node.c, -1)
        self.statement_body(node.b, false)
      } else {
        self.statement(id)
      }
      previous_declaration_kind = -1
    }
  }
}

///|
fn Program::can_flatten_block(self : Program, id : Int) -> Bool {
  let block = self.nodes[id]
  for index in 0..<block.b {
    let statement = self.nodes[self.edges[block.a + index]]
    match statement.kind {
      VariableDeclaration if statement.a != DECLARATION_VAR => return false
      FunctionDeclaration | ClassDeclaration => return false
      ExpressionStatement if index == 0 &&
        self.nodes[statement.a].kind is StringLiteral => return false
      _ => ()
    }
  }
  true
}

///|
fn Printer::expressions(self : Printer, offset : Int, count : Int) -> Unit {
  for i in 0..<count {
    if i > 0 {
      self.text(",", false)
    }
    let argument = self.program.edges[offset + i]
    if self.program.nodes[argument].kind is Parenthesized {
      let value = self.program.unwrapped_expression(argument)
      if !(self.program.nodes[value].kind is Sequence) {
        self.expression(value)
        continue
      }
    }
    self.expression(argument)
  }
}

///|
fn Printer::pattern(self : Printer, id : Int) -> Unit {
  let node = self.program.nodes[id]
  match node.kind {
    PatternIdentifier | Identifier => self.reference(node.a)
    Member | Index => self.expression(id)
    PatternArray => self.array_pattern(node.a, node.b)
    PatternObject => self.object_pattern(node.a, node.b)
    PatternProperty => self.pattern_property(node)
    PatternRest => {
      self.text("...", false)
      self.pattern(node.a)
    }
    PatternDefault => {
      self.pattern(node.a)
      self.text("=", false)
      self.expression(node.b)
    }
    _ => abort("bad pattern: \{node.kind.to_repr()}")
  }
}

///|
fn Printer::array_pattern(self : Printer, offset : Int, count : Int) -> Unit {
  self.text("[", false)
  for index in 0..<count {
    if index > 0 {
      self.text(",", false)
    }
    let element = self.program.edges[offset + index]
    guard element >= 0 else { continue }
    self.pattern(element)
  }
  self.array_end(offset, count)
}

///|
fn Printer::array_end(self : Printer, offset : Int, count : Int) -> Unit {
  let ends_with_hole = count > 0 && self.program.edges[offset + count - 1] < 0
  if ends_with_hole {
    self.text(",", false)
  }
  self.text("]", false)
}

///|
fn Printer::object_pattern(self : Printer, offset : Int, count : Int) -> Unit {
  self.text("{", false)
  for index in 0..<count {
    if index > 0 {
      self.text(",", false)
    }
    self.pattern(self.program.edges[offset + index])
  }
  self.text("}", false)
}

///|
fn Printer::property_key(self : Printer, key : Int, is_computed : Bool) -> Unit {
  guard is_computed else {
    self.pattern_key(key)
    return
  }
  self.text("[", false)
  self.property_expression(key)
  self.text("]", false)
}

///|
fn Printer::pattern_property(self : Printer, property : Node) -> Unit {
  let key = property.a
  let value = property.b
  self.property_key(key, property.d == PROPERTY_COMPUTED)
  guard property.c == PROPERTY_SHORTHAND else {
    self.text(":", false)
    self.pattern(value)
    return
  }
  let value_node = self.program.nodes[value]
  let (binding, default_value) = if value_node.kind == PatternDefault {
    (value_node.a, value_node.b)
  } else {
    (value, -1)
  }
  let binding_node = self.program.nodes[binding]
  let symbol = if binding_node.kind == Identifier ||
    binding_node.kind == PatternIdentifier {
    self.program.references[binding_node.a].symbol
  } else {
    -1
  }
  let needs_expansion = self.is_renamed(symbol) ||
    self.program.property_names.contains(key)
  if needs_expansion {
    self.text(":", false)
    self.pattern(binding)
  }
  if default_value >= 0 {
    self.text("=", false)
    self.expression(default_value)
  }
}

///|
fn Printer::pattern_key(self : Printer, id : Int) -> Unit {
  guard self.program.property_names.get(id) is Some(name) else {
    let node = self.program.nodes[id]
    match node.kind {
      StringLiteral => self.string(node.a, node.b)
      NumberLiteral => self.number(node.a, node.b)
      _ => self.expression(id)
    }
    return
  }
  self.text(name, true)
}

///|
fn Printer::property(self : Printer, id : Int) -> Unit {
  let node = self.program.nodes[id]
  match node.kind {
    PatternRest => {
      self.text("...", false)
      self.expression(node.a)
    }
    Method => {
      let function = self.program.functions[node.b]
      self.method_prefix(node.c, function)
      self.property_key(node.a, node.d == PROPERTY_COMPUTED)
      self.parameters(function)
      self.isolated_block(function.body)
    }
    _ => self.data_property(node)
  }
}

///|
fn Printer::method_prefix(
  self : Printer,
  kind : Int,
  function : Function,
) -> Unit {
  guard kind != METHOD_GETTER else { return self.text("get", true) }
  guard kind != METHOD_SETTER else { return self.text("set", true) }
  if (function.flags & FUNCTION_ASYNC) != 0 {
    self.text("async", true)
  }
  guard (function.flags & FUNCTION_GENERATOR) != 0 else { return }
  self.text("*", false)
}

///|
fn Printer::data_property(self : Printer, property : Node) -> Unit {
  let key = property.a
  let value = property.b
  let is_shorthand = property.c == PROPERTY_SHORTHAND
  let expanded_proto = if is_shorthand {
    let reference = self.program.references[self.program.nodes[value].a]
    reference.name == NAME_PROTO && self.shorthand_value_changed(value)
  } else {
    false
  }
  if expanded_proto {
    self.text("[\"__proto__\"]", false)
  } else {
    self.property_key(key, property.d == PROPERTY_COMPUTED)
  }
  let needs_value = if is_shorthand {
    self.shorthand_value_changed(value) ||
    self.program.property_names.contains(key)
  } else {
    true
  }
  guard needs_value else { return }
  self.text(":", false)
  self.expression(value)
}

///|
fn Printer::shorthand_value_changed(self : Printer, id : Int) -> Bool {
  let reference = self.program.references[self.program.nodes[id].a]
  guard reference.symbol >= 0 else { return false }
  let symbol = self.program.symbols[reference.symbol]
  guard symbol.name_slot >= 0 else { return false }
  self.program.generated_names[symbol.name_slot] !=
  self.program.names[reference.name]
}

///|
fn Printer::parameters(self : Printer, function : Function) -> Unit {
  self.text("(", false)
  for i in 0..<function.parameters_count {
    if i > 0 {
      self.text(",", false)
    }
    self.pattern(self.program.edges[function.parameters_offset + i])
  }
  self.text(")", false)
}

///|
fn Printer::function(self : Printer, id : Int, with_name : Bool) -> Unit {
  let function = self.program.functions[id]
  if (function.flags & FUNCTION_ASYNC) != 0 {
    self.text("async", true)
  }
  guard (function.flags & FUNCTION_ARROW) == 0 else {
    self.arrow_function(function)
    return
  }
  self.text("function", true)
  if (function.flags & FUNCTION_GENERATOR) != 0 {
    self.text("*", false)
  }
  if with_name && function.name >= 0 {
    self.reference(function.name)
  }
  self.parameters(function)
  self.isolated_block(function.body)
}

///|
fn Printer::arrow_function(self : Printer, function : Function) -> Unit {
  let has_single_identifier = function.parameters_count == 1 &&
    self.program.nodes[self.program.edges[function.parameters_offset]].kind ==
    PatternIdentifier
  if has_single_identifier {
    self.pattern(self.program.edges[function.parameters_offset])
  } else {
    self.parameters(function)
  }
  self.text("=>", false)
  guard (function.flags & FUNCTION_EXPRESSION_BODY) != 0 else {
    self.isolated_block(function.body)
    return
  }
  self.expression(function.body)
}

///|
fn Printer::class(self : Printer, id : Int) -> Unit {
  let class = self.program.classes[id]
  self.text("class", true)
  if class.name >= 0 {
    self.reference(class.name)
  }
  if class.superclass >= 0 {
    self.text("extends", true)
    self.primary(class.superclass)
  }
  self.text("{", false)
  for i in 0..<class.members_count {
    self.class_member(self.program.edges[class.members_offset + i])
  }
  self.text("}", false)
}

///|
fn Printer::class_member(self : Printer, id : Int) -> Unit {
  let node = self.program.nodes[id]
  match node.kind {
    Method => {
      if (node.d & CLASS_MEMBER_STATIC) != 0 {
        self.text("static", true)
      }
      let function = self.program.functions[node.b]
      self.method_prefix(node.c, function)
      self.class_key(node)
      self.parameters(function)
      self.isolated_block(function.body)
    }
    ClassField => {
      if (node.d & CLASS_MEMBER_STATIC) != 0 {
        self.text("static", true)
      }
      self.class_key(node)
      if node.b >= 0 {
        self.text("=", false)
        self.expression(node.b)
      }
      self.text(";", false)
    }
    ClassStaticBlock => {
      self.text("static", true)
      self.isolated_block(node.a)
    }
    _ => abort("bad class member: \{node.kind.to_repr()}")
  }
}

///|
fn Printer::block(self : Printer, id : Int) -> Unit {
  let node = self.program.nodes[id]
  self.text("{", false)
  self.statements(node.a, node.b)
  self.pending_semicolon = false
  self.text("}", false)
}

///|
fn Printer::primary(self : Printer, id : Int) -> Unit {
  guard id >= 0 else { return }
  let node = self.program.nodes[id]
  let value = if node.kind is Parenthesized {
    self.program.nodes[self.program.unwrapped_expression(id)]
  } else {
    node
  }
  if value.kind is EvaluatedValue {
    self.detached_value(value.a)
    return
  }
  if node.kind is New {
    self.new_expression(node, true)
    return
  }
  let needs_parentheses = node.kind is BooleanLiteral ||
    (node.kind is Identifier && self.program.is_undefined_value(id))
  if needs_parentheses {
    self.text("(", false)
  }
  self.expression(id)
  if needs_parentheses {
    self.text(")", false)
  }
}

///|
fn Printer::expression(self : Printer, id : Int) -> Unit {
  guard id >= 0 else { return }
  let node = self.program.nodes[id]
  match node.kind {
    Identifier =>
      if self.program.is_undefined_value(id) {
        self.text("void", true)
        self.text("0", true)
      } else {
        self.reference(node.a)
      }
    NumberLiteral => self.number(node.a, node.b)
    ZeroLiteral => self.text("0", true)
    StringLiteral => self.string(node.a, node.b)
    RegexLiteral => self.span(node.a, node.b, true)
    TemplateNoSubstitution => self.span(node.a, node.b, false)
    BooleanLiteral => {
      let is_true = node.a == 1
      self.text("!", false)
      self.text(if is_true { "0" } else { "1" }, true)
    }
    NullLiteral => self.text("null", true)
    This => self.text("this", true)
    Super => self.text("super", true)
    ArrayLiteral => self.array_literal(node.a, node.b)
    ObjectLiteral => self.object_literal(node.a, node.b)
    FunctionExpression | FunctionDeclaration => self.function(node.a, true)
    ArrowExpression => self.function(node.a, false)
    ClassExpression | ClassDeclaration => self.class(node.a)
    Unary => {
      let operator = self.program.operators[node.a]
      self.text(operator.text(), operator.is_word())
      if operator is Delete {
        self.assignment_target(node.b)
      } else if operator is TypeOf &&
        self.program.nodes[self.program.unwrapped_expression(node.b)].kind
        is EvaluatedValue {
        self.detached_value(
          self.program.nodes[self.program.unwrapped_expression(node.b)].a,
        )
      } else {
        self.expression(node.b)
      }
    }
    Update =>
      self.update_expression(
        self.program.operators[node.a],
        node.c,
        node.b == UPDATE_PREFIX,
      )
    Binary | Logical =>
      self.binary_expression(self.program.operators[node.a], node.b, node.c)
    Assignment => {
      self.assignment_target(node.b)
      let operator = self.program.operators[node.a]
      if operator != Operator::Assignment {
        self.text(operator.text(), false)
      }
      self.text("=", false)
      self.expression(node.c)
    }
    Conditional => {
      self.expression(node.a)
      self.text("?", false)
      self.expression(node.b)
      self.text(":", false)
      self.expression(node.c)
    }
    Call => {
      self.primary(node.a)
      if node.d == CHAIN_OPTIONAL {
        self.text("?.", false)
      }
      self.text("(", false)
      self.expressions(node.b, node.c)
      self.text(")", false)
    }
    New => self.new_expression(node, false)
    Member => self.member_expression(id, node)
    Index => {
      self.primary(node.a)
      if node.d == CHAIN_OPTIONAL {
        self.text("?.", false)
      }
      self.text("[", false)
      self.property_expression(node.b)
      self.text("]", false)
    }
    Sequence => self.sequence_expression(id)
    Spread | PatternRest => {
      self.text("...", false)
      self.expression(node.a)
    }
    Parenthesized => {
      self.text("(", false)
      self.expression(node.a)
      self.text(")", false)
    }
    EvaluatedValue => {
      let value = self.program.nodes[self.program.unwrapped_expression(node.a)]
      if value.kind is Identifier && !self.program.is_undefined_value(node.a) {
        self.reference(value.a)
      } else if value.kind
        is (ArrowExpression | FunctionExpression | ClassExpression) {
        self.detached_value(node.a)
      } else {
        self.parenthesized_expression(node.a)
      }
    }
    Template => self.template_expression(node.a, node.b)
    TaggedTemplate => {
      self.primary(node.a)
      self.expression(node.b)
    }
    PatternIdentifier
    | PatternArray
    | PatternObject
    | PatternProperty
    | PatternDefault => self.pattern(id)
    _ => abort("bad expression: \{node.kind.to_repr()}")
  }
}

///|
/// Empty arguments may be omitted only when no surrounding primary operation
/// could attach to the constructor instead of the newly constructed value.
fn Printer::new_expression(
  self : Printer,
  node : Node,
  preserve_empty_arguments : Bool,
) -> Unit {
  self.text("new", true)
  self.primary(node.a)
  if node.b >= 0 && (node.c > 0 || preserve_empty_arguments) {
    self.text("(", false)
    self.expressions(node.b, node.c)
    self.text(")", false)
  }
}

///|
fn Printer::array_literal(self : Printer, offset : Int, count : Int) -> Unit {
  self.text("[", false)
  for index in 0..<count {
    if index > 0 {
      self.text(",", false)
    }
    let element = self.program.edges[offset + index]
    guard element >= 0 else { continue }
    self.expression(element)
  }
  self.array_end(offset, count)
}

///|
fn Printer::object_literal(self : Printer, offset : Int, count : Int) -> Unit {
  self.text("{", false)
  for index in 0..<count {
    if index > 0 {
      self.text(",", false)
    }
    self.property(self.program.edges[offset + index])
  }
  self.text("}", false)
}

///|
fn Printer::update_expression(
  self : Printer,
  operator : Operator,
  operand : Int,
  is_prefix : Bool,
) -> Unit {
  if is_prefix {
    self.text(operator.text(), false)
    self.assignment_target(operand)
  } else {
    self.assignment_target(operand)
    self.text(operator.text(), false)
  }
}

///|
fn Printer::assignment_target(self : Printer, id : Int) -> Unit {
  let node = self.program.nodes[id]
  match node.kind {
    EvaluatedValue => self.detached_value(node.a)
    Identifier | PatternIdentifier => self.reference(node.a)
    Parenthesized => {
      self.text("(", false)
      self.assignment_target(node.a)
      self.text(")", false)
    }
    _ => self.expression(id)
  }
}

///|
fn Printer::detached_value(self : Printer, id : Int) -> Unit {
  self.text("(0,", false)
  self.expression(id)
  self.text(")", false)
}

///|
fn Printer::binary_expression(
  self : Printer,
  operator : Operator,
  left : Int,
  right : Int,
) -> Unit {
  match operator {
    Exponent => self.primary(left)
    In => self.property_expression(left)
    _ => self.expression(left)
  }
  self.text(operator.text(), operator.is_word())
  if operator is (LogicalAnd | LogicalOr | NullishCoalescing) {
    let right_node = self.program.nodes[right]
    if right_node.kind is Parenthesized {
      let inner = self.program.nodes[right_node.a]
      if inner.kind is Logical && self.program.operators[inner.a] == operator {
        self.expression(right_node.a)
        return
      }
    }
  }
  self.expression(right)
}

///|
fn Printer::member_expression(
  self : Printer,
  id : Int,
  member_node : Node,
) -> Unit {
  let object = member_node.a
  let name_start = member_node.b
  let name_end = member_node.c
  guard object >= 0 else {
    self.text("new", true)
    self.text(".", false)
    self.span(name_start, name_end, true)
    return
  }
  let object_node = self.program.nodes[object]
  let object_kind = object_node.kind
  let needs_parentheses = object_kind == NumberLiteral ||
    object_kind == BooleanLiteral ||
    (object_kind is Identifier && self.program.is_undefined_value(object))
  if needs_parentheses {
    self.text("(", false)
  }
  if object_kind is New {
    self.new_expression(object_node, true)
  } else {
    self.expression(object)
  }
  if needs_parentheses {
    self.text(")", false)
  }
  self.text(if member_node.d == CHAIN_OPTIONAL { "?." } else { "." }, false)
  guard self.program.member_names.get(id) is Some(name) else {
    self.span(name_start, name_end, true)
    return
  }
  self.text(name, true)
}

///|
fn Printer::template_expression(
  self : Printer,
  offset : Int,
  count : Int,
) -> Unit {
  self.text("`", false)
  // Each literal span has two edges; each following interpolation has one.
  for part = 0, edge = offset {
    guard part < count else { break }
    let literal_start = self.program.edges[edge]
    let literal_end = self.program.edges[edge + 1]
    self.span(literal_start, literal_end, false)
    guard part < count - 1 else { continue part + 1, edge + 2 }
    let expression = self.program.edges[edge + 2]
    self.text("${", false)
    self.expression(expression)
    self.text("}", false)
    continue part + 1, edge + 3
  }
  self.text("`", false)
}

///|
fn NodeKind::needs_semicolon(kind : NodeKind) -> Bool {
  match kind {
    ExpressionStatement
    | VariableDeclaration
    | Return
    | Throw
    | Break
    | Continue
    | Debugger
    | DoWhile => true
    _ => false
  }
}

///|
fn Printer::statement(self : Printer, id : Int) -> Unit {
  let node = self.program.nodes[id]
  match node.kind {
    ExpressionStatement => self.expression_statement(node.a)
    ModuleStatement => self.module_stmt(node.a)
    Block => self.block(id)
    VariableDeclaration => self.declaration(node)
    Declarator => ()
    FunctionDeclaration => self.function(node.a, true)
    ClassDeclaration => self.class(node.a)
    Empty => self.text(";", false)
    Elided => ()
    Debugger => self.text("debugger", true)
    If => self.if_statement(node.a, node.b, node.c)
    For => self.for_statement(node)
    ForIn | ForOf => self.for_each_statement(node)
    While => {
      self.text("while", true)
      self.parenthesized_expression(node.a)
      self.statement_body(node.b, false)
    }
    DoWhile => {
      self.text("do", true)
      self.statement_body(node.a, false)
      self.text("while", true)
      self.parenthesized_expression(node.b)
    }
    Return => {
      self.text("return", true)
      if node.a >= 0 {
        self.expression(node.a)
      }
    }
    Throw => {
      self.text("throw", true)
      self.expression(node.a)
    }
    Break | Continue => {
      self.text(if node.kind == Break { "break" } else { "continue" }, true)
      if node.a >= 0 {
        self.label_reference(node.c, node.a, node.b)
      }
    }
    Labeled => self.labeled_statement(node.d, node.c)
    Switch => self.switch_statement(node.a, node.b, node.c)
    Try => self.try_statement(node.a, node.b, node.c)
    With => {
      self.text("with", true)
      self.parenthesized_expression(node.a)
      self.statement(node.b)
    }
    _ => abort("bad statement: \{node.kind.to_repr()}")
  }
  guard node.kind.needs_semicolon() else { return }
  self.pending_semicolon = true
}

///|
fn Printer::parenthesized_expression(self : Printer, expression : Int) -> Unit {
  self.text("(", false)
  self.expression(expression)
  self.text(")", false)
}

///|
/// An expression can become the start of a statement after value or branch
/// folding. Follow its first printed operand, keeping directive strings bare.
fn Printer::needs_statement_parentheses(self : Printer, id : Int) -> Bool {
  guard id >= 0 else { return false }
  let node = self.program.nodes[id]
  match node.kind {
    ObjectLiteral | FunctionExpression | ClassExpression | PatternObject => true
    Identifier | PatternIdentifier => {
      let reference = self.program.references[node.a]
      self.program.names[reference.name] == "let" &&
      (
        reference.symbol < 0 ||
        self.program.symbols[reference.symbol].name_slot < 0
      )
    }
    Member | Index | Call | Conditional | TaggedTemplate =>
      self.needs_statement_parentheses(node.a)
    Binary | Logical | Assignment => self.needs_statement_parentheses(node.b)
    Sequence if node.b > 0 =>
      self.needs_statement_parentheses(self.program.edges[node.a])
    Update if node.b != UPDATE_PREFIX =>
      self.needs_statement_parentheses(node.c)
    _ => false
  }
}

///|
fn Printer::expression_statement(self : Printer, id : Int) -> Unit {
  if self.needs_statement_parentheses(id) {
    self.parenthesized_expression(id)
  } else {
    self.expression(id)
  }
}

///|
fn Printer::statement_slot_expression(
  self : Printer,
  id : Int,
  context : SlotOperand,
) -> Unit {
  if self.needs_statement_parentheses(id) {
    self.parenthesized_expression(id)
  } else {
    self.slot_expression(id, context)
  }
}

///|
fn Printer::if_statement(
  self : Printer,
  condition : Int,
  consequent : Int,
  alternative : Int,
) -> Unit {
  let consequent_node = self.program.nodes[self.program.single_statement(
      consequent,
    )]
  let consequent_empty = self.program.empty_branch(consequent)
  let alternative_empty = self.program.empty_branch(alternative)
  if consequent_empty {
    if alternative_empty {
      // A bare string condition must not become a directive.
      self.parenthesized_expression(condition)
      self.pending_semicolon = true
    } else {
      let alternative_node = self.program.nodes[self.program.single_statement(
          alternative,
        )]
      if alternative_node.kind is ExpressionStatement {
        self.logical_statement(LogicalOr, condition, alternative_node.a)
      } else {
        self.if_statement(
          self.program.negated_value(condition),
          alternative,
          -1,
        )
      }
    }
    return
  }
  if alternative >= 0 && alternative_empty {
    self.if_statement(condition, consequent, -1)
    return
  }
  if alternative >= 0 &&
    self.same_effect_if_statement(condition, consequent, alternative) {
    return
  }
  if consequent_node.kind is ExpressionStatement {
    if alternative < 0 {
      self.logical_statement(LogicalAnd, condition, consequent_node.a)
      return
    }
    let alternative_node = self.program.nodes[self.program.single_statement(
        alternative,
      )]
    if alternative_node.kind is ExpressionStatement {
      let assignment = self.program.nodes[consequent_node.a]
      let symbol = if assignment.kind is Assignment {
        self.program.identifier_symbol(assignment.b)
      } else {
        -1
      }
      if symbol >= 0 && !self.program.has_dynamic_scope {
        let yes = self.program.slot_assignment(consequent, symbol)
        let no = self.program.slot_assignment(alternative, symbol)
        if yes >= 0 &&
          no >= 0 &&
          self.program.nodes[alternative_node.a].kind is Assignment {
          self.expression_statement(assignment.b)
          self.text("=", false)
          self.slot_expression(condition, ConditionalTest)
          self.text("?", false)
          self.slot_expression(yes, ConditionalArm)
          self.text(":", false)
          self.slot_expression(no, ConditionalArm)
          self.pending_semicolon = true
          return
        }
      }
      self.statement_slot_expression(condition, ConditionalTest)
      self.text("?", false)
      self.slot_expression(consequent_node.a, ConditionalArm)
      self.text(":", false)
      self.slot_expression(alternative_node.a, ConditionalArm)
      self.pending_semicolon = true
      return
    }
  }
  self.text("if", true)
  self.parenthesized_expression(condition)
  self.statement_body(consequent, alternative >= 0)
  guard alternative >= 0 else { return }
  self.text("else", true)
  self.statement_body(alternative, false)
}

///|
fn Program::empty_branch(self : Program, id : Int) -> Bool {
  id < 0 || self.is_empty_statement(self.single_statement(id))
}

///|
/// Extract one optional guard without moving declarations or allocating nodes.
/// The boolean says whether the effect runs when that guard is false.
fn Program::guarded_statement_effect(
  self : Program,
  id : Int,
) -> (Int, Bool, Int)? {
  let statement = self.nodes[self.single_statement(id)]
  match statement.kind {
    ExpressionStatement => {
      let value = self.nodes[self.unwrapped_expression(statement.a)]
      if value.kind is Logical {
        let operator = self.operators[value.a]
        if operator is (LogicalAnd | LogicalOr) {
          return Some((value.b, operator is LogicalOr, value.c))
        }
      }
      Some((-1, false, statement.a))
    }
    If => {
      let (condition, negate, body) = if self.empty_branch(statement.c) {
        (statement.a, false, statement.b)
      } else if self.empty_branch(statement.b) {
        (statement.a, true, statement.c)
      } else {
        return None
      }
      let effect = self.nodes[self.single_statement(body)]
      guard effect.kind is ExpressionStatement else { return None }
      Some((condition, negate, effect.a))
    }
    _ => None
  }
}

///|
fn Printer::same_effect_if_statement(
  self : Printer,
  condition : Int,
  consequent : Int,
  alternative : Int,
) -> Bool {
  let program = self.program
  guard !program.has_dynamic_scope else { return false }
  guard program.guarded_statement_effect(consequent)
    is Some((yes_guard, negate_yes, effect)) else {
    return false
  }
  guard program.guarded_statement_effect(alternative)
    is Some((no_guard, negate_no, other_effect)) &&
    program.same_value_expression(effect, other_effect) else {
    return false
  }
  if yes_guard < 0 && no_guard < 0 {
    self.expression_statement(program.sequence_pair(condition, effect))
    self.pending_semicolon = true
    return true
  }
  if no_guard < 0 && negate_yes {
    let skip_effect = program.slot_logical(LogicalAnd, condition, yes_guard)
    self.logical_statement(LogicalOr, skip_effect, effect)
    return true
  }
  let yes_condition = if negate_yes {
    program.negated_value(yes_guard)
  } else {
    yes_guard
  }
  let no_condition = if negate_no {
    program.negated_value(no_guard)
  } else {
    no_guard
  }
  let guard_condition = if yes_guard < 0 {
    program.slot_logical(LogicalOr, condition, no_condition)
  } else if no_guard < 0 {
    program.slot_logical(
      LogicalOr,
      program.negated_value(condition),
      yes_condition,
    )
  } else {
    program.compact_choice(condition, yes_condition, no_condition)
  }
  self.logical_statement(LogicalAnd, guard_condition, effect)
  true
}

///|
fn Printer::logical_statement(
  self : Printer,
  operator : Operator,
  left : Int,
  right : Int,
) -> Unit {
  self.statement_slot_expression(left, LogicalOperand(operator))
  self.text(operator.text(), false)
  self.slot_expression(right, LogicalOperand(operator))
  self.pending_semicolon = true
}

///|
fn Printer::slot_expression(
  self : Printer,
  id : Int,
  context : SlotOperand,
) -> Unit {
  if self.program.slot_needs_parentheses(id, context) {
    self.parenthesized_expression(id)
  } else {
    self.expression(id)
  }
}

///|
fn Program::always_exits(self : Program, id : Int) -> Bool {
  let node = self.nodes[id]
  match node.kind {
    Return | Throw | Break | Continue => true
    Block if node.b > 0 => self.always_exits(self.edges[node.a + node.b - 1])
    If => node.c >= 0 && self.always_exits(node.b) && self.always_exits(node.c)
    _ => false
  }
}

///|
fn Printer::statement_body(
  self : Printer,
  id : Int,
  protect_else : Bool,
) -> Unit {
  let statement = self.program.single_statement(id)
  let node = self.program.nodes[statement]
  if protect_else &&
    self.program.has_unmatched_if(statement) &&
    !(self.program.nodes[id].kind is Block) {
    self.text("{", false)
    self.statement(statement)
    self.pending_semicolon = false
    self.text("}", false)
    return
  }
  // Lexical declarations and block functions need their original environment.
  // Keep an unmatched inner if enclosed when an outer else follows it.
  let needs_block = match node.kind {
    VariableDeclaration => node.a != DECLARATION_VAR
    FunctionDeclaration | ClassDeclaration => true
    _ => protect_else && self.program.has_unmatched_if(statement)
  }
  self.statement(if needs_block { id } else { statement })
}

///|
fn Program::has_unmatched_if(self : Program, id : Int) -> Bool {
  let node = self.nodes[self.single_statement(id)]
  match node.kind {
    If =>
      self.empty_branch(node.b) ||
      self.empty_branch(node.c) ||
      self.has_unmatched_if(node.c)
    While | With => self.has_unmatched_if(node.b)
    For => self.has_unmatched_if(node.d)
    ForIn | ForOf => self.has_unmatched_if(node.c)
    Labeled => self.has_unmatched_if(node.c)
    _ => false
  }
}

///|
fn Printer::for_initializer(self : Printer, initializer : Int) -> Unit {
  let node = self.program.nodes[initializer]
  guard node.kind == VariableDeclaration else {
    return self.expression(initializer)
  }
  self.declaration(node)
}

///|
fn Printer::for_statement(self : Printer, statement : Node) -> Unit {
  let initializer = statement.a
  let condition = statement.b
  let update = statement.c
  let body = statement.d
  self.text("for", true)
  self.text("(", false)
  if initializer >= 0 {
    self.for_initializer(initializer)
  }
  self.text(";", false)
  self.expression(condition)
  self.text(";", false)
  self.expression(update)
  self.text(")", false)
  self.statement_body(body, false)
}

///|
fn Printer::for_each_statement(self : Printer, statement : Node) -> Unit {
  let binding = statement.a
  let iterable = statement.b
  let body = statement.c
  let is_await = statement.d == 1
  self.text("for", true)
  if is_await {
    self.text("await", true)
  }
  self.text("(", false)
  if self.program.nodes[binding].kind is VariableDeclaration {
    self.for_initializer(binding)
  } else {
    self.assignment_target(binding)
  }
  self.text(if statement.kind == ForIn { "in" } else { "of" }, true)
  self.expression(iterable)
  self.text(")", false)
  self.statement_body(body, false)
}

///|
fn Printer::labeled_statement(self : Printer, label : Int, body : Int) -> Unit {
  let name = self.label_name()
  self.labels.active.push((label, name))
  self.text(name, true)
  self.text(":", false)
  self.statement_body(body, false)
  ignore(self.labels.active.pop())
}

///|
fn Printer::switch_statement(
  self : Printer,
  discriminant : Int,
  offset : Int,
  count : Int,
) -> Unit {
  self.text("switch", true)
  self.text("(", false)
  self.expression(discriminant)
  self.text("){", false)
  for index in 0..<count {
    self.switch_case(self.program.edges[offset + index])
  }
  self.pending_semicolon = false
  self.text("}", false)
}

///|
fn Printer::try_statement(
  self : Printer,
  body : Int,
  handler : Int,
  finalizer : Int,
) -> Unit {
  self.text("try", true)
  self.block(body)
  if handler >= 0 {
    let catch_clause = self.program.nodes[handler]
    self.text("catch", true)
    if catch_clause.a >= 0 {
      self.text("(", false)
      self.pattern(catch_clause.a)
      self.text(")", false)
    }
    self.block(catch_clause.b)
  }
  guard finalizer >= 0 else { return }
  self.text("finally", true)
  self.block(finalizer)
}

///|
fn Printer::switch_case(self : Printer, id : Int) -> Unit {
  let node = self.program.nodes[id]
  if node.a >= 0 {
    self.text("case", true)
    self.expression(node.a)
  } else {
    self.text("default", true)
  }
  self.text(":", false)
  self.statements(node.b, node.c)
}

///|
fn Printer::declaration(self : Printer, node : Node) -> Unit {
  self.text(
    match node.a {
      DECLARATION_VAR => "var"
      DECLARATION_LET => "let"
      _ => "const"
    },
    true,
  )
  self.declarators(node.b, node.c)
}

///|
fn Printer::declarators(self : Printer, offset : Int, count : Int) -> Unit {
  for i in 0..<count {
    if i > 0 {
      self.text(",", false)
    }
    let declarator = self.program.nodes[self.program.edges[offset + i]]
    self.pattern(declarator.a)
    guard declarator.b >= 0 else { continue }
    self.text("=", false)
    self.expression(declarator.b)
  }
}

///|
fn Program::print(program : Program) -> String {
  let printer = Printer::new(program)
  let root = program.nodes[program.root]
  printer.statements(root.a, root.b)
  printer.flush_semicolon()
  printer.output.to_string()
}

///|
fn Printer::class_key(self : Printer, property : Node) -> Unit {
  self.property_key(property.a, (property.d & CLASS_MEMBER_COMPUTED) != 0)
}

///|
fn Printer::is_renamed(self : Printer, symbol : Int) -> Bool {
  guard symbol >= 0 else { return false }
  let binding = self.program.symbols[symbol]
  binding.name_slot >= 0 &&
  self.program.generated_names[binding.name_slot] !=
  self.program.names[binding.name]
}

///|
/// Statement folding builds a right-hand sequence spine. Print it iteratively
/// so a long run of expression statements does not grow the call stack.
fn Printer::sequence_expression(self : Printer, id : Int) -> Unit {
  for current = id {
    let node = self.program.nodes[current]
    guard node.b > 0 else { break }
    for index in 0..<(node.b - 1) {
      self.expression(self.program.edges[node.a + index])
      self.text(",", false)
    }
    let tail = self.program.edges[node.a + node.b - 1]
    guard self.program.nodes[tail].kind is Sequence else {
      self.expression(tail)
      break
    }
    continue tail
  }
}
