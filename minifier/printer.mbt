///|
fn operator_text(operator : Operator) -> String {
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
fn operator_is_word(operator : Operator) -> Bool {
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
  }
}

///|
fn Printer::flush_semicolon(self : Printer) -> Unit {
  if self.pending_semicolon {
    self.output.write_string(";")
    self.previous_is_word = false
    self.last_character = ';'
    self.pending_semicolon = false
  }
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
  if slot >= 0 {
    self.text(self.program.generated_names[slot], true)
  } else {
    self.span(reference.name_start, reference.name_end, true)
  }
}

///|
fn Printer::statements(self : Printer, offset : Int, count : Int) -> Unit {
  for i in 0..<count {
    self.statement(self.program.edges[offset + i])
  }
}

///|
fn Printer::expressions(self : Printer, offset : Int, count : Int) -> Unit {
  for i in 0..<count {
    if i > 0 {
      self.text(",", false)
    }
    self.expression(self.program.edges[offset + i])
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
    if element >= 0 {
      self.pattern(element)
    }
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
  if is_computed {
    self.text("[", false)
    self.property_expression(key)
    self.text("]", false)
  } else {
    self.pattern_key(key)
  }
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
  if self.program.property_names.get(id) is Some(name) {
    self.text(name, true)
    return
  }
  let node = self.program.nodes[id]
  match node.kind {
    StringLiteral => self.string(node.a, node.b)
    NumberLiteral => self.number(node.a, node.b)
    _ => self.expression(id)
  }
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
  if kind == METHOD_GETTER {
    self.text("get", true)
  } else if kind == METHOD_SETTER {
    self.text("set", true)
  } else {
    if (function.flags & FUNCTION_ASYNC) != 0 {
      self.text("async", true)
    }
    if (function.flags & FUNCTION_GENERATOR) != 0 {
      self.text("*", false)
    }
  }
}

///|
fn Printer::data_property(self : Printer, property : Node) -> Unit {
  let key = property.a
  let value = property.b
  let is_shorthand = property.c == PROPERTY_SHORTHAND
  let expanded_proto = if is_shorthand {
    let reference = self.program.references[self.program.nodes[value].a]
    reference.name == name_proto && self.is_renamed(reference.symbol)
  } else {
    false
  }
  if expanded_proto {
    self.text("[\"__proto__\"]", false)
  } else {
    self.property_key(key, property.d == PROPERTY_COMPUTED)
  }
  let needs_value = if is_shorthand {
    let symbol = self.program.references[self.program.nodes[value].a].symbol
    self.is_renamed(symbol) || self.program.property_names.contains(key)
  } else {
    true
  }
  if needs_value {
    self.text(":", false)
    self.expression(value)
  }
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
  if (function.flags & FUNCTION_EXPRESSION_BODY) != 0 {
    self.expression(function.body)
  } else {
    self.isolated_block(function.body)
  }
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
  let needs_parentheses = id >= 0 &&
    self.program.nodes[id].kind == BooleanLiteral
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
    Identifier => self.reference(node.a)
    NumberLiteral => self.number(node.a, node.b)
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
      self.text(operator_text(operator), operator_is_word(operator))
      self.expression(node.b)
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
      self.expression(node.b)
      let operator = self.program.operators[node.a]
      if operator != Operator::Assignment {
        self.text(operator_text(operator), false)
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
    New => {
      self.text("new", true)
      self.primary(node.a)
      if node.b >= 0 {
        self.text("(", false)
        self.expressions(node.b, node.c)
        self.text(")", false)
      }
    }
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
    Sequence => self.expressions(node.a, node.b)
    Spread | PatternRest => {
      self.text("...", false)
      self.expression(node.a)
    }
    Parenthesized => {
      self.text("(", false)
      self.expression(node.a)
      self.text(")", false)
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
fn Printer::array_literal(self : Printer, offset : Int, count : Int) -> Unit {
  self.text("[", false)
  for index in 0..<count {
    if index > 0 {
      self.text(",", false)
    }
    let element = self.program.edges[offset + index]
    if element >= 0 {
      self.expression(element)
    }
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
    self.text(operator_text(operator), false)
    self.expression(operand)
  } else {
    self.expression(operand)
    self.text(operator_text(operator), false)
  }
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
  self.text(operator_text(operator), operator_is_word(operator))
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
  let object_kind = self.program.nodes[object].kind
  let needs_parentheses = object_kind == NumberLiteral ||
    object_kind == BooleanLiteral
  if needs_parentheses {
    self.text("(", false)
  }
  self.expression(object)
  if needs_parentheses {
    self.text(")", false)
  }
  self.text(if member_node.d == CHAIN_OPTIONAL { "?." } else { "." }, false)
  if self.program.member_names.get(id) is Some(name) {
    self.text(name, true)
  } else {
    self.span(name_start, name_end, true)
  }
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
    if part < count - 1 {
      let expression = self.program.edges[edge + 2]
      self.text("${", false)
      self.expression(expression)
      self.text("}", false)
      continue part + 1, edge + 3
    }
    continue part + 1, edge + 2
  }
  self.text("`", false)
}

///|
fn needs_semicolon(kind : NodeKind) -> Bool {
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
    ExpressionStatement => self.expression(node.a)
    ModuleStatement => self.module_stmt(node.a)
    Block => self.block(id)
    VariableDeclaration => self.declaration(node)
    Declarator => ()
    FunctionDeclaration => self.function(node.a, true)
    ClassDeclaration => self.class(node.a)
    Empty => self.text(";", false)
    Debugger => self.text("debugger", true)
    If => self.if_statement(node.a, node.b, node.c)
    For => self.for_statement(node)
    ForIn | ForOf => self.for_each_statement(node)
    While => {
      self.text("while", true)
      self.parenthesized_expression(node.a)
      self.statement(node.b)
    }
    DoWhile => {
      self.text("do", true)
      self.statement(node.a)
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
  if needs_semicolon(node.kind) {
    self.pending_semicolon = true
  }
}

///|
fn Printer::parenthesized_expression(self : Printer, expression : Int) -> Unit {
  self.text("(", false)
  self.expression(expression)
  self.text(")", false)
}

///|
fn Printer::if_statement(
  self : Printer,
  condition : Int,
  consequent : Int,
  alternative : Int,
) -> Unit {
  self.text("if", true)
  self.parenthesized_expression(condition)
  self.statement(consequent)
  if alternative >= 0 {
    self.text("else", true)
    self.statement(alternative)
  }
}

///|
fn Printer::for_initializer(self : Printer, initializer : Int) -> Unit {
  let node = self.program.nodes[initializer]
  if node.kind == VariableDeclaration {
    self.declaration(node)
  } else {
    self.expression(initializer)
  }
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
  self.statement(body)
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
  self.for_initializer(binding)
  self.text(if statement.kind == ForIn { "in" } else { "of" }, true)
  self.expression(iterable)
  self.text(")", false)
  self.statement(body)
}

///|
fn Printer::labeled_statement(self : Printer, label : Int, body : Int) -> Unit {
  let name = self.label_name()
  self.labels.active.push((label, name))
  self.text(name, true)
  self.text(":", false)
  self.statement(body)
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
  if finalizer >= 0 {
    self.text("finally", true)
    self.block(finalizer)
  }
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
    if node.a == DECLARATION_VAR {
      "var"
    } else if node.a == DECLARATION_LET {
      "let"
    } else {
      "const"
    },
    true,
  )
  for i in 0..<node.c {
    if i > 0 {
      self.text(",", false)
    }
    let declarator = self.program.nodes[self.program.edges[node.b + i]]
    self.pattern(declarator.a)
    if declarator.b >= 0 {
      self.text("=", false)
      self.expression(declarator.b)
    }
  }
}

///|
fn print(program : Program) -> String {
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
