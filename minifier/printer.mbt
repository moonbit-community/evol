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
  mut last_word : Bool
  mut last_char : UInt16
}

///|
fn Printer::new(program : Program) -> Printer {
  { program, output: StringBuilder(), last_word: false, last_char: 0, }
}

///|
fn Printer::need_sep(self : Printer, first : UInt16, word : Bool) -> Bool {
  (self.last_word && word) ||
  (self.last_char == '+' && first == '+') ||
  (self.last_char == '-' && first == '-') ||
  (self.last_char == '!' && first == '-') ||
  (self.last_char == '/' && (first == '/' || first == '*'))
}

///|
fn Printer::text(self : Printer, s : String, word : Bool) -> Unit {
  if s.length() > 0 {
    if self.need_sep(s[0], word) {
      self.output.write_string(" ")
    }
    self.output.write_string(s)
    self.last_char = s[s.length() - 1]
    self.last_word = word
  }
}

///|
fn Printer::span(self : Printer, start : Int, end : Int, word : Bool) -> Unit {
  if end > start {
    let source = self.program.source
    if self.need_sep(source[start], word) {
      self.output.write_string(" ")
    }
    self.output.write_substring(source, start, end - start)
    self.last_char = source[end - 1]
    self.last_word = word
  }
}

///|
fn Printer::reference(self : Printer, id : Int) -> Unit {
  let r = self.program.references[id]
  let slot = if r.symbol >= 0 {
    self.program.symbols[r.symbol].name_slot
  } else {
    -1
  }
  if slot >= 0 {
    self.text(self.program.generated_names[slot], true)
  } else {
    self.span(r.name_start, r.name_end, true)
  }
}

///|
fn Printer::statements(self : Printer, offset : Int, count : Int) -> Unit {
  for i in 0..<count {
    self.statement(self.program.edges[offset + i])
  }
}

///|

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
  let n = self.program.nodes[id]
  match n.kind {
    PatternIdentifier | Identifier => self.reference(n.a)
    Member | Index => self.expression(id)
    PatternArray => {
      self.text("[", false)
      for i in 0..<n.b {
        if i > 0 {
          self.text(",", false)
        }
        let item = self.program.edges[n.a + i]
        if item >= 0 {
          self.pattern(item)
        }
      }
      if n.b > 0 && self.program.edges[n.a + n.b - 1] < 0 {
        self.text(",", false)
      }
      self.text("]", false)
    }
    PatternObject => {
      self.text("{", false)
      for i in 0..<n.b {
        if i > 0 {
          self.text(",", false)
        }
        self.pattern(self.program.edges[n.a + i])
      }
      self.text("}", false)
    }
    PatternProperty => {
      if n.d == 1 {
        self.text("[", false)
        self.expression(n.a)
        self.text("]", false)
      } else {
        self.pattern_key(n.a)
      }
      if n.c == 1 {
        let mut bind = n.b
        let mut default_value = -1
        if self.program.nodes[bind].kind == PatternDefault {
          default_value = self.program.nodes[bind].b
          bind = self.program.nodes[bind].a
        }
        let bind_node = self.program.nodes[bind]
        let symbol = if bind_node.kind == Identifier ||
          bind_node.kind == PatternIdentifier {
          self.program.references[bind_node.a].symbol
        } else {
          -1
        }
        if self.is_renamed(symbol) {
          self.text(":", false)
          self.text(
            self.program.generated_names[self.program.symbols[symbol].name_slot],
            true,
          )
        }
        if default_value >= 0 {
          self.text("=", false)
          self.expression(default_value)
        }
      } else {
        self.text(":", false)
        self.pattern(n.b)
      }
    }
    PatternRest => {
      self.text("...", false)
      self.pattern(n.a)
    }
    PatternDefault => {
      self.pattern(n.a)
      self.text("=", false)
      self.expression(n.b)
    }
    _ => abort("bad pattern: \{n.kind.to_repr()}")
  }
}

///|
fn Printer::pattern_key(self : Printer, id : Int) -> Unit {
  let n = self.program.nodes[id]
  match n.kind {
    StringLiteral | NumberLiteral => self.span(n.a, n.b, true)
    _ => self.expression(id)
  }
}

///|
fn Printer::property(self : Printer, id : Int) -> Unit {
  let n = self.program.nodes[id]
  if n.kind == PatternRest {
    self.text("...", false)
    self.expression(n.a)
    return
  }
  if n.kind == Method {
    let f = self.program.functions[n.b]
    if n.c == 1 {
      self.text("get", true)
    } else if n.c == 2 {
      self.text("set", true)
    } else {
      if (f.flags & FUNCTION_ASYNC) != 0 {
        self.text("async", true)
      }
      if (f.flags & FUNCTION_GENERATOR) != 0 {
        self.text("*", false)
      }
    }
    if n.d == 1 {
      self.text("[", false)
      self.expression(n.a)
      self.text("]", false)
    } else {
      self.pattern_key(n.a)
    }
    self.parameters(f)
    self.block(f.body)
    return
  }
  let expanded_proto = if n.c == 1 {
    let reference = self.program.references[self.program.nodes[n.b].a]
    reference.name == name_proto && self.is_renamed(reference.symbol)
  } else {
    false
  }
  if expanded_proto {
    self.text("[\"__proto__\"]", false)
  } else if n.d == 1 {
    self.text("[", false)
    self.expression(n.a)
    self.text("]", false)
  } else {
    self.pattern_key(n.a)
  }
  if n.c == 1 {
    let symbol = self.program.references[self.program.nodes[n.b].a].symbol
    if self.is_renamed(symbol) {
      self.text(":", false)
      self.text(
        self.program.generated_names[self.program.symbols[symbol].name_slot],
        true,
      )
    }
  } else {
    self.text(":", false)
    self.expression(n.b)
  }
}

///|
fn Printer::parameters(self : Printer, f : Function) -> Unit {
  self.text("(", false)
  for i in 0..<f.parameters_count {
    if i > 0 {
      self.text(",", false)
    }
    self.pattern(self.program.edges[f.parameters_offset + i])
  }
  self.text(")", false)
}

///|
fn Printer::function(self : Printer, id : Int, with_name : Bool) -> Unit {
  let f = self.program.functions[id]
  if (f.flags & FUNCTION_ARROW) != 0 {
    if (f.flags & FUNCTION_ASYNC) != 0 {
      self.text("async", true)
    }
    self.parameters(f)
    self.text("=>", false)
    if (f.flags & FUNCTION_EXPRESSION_BODY) != 0 {
      self.expression(f.body)
    } else {
      self.block(f.body)
    }
    return
  }
  if (f.flags & FUNCTION_ASYNC) != 0 {
    self.text("async", true)
  }
  self.text("function", true)
  if (f.flags & FUNCTION_GENERATOR) != 0 {
    self.text("*", false)
  }
  if with_name && f.name >= 0 {
    self.reference(f.name)
  }
  self.parameters(f)
  self.block(f.body)
}

///|
fn Printer::class(self : Printer, id : Int) -> Unit {
  let c = self.program.classes[id]
  self.text("class", true)
  if c.name >= 0 {
    self.reference(c.name)
  }
  if c.superclass >= 0 {
    self.text("extends", true)
    self.expression(c.superclass)
  }
  self.text("{", false)
  for i in 0..<c.members_count {
    self.class_member(self.program.edges[c.members_offset + i])
  }
  self.text("}", false)
}

///|
fn Printer::class_member(self : Printer, id : Int) -> Unit {
  let n = self.program.nodes[id]
  match n.kind {
    Method => {
      if (n.d & 1) != 0 {
        self.text("static", true)
      }
      let f = self.program.functions[n.b]
      if n.c == 1 {
        self.text("get", true)
      } else if n.c == 2 {
        self.text("set", true)
      } else {
        if (f.flags & FUNCTION_ASYNC) != 0 {
          self.text("async", true)
        }
        if (f.flags & FUNCTION_GENERATOR) != 0 {
          self.text("*", false)
        }
      }
      self.class_key(n)
      self.parameters(f)
      self.block(f.body)
    }
    ClassField => {
      if (n.d & 1) != 0 {
        self.text("static", true)
      }
      self.class_key(n)
      if n.b >= 0 {
        self.text("=", false)
        self.expression(n.b)
      }
      self.text(";", false)
    }
    ClassStaticBlock => {
      self.text("static", true)
      self.block(n.a)
    }
    _ => abort("bad class member: \{n.kind.to_repr()}")
  }
}

///|
fn Printer::block(self : Printer, id : Int) -> Unit {
  let n = self.program.nodes[id]
  self.text("{", false)
  self.statements(n.a, n.b)
  self.text("}", false)
}

///|
fn Printer::expression(self : Printer, id : Int) -> Unit {
  if id < 0 {
    return
  }
  let n = self.program.nodes[id]
  match n.kind {
    Identifier => self.reference(n.a)
    NumberLiteral | StringLiteral | RegexLiteral | TemplateNoSubstitution =>
      self.span(n.a, n.b, true)
    BooleanLiteral => self.text(if n.a == 1 { "true" } else { "false" }, true)
    NullLiteral => self.text("null", true)
    This => self.text("this", true)
    Super => self.text("super", true)
    ArrayLiteral => {
      self.text("[", false)
      for i in 0..<n.b {
        if i > 0 {
          self.text(",", false)
        }
        let item = self.program.edges[n.a + i]
        if item >= 0 {
          self.expression(item)
        }
      }
      if n.b > 0 && self.program.edges[n.a + n.b - 1] < 0 {
        self.text(",", false)
      }
      self.text("]", false)
    }
    ObjectLiteral => {
      self.text("{", false)
      for i in 0..<n.b {
        if i > 0 {
          self.text(",", false)
        }
        self.property(self.program.edges[n.a + i])
      }
      self.text("}", false)
    }
    FunctionExpression => self.function(n.a, true)
    FunctionDeclaration => self.function(n.a, true)
    ArrowExpression => self.function(n.a, false)
    ClassExpression => self.class(n.a)
    ClassDeclaration => self.class(n.a)
    Unary => {
      let operator = self.program.operators[n.a]
      self.text(operator_text(operator), operator_is_word(operator))
      self.expression(n.b)
    }
    Update => {
      let operator = self.program.operators[n.a]
      if n.b == 1 {
        self.text(operator_text(operator), false)
        self.expression(n.c)
      } else {
        self.expression(n.c)
        self.text(operator_text(operator), false)
      }
    }
    Binary | Logical => {
      self.expression(n.b)
      self.text(
        operator_text(self.program.operators[n.a]),
        operator_is_word(self.program.operators[n.a]),
      )
      self.expression(n.c)
    }
    Assignment => {
      self.expression(n.b)
      let operator = self.program.operators[n.a]
      if operator == Operator::Assignment {
        self.text("=", false)
      } else {
        self.text(operator_text(operator), false)
        self.text("=", false)
      }
      self.expression(n.c)
    }
    Conditional => {
      self.expression(n.a)
      self.text("?", false)
      self.expression(n.b)
      self.text(":", false)
      self.expression(n.c)
    }
    Call => {
      self.expression(n.a)
      if n.d == 1 {
        self.text("?.", false)
      }
      self.text("(", false)
      self.expressions(n.b, n.c)
      self.text(")", false)
    }
    New => {
      self.text("new", true)
      self.expression(n.a)
      if n.b >= 0 {
        self.text("(", false)
        self.expressions(n.b, n.c)
        self.text(")", false)
      }
    }
    Member =>
      if n.a < 0 {
        self.text("new", true)
        self.text(".", false)
        self.span(n.b, n.c, true)
      } else {
        let numeric = self.program.nodes[n.a].kind == NumberLiteral
        if numeric {
          self.text("(", false)
        }
        self.expression(n.a)
        if numeric {
          self.text(")", false)
        }
        if n.d == 1 {
          self.text("?.", false)
        } else {
          self.text(".", false)
        }
        self.span(n.b, n.c, true)
      }
    Index => {
      self.expression(n.a)
      if n.d == 1 {
        self.text("?.", false)
      }
      self.text("[", false)
      self.expression(n.b)
      self.text("]", false)
    }
    Sequence => self.expressions(n.a, n.b)
    Spread => {
      self.text("...", false)
      self.expression(n.a)
    }
    Parenthesized => {
      self.text("(", false)
      self.expression(n.a)
      self.text(")", false)
    }
    Template => {
      self.text("`", false)
      let mut index = n.a
      for q in 0..<n.b {
        self.span(
          self.program.edges[index],
          self.program.edges[index + 1],
          false,
        )
        index = index + 2
        if q < n.b - 1 {
          self.text("${", false)
          self.expression(self.program.edges[index])
          index = index + 1
          self.text("}", false)
        }
      }
      self.text("`", false)
    }
    PatternRest => {
      self.text("...", false)
      self.expression(n.a)
    }
    TaggedTemplate => {
      self.expression(n.a)
      self.expression(n.b)
    }
    PatternIdentifier
    | PatternArray
    | PatternObject
    | PatternProperty
    | PatternDefault => self.pattern(id)
    _ => abort("bad expression: \{n.kind.to_repr()}")
  }
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
    | Empty
    | DoWhile => true
    _ => false
  }
}

///|
fn Printer::statement(self : Printer, id : Int) -> Unit {
  let n = self.program.nodes[id]
  match n.kind {
    ExpressionStatement => self.expression(n.a)
    ModuleStatement => self.module_stmt(n.a)
    Block => {
      self.block(id)
      return
    }
    VariableDeclaration => self.declaration(n)
    Declarator => ()
    FunctionDeclaration => self.function(n.a, true)
    ClassDeclaration => self.class(n.a)
    Empty => ()
    Debugger => self.text("debugger", true)
    If => {
      self.text("if", true)
      self.text("(", false)
      self.expression(n.a)
      self.text(")", false)
      self.statement(n.b)
      if n.c >= 0 {
        self.text("else", true)
        self.statement(n.c)
      }
      return
    }
    For => {
      self.text("for", true)
      self.text("(", false)
      if n.a >= 0 {
        let initializer = self.program.nodes[n.a]
        if initializer.kind == VariableDeclaration {
          self.declaration(initializer)
        } else {
          self.expression(n.a)
        }
      }
      self.text(";", false)
      if n.b >= 0 {
        self.expression(n.b)
      }
      self.text(";", false)
      if n.c >= 0 {
        self.expression(n.c)
      }
      self.text(")", false)
      self.statement(n.d)
      return
    }
    ForIn | ForOf => {
      self.text("for", true)
      if n.d == 1 {
        self.text("await", true)
      }
      self.text("(", false)
      let left = self.program.nodes[n.a]
      if left.kind == VariableDeclaration {
        self.declaration(left)
      } else {
        self.expression(n.a)
      }
      self.text(if n.kind == ForIn { "in" } else { "of" }, true)
      self.expression(n.b)
      self.text(")", false)
      self.statement(n.c)
      return
    }
    While => {
      self.text("while", true)
      self.text("(", false)
      self.expression(n.a)
      self.text(")", false)
      self.statement(n.b)
      return
    }
    DoWhile => {
      self.text("do", true)
      self.statement(n.a)
      self.text("while", true)
      self.text("(", false)
      self.expression(n.b)
      self.text(")", false)
    }
    Return => {
      self.text("return", true)
      if n.a >= 0 {
        self.expression(n.a)
      }
    }
    Throw => {
      self.text("throw", true)
      self.expression(n.a)
    }
    Break | Continue => {
      self.text(if n.kind == Break { "break" } else { "continue" }, true)
      if n.a >= 0 {
        self.span(n.a, n.b, true)
      }
    }
    Labeled => {
      self.span(n.a, n.b, true)
      self.text(":", false)
      self.statement(n.c)
      return
    }
    Switch => {
      self.text("switch", true)
      self.text("(", false)
      self.expression(n.a)
      self.text("){", false)
      for i in 0..<n.c {
        self.switch_case(self.program.edges[n.b + i])
      }
      self.text("}", false)
      return
    }
    Try => {
      self.text("try", true)
      self.block(n.a)
      if n.b >= 0 {
        let c = self.program.nodes[n.b]
        self.text("catch", true)
        if c.a >= 0 {
          self.text("(", false)
          self.pattern(c.a)
          self.text(")", false)
        }
        self.block(c.b)
      }
      if n.c >= 0 {
        self.text("finally", true)
        self.block(n.c)
      }
      return
    }
    With => {
      self.text("with", true)
      self.text("(", false)
      self.expression(n.a)
      self.text(")", false)
      self.statement(n.b)
      return
    }
    _ => abort("bad statement: \{n.kind.to_repr()}")
  }
  if needs_semicolon(n.kind) {
    self.text(";", false)
  }
}

///|
fn Printer::switch_case(self : Printer, id : Int) -> Unit {
  let n = self.program.nodes[id]
  if n.a >= 0 {
    self.text("case", true)
    self.expression(n.a)
  } else {
    self.text("default", true)
  }
  self.text(":", false)
  self.statements(n.b, n.c)
}

///|
fn Printer::declaration(self : Printer, n : Node) -> Unit {
  self.text(
    if n.a == 0 {
      "var"
    } else if n.a == 1 {
      "let"
    } else {
      "const"
    },
    true,
  )
  for i in 0..<n.c {
    if i > 0 {
      self.text(",", false)
    }
    let d = self.program.nodes[self.program.edges[n.b + i]]
    self.pattern(d.a)
    if d.b >= 0 {
      self.text("=", false)
      self.expression(d.b)
    }
  }
}

///|
fn print(program : Program) -> String {
  let p = Printer::new(program)
  let root = program.nodes[program.root]
  p.statements(root.a, root.b)
  p.output.to_string()
}

///|
fn Printer::class_key(self : Printer, property : Node) -> Unit {
  if (property.d & 2) != 0 {
    self.text("[", false)
    self.expression(property.a)
    self.text("]", false)
  } else {
    self.pattern_key(property.a)
  }
}

///|
fn Printer::is_renamed(self : Printer, symbol : Int) -> Bool {
  guard symbol >= 0 else { return false }
  let binding = self.program.symbols[symbol]
  binding.name_slot >= 0 &&
  self.program.generated_names[binding.name_slot] !=
  self.program.names[binding.name]
}
