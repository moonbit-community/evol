///|
fn op_text(op : Op) -> String {
  match op {
    Assign => "="
    Add => "+"
    Sub => "-"
    Mul => "*"
    Div => "/"
    Mod => "%"
    Exp => "**"
    Shl => "<<"
    Shr => ">>"
    Ushr => ">>>"
    Lt => "<"
    Gt => ">"
    Le => "<="
    Ge => ">="
    Eq => "=="
    Ne => "!="
    StrictEq => "==="
    StrictNe => "!=="
    BitAnd => "&"
    BitOr => "|"
    BitXor => "^"
    LogicalAnd => "&&"
    LogicalOr => "||"
    Nullish => "??"
    In => "in"
    Instanceof => "instanceof"
    Not => "!"
    BitNot => "~"
    Plus => "+"
    Minus => "-"
    Typeof => "typeof"
    Void => "void"
    Delete => "delete"
    Await => "await"
    Yield => "yield"
    YieldStar => "yield*"
    Inc => "++"
    Dec => "--"
  }
}

///|
fn op_is_word(op : Op) -> Bool {
  match op {
    In | Instanceof | Typeof | Void | Delete | Await | Yield | YieldStar => true
    _ => false
  }
}

///|
priv struct Printer {
  prog : Program
  out : StringBuilder
  mut prev_word : Bool
  mut prev_last : UInt16
}

///|
fn Printer::new(prog : Program) -> Printer {
  { prog, out: StringBuilder(), prev_word: false, prev_last: 0, }
}

///|
fn Printer::need_sep(self : Printer, first : UInt16, word : Bool) -> Bool {
  (self.prev_word && word) ||
  (self.prev_last == '+' && first == '+') ||
  (self.prev_last == '-' && first == '-') ||
  (self.prev_last == '!' && first == '-') ||
  (self.prev_last == '/' && (first == '/' || first == '*'))
}

///|
fn Printer::write_str(self : Printer, s : String, word : Bool) -> Unit {
  if s.length() > 0 {
    if self.need_sep(s[0], word) {
      self.out.write_string(" ")
    }
    self.out.write_string(s)
    self.prev_last = s[s.length() - 1]
    self.prev_word = word
  }
}

///|
fn Printer::write_span(
  self : Printer,
  start : Int,
  end : Int,
  word : Bool,
) -> Unit {
  if end > start {
    let src = self.prog.src
    if self.need_sep(src[start], word) {
      self.out.write_string(" ")
    }
    self.out.write_substring(src, start, end - start)
    self.prev_last = src[end - 1]
    self.prev_word = word
  }
}

///|
fn Printer::write_ref(self : Printer, ref_id : Int) -> Unit {
  let r = self.prog.refs[ref_id]
  let slot = if r.sym >= 0 { self.prog.symbols[r.sym].name_slot } else { -1 }
  if slot >= 0 {
    self.write_str(self.prog.generated_names[slot], true)
  } else {
    self.write_span(r.name_start, r.name_end, true)
  }
}

///|
fn Printer::stmts(
  self : Printer,
  off : Int,
  count : Int,
) -> Unit raise ParseError {
  for i in 0..<count {
    self.stmt(self.prog.edges[off + i])
  }
}

///|

///|
fn Printer::expr_list(
  self : Printer,
  off : Int,
  count : Int,
) -> Unit raise ParseError {
  for i in 0..<count {
    if i > 0 {
      self.write_str(",", false)
    }
    self.expr(self.prog.edges[off + i])
  }
}

///|
fn Printer::pat(self : Printer, id : Int) -> Unit raise ParseError {
  let n = self.prog.nodes[id]
  match n.kind {
    PatIdent | Ident => self.write_ref(n.a)
    Member | Index => self.expr(id)
    PatArray => {
      self.write_str("[", false)
      for i in 0..<n.b {
        if i > 0 {
          self.write_str(",", false)
        }
        let item = self.prog.edges[n.a + i]
        if item >= 0 {
          self.pat(item)
        }
      }
      if n.b > 0 && self.prog.edges[n.a + n.b - 1] < 0 {
        self.write_str(",", false)
      }
      self.write_str("]", false)
    }
    PatObject => {
      self.write_str("{", false)
      for i in 0..<n.b {
        if i > 0 {
          self.write_str(",", false)
        }
        self.pat(self.prog.edges[n.a + i])
      }
      self.write_str("}", false)
    }
    PatProp => {
      if n.d == 1 {
        self.write_str("[", false)
        self.expr(n.a)
        self.write_str("]", false)
      } else {
        self.pat_key(n.a)
      }
      if n.c == 1 {
        let mut bind = n.b
        let mut dflt = -1
        if self.prog.nodes[bind].kind == PatDefault {
          dflt = self.prog.nodes[bind].b
          bind = self.prog.nodes[bind].a
        }
        let bind_node = self.prog.nodes[bind]
        let sym = if bind_node.kind == Ident || bind_node.kind == PatIdent {
          self.prog.refs[bind_node.a].sym
        } else {
          -1
        }
        if self.binding_renamed(sym) {
          self.write_str(":", false)
          self.write_str(
            self.prog.generated_names[self.prog.symbols[sym].name_slot],
            true,
          )
        }
        if dflt >= 0 {
          self.write_str("=", false)
          self.expr(dflt)
        }
      } else {
        self.write_str(":", false)
        self.pat(n.b)
      }
    }
    PatRest => {
      self.write_str("...", false)
      self.pat(n.a)
    }
    PatDefault => {
      self.pat(n.a)
      self.write_str("=", false)
      self.expr(n.b)
    }
    _ => raise Unexpected("bad pattern: " + n.kind.to_repr().to_string())
  }
}

///|
fn Printer::pat_key(self : Printer, id : Int) -> Unit raise ParseError {
  let n = self.prog.nodes[id]
  match n.kind {
    StringLit | NumberLit => self.write_span(n.a, n.b, true)
    _ => self.expr(id)
  }
}

///|
fn Printer::prop(self : Printer, id : Int) -> Unit raise ParseError {
  let n = self.prog.nodes[id]
  if n.kind == PatRest {
    self.write_str("...", false)
    self.expr(n.a)
    return
  }
  if n.kind == Method {
    let f = self.prog.funcs[n.b]
    if n.c == 1 {
      self.write_str("get", true)
    } else if n.c == 2 {
      self.write_str("set", true)
    } else {
      if (f.flags & fn_async) != 0 {
        self.write_str("async", true)
      }
      if (f.flags & fn_generator) != 0 {
        self.write_str("*", false)
      }
    }
    if n.d == 1 {
      self.write_str("[", false)
      self.expr(n.a)
      self.write_str("]", false)
    } else {
      self.pat_key(n.a)
    }
    self.params(f)
    self.block(f.body)
    return
  }
  let expanded_proto = if n.c == 1 {
    let reference = self.prog.refs[self.prog.nodes[n.b].a]
    reference.name == name_proto && self.binding_renamed(reference.sym)
  } else {
    false
  }
  if expanded_proto {
    self.write_str("[\"__proto__\"]", false)
  } else if n.d == 1 {
    self.write_str("[", false)
    self.expr(n.a)
    self.write_str("]", false)
  } else {
    self.pat_key(n.a)
  }
  if n.c == 1 {
    let sym = self.prog.refs[self.prog.nodes[n.b].a].sym
    if self.binding_renamed(sym) {
      self.write_str(":", false)
      self.write_str(
        self.prog.generated_names[self.prog.symbols[sym].name_slot],
        true,
      )
    }
  } else {
    self.write_str(":", false)
    self.expr(n.b)
  }
}

///|
fn Printer::params(self : Printer, f : Func) -> Unit raise ParseError {
  self.write_str("(", false)
  for i in 0..<f.params_count {
    if i > 0 {
      self.write_str(",", false)
    }
    self.pat(self.prog.edges[f.params_off + i])
  }
  self.write_str(")", false)
}

///|
fn Printer::func(
  self : Printer,
  id : Int,
  with_name : Bool,
) -> Unit raise ParseError {
  let f = self.prog.funcs[id]
  if (f.flags & fn_arrow) != 0 {
    if (f.flags & fn_async) != 0 {
      self.write_str("async", true)
    }
    self.params(f)
    self.write_str("=>", false)
    if (f.flags & fn_expr_body) != 0 {
      self.expr(f.body)
    } else {
      self.block(f.body)
    }
    return
  }
  if (f.flags & fn_async) != 0 {
    self.write_str("async", true)
  }
  self.write_str("function", true)
  if (f.flags & fn_generator) != 0 {
    self.write_str("*", false)
  }
  if with_name && f.name >= 0 {
    self.write_ref(f.name)
  }
  self.params(f)
  self.block(f.body)
}

///|
fn Printer::class(self : Printer, id : Int) -> Unit raise ParseError {
  let c = self.prog.classes[id]
  self.write_str("class", true)
  if c.name >= 0 {
    self.write_ref(c.name)
  }
  if c.superclass >= 0 {
    self.write_str("extends", true)
    self.expr(c.superclass)
  }
  self.write_str("{", false)
  for i in 0..<c.members_count {
    self.class_member(self.prog.edges[c.members_off + i])
  }
  self.write_str("}", false)
}

///|
fn Printer::class_member(self : Printer, id : Int) -> Unit raise ParseError {
  let n = self.prog.nodes[id]
  match n.kind {
    Method => {
      if (n.d & 1) != 0 {
        self.write_str("static", true)
      }
      let f = self.prog.funcs[n.b]
      if n.c == 1 {
        self.write_str("get", true)
      } else if n.c == 2 {
        self.write_str("set", true)
      } else {
        if (f.flags & fn_async) != 0 {
          self.write_str("async", true)
        }
        if (f.flags & fn_generator) != 0 {
          self.write_str("*", false)
        }
      }
      self.class_key(n)
      self.params(f)
      self.block(f.body)
    }
    ClassField => {
      if (n.d & 1) != 0 {
        self.write_str("static", true)
      }
      self.class_key(n)
      if n.b >= 0 {
        self.write_str("=", false)
        self.expr(n.b)
      }
      self.write_str(";", false)
    }
    ClassStaticBlock => {
      self.write_str("static", true)
      self.block(n.a)
    }
    _ => raise Unexpected("bad class member: " + n.kind.to_repr().to_string())
  }
}

///|
fn Printer::block(self : Printer, id : Int) -> Unit raise ParseError {
  let n = self.prog.nodes[id]
  self.write_str("{", false)
  self.stmts(n.a, n.b)
  self.write_str("}", false)
}

///|
fn Printer::expr(self : Printer, id : Int) -> Unit raise ParseError {
  if id < 0 {
    return
  }
  let n = self.prog.nodes[id]
  match n.kind {
    Ident => self.write_ref(n.a)
    NumberLit | StringLit | RegexLit | TemplateNoSub =>
      self.write_span(n.a, n.b, true)
    BoolLit => self.write_str(if n.a == 1 { "true" } else { "false" }, true)
    NullLit => self.write_str("null", true)
    This => self.write_str("this", true)
    Super => self.write_str("super", true)
    ArrayLit => {
      self.write_str("[", false)
      for i in 0..<n.b {
        if i > 0 {
          self.write_str(",", false)
        }
        let item = self.prog.edges[n.a + i]
        if item >= 0 {
          self.expr(item)
        }
      }
      if n.b > 0 && self.prog.edges[n.a + n.b - 1] < 0 {
        self.write_str(",", false)
      }
      self.write_str("]", false)
    }
    ObjectLit => {
      self.write_str("{", false)
      for i in 0..<n.b {
        if i > 0 {
          self.write_str(",", false)
        }
        self.prop(self.prog.edges[n.a + i])
      }
      self.write_str("}", false)
    }
    FunctionExpr => self.func(n.a, true)
    FunctionDecl => self.func(n.a, true)
    ArrowExpr => self.func(n.a, false)
    ClassExpr => self.class(n.a)
    ClassDecl => self.class(n.a)
    Unary => {
      let op = self.prog.ops[n.a]
      if n.b < 0 {
        self.write_str(op_text(op), op_is_word(op))
      } else {
        self.write_str(op_text(op), op_is_word(op))
        self.expr(n.b)
      }
    }
    Update => {
      let op = self.prog.ops[n.a]
      if n.b == 1 {
        self.write_str(op_text(op), false)
        self.expr(n.c)
      } else {
        self.expr(n.c)
        self.write_str(op_text(op), false)
      }
    }
    Binary | Logical => {
      self.expr(n.b)
      self.write_str(
        op_text(self.prog.ops[n.a]),
        op_is_word(self.prog.ops[n.a]),
      )
      self.expr(n.c)
    }
    Assign => {
      self.expr(n.b)
      let op = self.prog.ops[n.a]
      if op == Op::Assign {
        self.write_str("=", false)
      } else {
        self.write_str(op_text(op), false)
        self.write_str("=", false)
      }
      self.expr(n.c)
    }
    Cond => {
      self.expr(n.a)
      self.write_str("?", false)
      self.expr(n.b)
      self.write_str(":", false)
      self.expr(n.c)
    }
    Call => {
      self.expr(n.a)
      if n.d == 1 {
        self.write_str("?.", false)
      }
      self.write_str("(", false)
      self.expr_list(n.b, n.c)
      self.write_str(")", false)
    }
    New => {
      self.write_str("new", true)
      self.expr(n.a)
      if n.b >= 0 {
        self.write_str("(", false)
        self.expr_list(n.b, n.c)
        self.write_str(")", false)
      }
    }
    Member =>
      if n.a < 0 {
        self.write_str("new", true)
        self.write_str(".", false)
        self.write_span(n.b, n.c, true)
      } else {
        let numeric = self.prog.nodes[n.a].kind == NumberLit
        if numeric {
          self.write_str("(", false)
        }
        self.expr(n.a)
        if numeric {
          self.write_str(")", false)
        }
        if n.d == 1 {
          self.write_str("?.", false)
        } else {
          self.write_str(".", false)
        }
        self.write_span(n.b, n.c, true)
      }
    Index => {
      self.expr(n.a)
      if n.d == 1 {
        self.write_str("?.", false)
      }
      self.write_str("[", false)
      self.expr(n.b)
      self.write_str("]", false)
    }
    Sequence => self.expr_list(n.a, n.b)
    Spread => {
      self.write_str("...", false)
      self.expr(n.a)
    }
    Paren => {
      self.write_str("(", false)
      self.expr(n.a)
      self.write_str(")", false)
    }
    Template => {
      self.write_str("`", false)
      let mut idx = n.a
      for q in 0..<n.b {
        self.write_span(self.prog.edges[idx], self.prog.edges[idx + 1], false)
        idx = idx + 2
        if q < n.b - 1 {
          self.write_str("${", false)
          self.expr(self.prog.edges[idx])
          idx = idx + 1
          self.write_str("}", false)
        }
      }
      self.write_str("`", false)
    }
    PatRest => {
      self.write_str("...", false)
      self.expr(n.a)
    }
    TaggedTemplate => {
      self.expr(n.a)
      self.expr(n.b)
    }
    PatIdent | PatArray | PatObject | PatProp | PatDefault => self.pat(id)
    _ => raise Unexpected("bad expression: " + n.kind.to_repr().to_string())
  }
}

///|
fn stmt_needs_semi(kind : Nk) -> Bool {
  match kind {
    ExprStmt
    | VarDecl
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
fn Printer::stmt(self : Printer, id : Int) -> Unit raise ParseError {
  let n = self.prog.nodes[id]
  match n.kind {
    ExprStmt => self.expr(n.a)
    ModuleStmt => self.module_stmt(n.a)
    Block => {
      self.block(id)
      return
    }
    VarDecl => self.var_decl(n)
    Declarator => ()
    FunctionDecl => self.func(n.a, true)
    ClassDecl => self.class(n.a)
    Empty => ()
    Debugger => self.write_str("debugger", true)
    If => {
      self.write_str("if", true)
      self.write_str("(", false)
      self.expr(n.a)
      self.write_str(")", false)
      self.stmt(n.b)
      if n.c >= 0 {
        self.write_str("else", true)
        self.stmt(n.c)
      }
      return
    }
    For => {
      self.write_str("for", true)
      self.write_str("(", false)
      if n.a >= 0 {
        let init = self.prog.nodes[n.a]
        if init.kind == VarDecl {
          self.var_decl(init)
        } else {
          self.expr(n.a)
        }
      }
      self.write_str(";", false)
      if n.b >= 0 {
        self.expr(n.b)
      }
      self.write_str(";", false)
      if n.c >= 0 {
        self.expr(n.c)
      }
      self.write_str(")", false)
      self.stmt(n.d)
      return
    }
    ForIn | ForOf => {
      self.write_str("for", true)
      if n.d == 1 {
        self.write_str("await", true)
      }
      self.write_str("(", false)
      let left = self.prog.nodes[n.a]
      if left.kind == VarDecl {
        self.var_decl(left)
      } else {
        self.expr(n.a)
      }
      self.write_str(if n.kind == ForIn { "in" } else { "of" }, true)
      self.expr(n.b)
      self.write_str(")", false)
      self.stmt(n.c)
      return
    }
    While => {
      self.write_str("while", true)
      self.write_str("(", false)
      self.expr(n.a)
      self.write_str(")", false)
      self.stmt(n.b)
      return
    }
    DoWhile => {
      self.write_str("do", true)
      self.stmt(n.a)
      self.write_str("while", true)
      self.write_str("(", false)
      self.expr(n.b)
      self.write_str(")", false)
    }
    Return => {
      self.write_str("return", true)
      if n.a >= 0 {
        self.expr(n.a)
      }
    }
    Throw => {
      self.write_str("throw", true)
      self.expr(n.a)
    }
    Break | Continue => {
      self.write_str(if n.kind == Break { "break" } else { "continue" }, true)
      if n.a >= 0 {
        self.write_span(n.a, n.b, true)
      }
    }
    Labeled => {
      self.write_span(n.a, n.b, true)
      self.write_str(":", false)
      self.stmt(n.c)
      return
    }
    Switch => {
      self.write_str("switch", true)
      self.write_str("(", false)
      self.expr(n.a)
      self.write_str(")", false)
      self.write_str("{", false)
      for i in 0..<n.c {
        self.switch_case(self.prog.edges[n.b + i])
      }
      self.write_str("}", false)
      return
    }
    Try => {
      self.write_str("try", true)
      self.block(n.a)
      if n.b >= 0 {
        let c = self.prog.nodes[n.b]
        self.write_str("catch", true)
        if c.a >= 0 {
          self.write_str("(", false)
          self.pat(c.a)
          self.write_str(")", false)
        }
        self.block(c.b)
      }
      if n.c >= 0 {
        self.write_str("finally", true)
        self.block(n.c)
      }
      return
    }
    With => {
      self.write_str("with", true)
      self.write_str("(", false)
      self.expr(n.a)
      self.write_str(")", false)
      self.stmt(n.b)
      return
    }
    _ => raise Unexpected("bad statement: " + n.kind.to_repr().to_string())
  }
  if stmt_needs_semi(n.kind) {
    self.write_str(";", false)
  }
}

///|
fn Printer::switch_case(self : Printer, id : Int) -> Unit raise ParseError {
  let n = self.prog.nodes[id]
  if n.a >= 0 {
    self.write_str("case", true)
    self.expr(n.a)
  } else {
    self.write_str("default", true)
  }
  self.write_str(":", false)
  self.stmts(n.b, n.c)
}

///|
fn Printer::var_decl(self : Printer, n : Node) -> Unit raise ParseError {
  self.write_str(
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
      self.write_str(",", false)
    }
    let d = self.prog.nodes[self.prog.edges[n.b + i]]
    self.pat(d.a)
    if d.b >= 0 {
      self.write_str("=", false)
      self.expr(d.b)
    }
  }
}

///|
fn print_program(prog : Program) -> String raise ParseError {
  let p = Printer::new(prog)
  let root = prog.nodes[prog.root]
  p.stmts(root.a, root.b)
  p.out.to_string()
}

///|
fn Printer::class_key(self : Printer, property : Node) -> Unit raise ParseError {
  if (property.d & 2) != 0 {
    self.write_str("[", false)
    self.expr(property.a)
    self.write_str("]", false)
  } else {
    self.pat_key(property.a)
  }
}

///|
fn Printer::binding_renamed(self : Printer, symbol : Int) -> Bool {
  guard symbol >= 0 else { return false }
  let binding = self.prog.symbols[symbol]
  binding.name_slot >= 0 &&
  self.prog.generated_names[binding.name_slot] != self.prog.names[binding.name]
}
