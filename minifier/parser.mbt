///|
pub suberror ParseError {
  Unexpected(String)
}

///|
priv struct Parser {
  prog : Program
  lx : Lexer
  mut scope : Int
}

///|
fn Parser::new(src : String) -> Parser {
  { prog: Program::new(src), lx: Lexer::new(src), scope: 0, }
}

///|
fn Parser::advance(self : Parser) -> Unit {
  self.lx.next(self.allows_regex())
}

///|
fn Parser::advance_regex(self : Parser, regex_allowed : Bool) -> Unit {
  self.lx.next(regex_allowed)
}

///|
fn Parser::allows_regex(self : Parser) -> Bool {
  match self.lx.tok {
    Ident =>
      self.lx.is_kw("return") ||
      self.lx.is_kw("typeof") ||
      self.lx.is_kw("void") ||
      self.lx.is_kw("delete") ||
      self.lx.is_kw("new") ||
      self.lx.is_kw("in") ||
      self.lx.is_kw("of") ||
      self.lx.is_kw("instanceof") ||
      self.lx.is_kw("case") ||
      self.lx.is_kw("do") ||
      self.lx.is_kw("else") ||
      self.lx.is_kw("yield") ||
      self.lx.is_kw("await") ||
      self.lx.is_kw("throw") ||
      self.lx.is_kw("extends") ||
      self.lx.is_kw("default")
    Number
    | Str
    | Regex
    | TmplNoSub
    | TmplTail
    | RParen
    | RBracket
    | PlusPlus
    | MinusMinus => false
    _ => true
  }
}

///|
fn Parser::expect(self : Parser, t : Tok) -> Unit raise ParseError {
  if self.lx.tok != t {
    raise Unexpected("expected \{t.to_repr()}, got \{self.lx.tok.to_repr()}")
  }
  self.advance()
}

///|
fn Parser::eat(self : Parser, t : Tok) -> Bool {
  if self.lx.tok == t {
    self.advance()
    true
  } else {
    false
  }
}

///|
fn Parser::peek_tok(self : Parser) -> Tok {
  let snap = self.lx.snapshot()
  self.lx.next(self.allows_regex())
  let t = self.lx.tok
  self.lx.restore(snap)
  t
}

///|
fn Parser::peek_is_kw(self : Parser, kw : String) -> Bool {
  let snap = self.lx.snapshot()
  self.lx.next(self.allows_regex())
  let r = self.lx.is_kw(kw)
  self.lx.restore(snap)
  r
}

///|
fn Parser::nearest_var_scope(self : Parser) -> Int {
  let mut s = self.scope
  while s >= 0 &&
        self.prog.scopes[s].kind != scope_module &&
        self.prog.scopes[s].kind != scope_function {
    s = self.prog.scopes[s].parent
  }
  if s < 0 {
    0
  } else {
    s
  }
}

///|
fn Parser::add_ref(self : Parser, sc : Int, s : Int, e : Int) -> Int {
  self.prog.refs.push({ scope: sc, name_start: s, name_end: e, sym: -1, })
  self.prog.refs.length() - 1
}

///|
fn Parser::declare_sym(self : Parser, sc : Int, s : Int, e : Int) -> Int {
  let sym = self.prog.add_symbol(s, e, sc)
  let r = self.add_ref(sc, s, e)
  self.prog.refs[r].sym = sym
  r
}

///|
fn Parser::declare_pattern(self : Parser, sc : Int, s : Int, e : Int) -> Int {
  self.prog.node(PatIdent, self.declare_sym(sc, s, e), 0, 0, 0)
}

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

///|
fn Parser::parse_pattern(
  self : Parser,
  declaring : Bool,
  sc : Int,
  allow_default : Bool,
) -> Int raise ParseError {
  let inner = self.parse_pattern_atom(declaring, sc)
  if allow_default && self.lx.tok == Assign {
    self.advance()
    let d = self.parse_assign()
    self.prog.node(PatDefault, inner, d, 0, 0)
  } else {
    inner
  }
}

///|
fn Parser::parse_pattern_atom(
  self : Parser,
  declaring : Bool,
  sc : Int,
) -> Int raise ParseError {
  match self.lx.tok {
    Ident => {
      let s = self.lx.start
      let e = self.lx.end
      self.advance()
      if declaring {
        self.declare_pattern(sc, s, e)
      } else {
        self.prog.node(Ident, self.add_ref(sc, s, e), 0, 0, 0)
      }
    }
    LBracket => self.parse_array_pattern(declaring, sc)
    LBrace => self.parse_object_pattern(declaring, sc)
    _ => raise Unexpected("pattern expected, got \{self.lx.tok.to_repr()}")
  }
}

///|
fn Parser::parse_array_pattern(
  self : Parser,
  declaring : Bool,
  sc : Int,
) -> Int raise ParseError {
  self.expect(LBracket)
  let items : Array[Int] = []
  while self.lx.tok != RBracket && self.lx.tok != Eof {
    if self.lx.tok == Comma {
      self.advance()
      items.push(-1)
      continue
    }
    if self.lx.tok == Ellipsis {
      self.advance()
      let inner = self.parse_pattern(declaring, sc, false)
      items.push(self.prog.node(PatRest, inner, 0, 0, 0))
    } else {
      items.push(self.parse_pattern(declaring, sc, true))
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RBracket)
  let (off, n) = self.prog.edge_list(items)
  self.prog.node(PatArray, off, n, 0, 0)
}

///|
fn Parser::parse_object_pattern(
  self : Parser,
  declaring : Bool,
  sc : Int,
) -> Int raise ParseError {
  self.expect(LBrace)
  let items : Array[Int] = []
  while self.lx.tok != RBrace && self.lx.tok != Eof {
    if self.lx.tok == Ellipsis {
      self.advance()
      let inner = self.parse_pattern(declaring, sc, false)
      items.push(self.prog.node(PatRest, inner, 0, 0, 0))
    } else {
      items.push(self.parse_pattern_prop(declaring, sc, true))
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RBrace)
  let (off, n) = self.prog.edge_list(items)
  self.prog.node(PatObject, off, n, 0, 0)
}

///|
fn Parser::parse_pattern_prop(
  self : Parser,
  declaring : Bool,
  sc : Int,
  allow_default : Bool,
) -> Int raise ParseError {
  let mut computed = 0
  let mut key = -1
  if self.lx.tok == LBracket {
    self.advance()
    key = self.parse_assign()
    self.expect(RBracket)
    computed = 1
  } else {
    key = self.parse_property_key()
  }
  if self.eat(Colon) {
    let v = self.parse_pattern(declaring, sc, allow_default)
    return self.prog.node(PatProp, key, v, 0, computed)
  }
  let ks = self.prog.nodes[key].a
  let ke = self.prog.nodes[key].b
  let v = if declaring {
    self.declare_pattern(sc, ks, ke)
  } else {
    self.prog.node(Ident, self.add_ref(sc, ks, ke), 0, 0, 0)
  }
  let v2 = if allow_default && self.lx.tok == Assign {
    self.advance()
    let d = self.parse_assign()
    self.prog.node(PatDefault, v, d, 0, 0)
  } else {
    v
  }
  self.prog.node(PatProp, key, v2, 1, 0)
}

// ---------------------------------------------------------------------------
// Program / statements
// ---------------------------------------------------------------------------

///|
fn parse_program(src : String) -> Program raise ParseError {
  let p = Parser::new(src)
  p.advance_regex(true)
  let stmts = p.parse_stmt_list(true)
  let (off, n) = p.prog.edge_list(stmts)
  p.prog.root = p.prog.node(Block, off, n, 0, 0)
  p.prog
}

///|
fn Parser::parse_stmt_list(
  self : Parser,
  top : Bool,
) -> Array[Int] raise ParseError {
  let out : Array[Int] = []
  while self.lx.tok != Eof && self.lx.tok != RBrace {
    out.push(self.parse_stmt())
  }
  ignore(top)
  out
}

///|
fn Parser::parse_stmt(self : Parser) -> Int raise ParseError {
  if self.lx.tok == LBrace {
    return self.parse_block()
  }
  if self.lx.tok == Semicolon {
    self.advance()
    return self.prog.node(Empty, 0, 0, 0, 0)
  }
  if self.lx.tok == Ident {
    if self.lx.is_kw("var") {
      self.advance()
      return self.parse_var_decl(0)
    }
    if self.lx.is_kw("let") && self.peek_tok() != Colon {
      self.advance()
      return self.parse_var_decl(1)
    }
    if self.lx.is_kw("const") {
      self.advance()
      return self.parse_var_decl(2)
    }
    if self.lx.is_kw("async") && self.peek_is_kw("function") {
      self.advance()
      self.advance()
      return self.parse_function(true, is_async=1)
    }
    if self.lx.is_kw("function") {
      self.advance()
      return self.parse_function(true)
    }
    if self.lx.is_kw("class") {
      self.advance()
      return self.parse_class(true)
    }
    if self.lx.is_kw("if") {
      self.advance()
      return self.parse_if()
    }
    if self.lx.is_kw("for") {
      self.advance()
      return self.parse_for()
    }
    if self.lx.is_kw("while") {
      self.advance()
      return self.parse_while()
    }
    if self.lx.is_kw("do") {
      self.advance()
      return self.parse_do_while()
    }
    if self.lx.is_kw("return") {
      self.advance()
      return self.parse_return()
    }
    if self.lx.is_kw("throw") {
      self.advance()
      return self.parse_throw()
    }
    if self.lx.is_kw("break") {
      self.advance()
      return self.parse_break_continue(true)
    }
    if self.lx.is_kw("continue") {
      self.advance()
      return self.parse_break_continue(false)
    }
    if self.lx.is_kw("switch") {
      self.advance()
      return self.parse_switch()
    }
    if self.lx.is_kw("try") {
      self.advance()
      return self.parse_try()
    }
    if self.lx.is_kw("debugger") {
      self.advance()
      self.eat_semi()
      return self.prog.node(Debugger, 0, 0, 0, 0)
    }
    if self.lx.is_kw("with") {
      self.advance()
      return self.parse_with()
    }
    if self.peek_tok() == Colon {
      return self.parse_label()
    }
  }
  let e = self.parse_expr()
  self.eat_semi()
  self.prog.node(ExprStmt, e, 0, 0, 0)
}

///|
fn Parser::eat_semi(self : Parser) -> Unit {
  if self.lx.tok == Semicolon {
    self.advance()
  }
}

///|
fn Parser::parse_block(self : Parser) -> Int raise ParseError {
  self.expect(LBrace)
  let saved = self.scope
  self.scope = self.prog.push_scope(saved, scope_block)
  let stmts = self.parse_stmt_list(false)
  self.scope = saved
  self.expect(RBrace)
  let (off, n) = self.prog.edge_list(stmts)
  self.prog.node(Block, off, n, 0, 0)
}

///|
fn Parser::parse_var_decl(self : Parser, kind : Int) -> Int raise ParseError {
  let sc = if kind == 0 { self.nearest_var_scope() } else { self.scope }
  let decls : Array[Int] = []
  while true {
    let pat = self.parse_pattern(true, sc, false)
    let init = if self.lx.tok == Assign {
      self.advance()
      self.parse_assign()
    } else {
      -1
    }
    decls.push(self.prog.node(Declarator, pat, init, 0, 0))
    if !self.eat(Comma) {
      break
    }
  }
  self.eat_semi()
  let (off, n) = self.prog.edge_list(decls)
  self.prog.node(VarDecl, kind, off, n, 0)
}

///|
fn Parser::parse_if(self : Parser) -> Int raise ParseError {
  self.expect(LParen)
  let cond = self.parse_expr()
  self.expect(RParen)
  let cons = self.parse_stmt()
  let alt = if self.lx.tok == Ident && self.lx.is_kw("else") {
    self.advance()
    self.parse_stmt()
  } else {
    -1
  }
  self.prog.node(If, cond, cons, alt, 0)
}

///|
fn Parser::parse_while(self : Parser) -> Int raise ParseError {
  self.expect(LParen)
  let cond = self.parse_expr()
  self.expect(RParen)
  let body = self.parse_stmt()
  self.prog.node(While, cond, body, 0, 0)
}

///|
fn Parser::parse_do_while(self : Parser) -> Int raise ParseError {
  let body = self.parse_stmt()
  if !(self.lx.tok == Ident && self.lx.is_kw("while")) {
    raise Unexpected("while expected")
  }
  self.advance()
  self.expect(LParen)
  let cond = self.parse_expr()
  self.expect(RParen)
  self.eat_semi()
  self.prog.node(DoWhile, body, cond, 0, 0)
}

///|
fn Parser::parse_for(self : Parser) -> Int raise ParseError {
  self.expect(LParen)
  let saved = self.scope
  self.scope = self.prog.push_scope(saved, scope_block)
  let mut init = -1
  if self.lx.tok == Semicolon {

  } else if self.lx.tok == Ident &&
    (self.lx.is_kw("var") || self.lx.is_kw("let") || self.lx.is_kw("const")) {
    let kind = if self.lx.is_kw("var") {
      0
    } else if self.lx.is_kw("let") {
      1
    } else {
      2
    }
    self.advance()
    init = self.parse_var_decl_no_semi(kind)
    if self.lx.tok == Ident && self.lx.is_kw("of") {
      self.advance()
      let right = self.parse_assign()
      self.expect(RParen)
      let body = self.parse_stmt()
      self.scope = saved
      return self.prog.node(ForOf, init, right, body, 0)
    }
    if self.lx.tok == Ident && self.lx.is_kw("in") {
      self.advance()
      let right = self.parse_expr()
      self.expect(RParen)
      let body = self.parse_stmt()
      self.scope = saved
      return self.prog.node(ForIn, init, right, body, 0)
    }
  } else {
    let e = self.parse_expr()
    if self.lx.tok == Ident && self.lx.is_kw("of") {
      self.advance()
      let right = self.parse_assign()
      self.expect(RParen)
      let body = self.parse_stmt()
      self.scope = saved
      return self.prog.node(ForOf, e, right, body, 0)
    }
    if self.lx.tok == Ident && self.lx.is_kw("in") {
      self.advance()
      let right = self.parse_expr()
      self.expect(RParen)
      let body = self.parse_stmt()
      self.scope = saved
      return self.prog.node(ForIn, e, right, body, 0)
    }
    init = e
  }
  self.expect(Semicolon)
  let cond = if self.lx.tok == Semicolon { -1 } else { self.parse_expr() }
  self.expect(Semicolon)
  let update = if self.lx.tok == RParen { -1 } else { self.parse_expr() }
  self.expect(RParen)
  let body = self.parse_stmt()
  self.scope = saved
  self.prog.node(For, init, cond, update, body)
}

///|
fn Parser::parse_var_decl_no_semi(
  self : Parser,
  kind : Int,
) -> Int raise ParseError {
  let sc = if kind == 0 { self.nearest_var_scope() } else { self.scope }
  let decls : Array[Int] = []
  while true {
    let pat = self.parse_pattern(true, sc, false)
    let init = if self.lx.tok == Assign {
      self.advance()
      self.parse_assign()
    } else {
      -1
    }
    decls.push(self.prog.node(Declarator, pat, init, 0, 0))
    if !self.eat(Comma) {
      break
    }
  }
  let (off, n) = self.prog.edge_list(decls)
  self.prog.node(VarDecl, kind, off, n, 0)
}

///|
fn Parser::parse_return(self : Parser) -> Int raise ParseError {
  let arg = if self.lx.newline_before ||
    self.lx.tok == Semicolon ||
    self.lx.tok == RBrace ||
    self.lx.tok == Eof {
    -1
  } else {
    self.parse_expr()
  }
  self.eat_semi()
  self.prog.node(Return, arg, 0, 0, 0)
}

///|
fn Parser::parse_throw(self : Parser) -> Int raise ParseError {
  if self.lx.newline_before {
    raise Unexpected("newline after throw")
  }
  let arg = self.parse_expr()
  self.eat_semi()
  self.prog.node(Throw, arg, 0, 0, 0)
}

///|
fn Parser::parse_break_continue(self : Parser, is_break : Bool) -> Int {
  let mut ls = -1
  let mut le = -1
  if !self.lx.newline_before && self.lx.tok == Ident {
    ls = self.lx.start
    le = self.lx.end
    self.advance()
  }
  self.eat_semi()
  self.prog.node(if is_break { Break } else { Continue }, ls, le, 0, 0)
}

///|
fn Parser::parse_label(self : Parser) -> Int raise ParseError {
  let ls = self.lx.start
  let le = self.lx.end
  self.advance()
  self.expect(Colon)
  let body = self.parse_stmt()
  self.prog.node(Labeled, ls, le, body, 0)
}

///|
fn Parser::parse_with(self : Parser) -> Int raise ParseError {
  self.expect(LParen)
  let obj = self.parse_expr()
  self.expect(RParen)
  let body = self.parse_stmt()
  self.scope = self.scope
  self.prog.node(With, obj, body, 0, 0)
}

///|
fn Parser::parse_switch(self : Parser) -> Int raise ParseError {
  self.expect(LParen)
  let disc = self.parse_expr()
  self.expect(RParen)
  self.expect(LBrace)
  let saved = self.scope
  self.scope = self.prog.push_scope(saved, scope_block)
  let cases : Array[Int] = []
  while self.lx.tok != RBrace && self.lx.tok != Eof {
    let cond = if self.lx.tok == Ident && self.lx.is_kw("case") {
      self.advance()
      self.parse_expr()
    } else if self.lx.tok == Ident && self.lx.is_kw("default") {
      self.advance()
      -1
    } else {
      raise Unexpected("case/default expected")
    }
    self.expect(Colon)
    let stmts : Array[Int] = []
    while self.lx.tok != RBrace &&
          self.lx.tok != Eof &&
          !(self.lx.tok == Ident &&
          (self.lx.is_kw("case") || self.lx.is_kw("default"))) {
      stmts.push(self.parse_stmt())
    }
    let (soff, sn) = self.prog.edge_list(stmts)
    cases.push(self.prog.node(SwitchCase, cond, soff, sn, 0))
  }
  self.scope = saved
  self.expect(RBrace)
  let (coff, cn) = self.prog.edge_list(cases)
  self.prog.node(Switch, disc, coff, cn, 0)
}

///|
fn Parser::parse_try(self : Parser) -> Int raise ParseError {
  let block = self.parse_block()
  let mut handler = -1
  if self.lx.tok == Ident && self.lx.is_kw("catch") {
    self.advance()
    let saved = self.scope
    self.scope = self.prog.push_scope(saved, scope_catch)
    let mut param = -1
    if self.eat(LParen) {
      param = self.parse_pattern(true, self.scope, false)
      self.expect(RParen)
    }
    let body = self.parse_block()
    self.scope = saved
    handler = self.prog.node(Catch, param, body, 0, 0)
  }
  let finalizer = if self.lx.tok == Ident && self.lx.is_kw("finally") {
    self.advance()
    self.parse_block()
  } else {
    -1
  }
  self.prog.node(Try, block, handler, finalizer, 0)
}

// ---------------------------------------------------------------------------
// Functions and classes
// ---------------------------------------------------------------------------

///|
fn Parser::parse_function(
  self : Parser,
  is_decl : Bool,
  is_async? : Int = 0,
) -> Int raise ParseError {
  let mut is_gen = 0
  if self.lx.tok == Star {
    self.advance()
    is_gen = 1
  }
  let mut name_ref = -1
  if self.lx.tok == Ident {
    let s = self.lx.start
    let e = self.lx.end
    self.advance()
    if is_decl {
      name_ref = self.declare_sym(self.scope, s, e)
    } else {
      name_ref = self.add_ref(self.scope, s, e)
    }
  }
  let func_id = self.parse_function_rest(name_ref, is_async, is_gen, !is_decl)
  self.prog.node(
    if is_decl {
      FunctionDecl
    } else {
      FunctionExpr
    },
    func_id,
    0,
    0,
    0,
  )
}

///|
fn Parser::parse_function_rest(
  self : Parser,
  name_ref : Int,
  is_async : Int,
  is_gen : Int,
  bind_name_inside : Bool,
) -> Int raise ParseError {
  let outer = self.scope
  let fscope = self.prog.push_scope(outer, scope_function)
  self.scope = fscope
  if bind_name_inside && name_ref >= 0 {
    let r = self.prog.refs[name_ref]
    let sym = self.prog.add_symbol(r.name_start, r.name_end, fscope)
    self.prog.refs[name_ref].sym = sym
  }
  self.expect(LParen)
  let params : Array[Int] = []
  while self.lx.tok != RParen && self.lx.tok != Eof {
    if self.lx.tok == Ellipsis {
      self.advance()
      let inner = self.parse_pattern(true, fscope, false)
      params.push(self.prog.node(PatRest, inner, 0, 0, 0))
    } else {
      params.push(self.parse_pattern(true, fscope, true))
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RParen)
  let body = self.parse_block()
  self.scope = outer
  let (off, n) = self.prog.edge_list(params)
  self.prog.funcs.push({
    name: name_ref,
    params_off: off,
    params_count: n,
    body,
    flags: is_async * fn_async + is_gen * fn_generator,
  })
  self.prog.funcs.length() - 1
}

///|
fn Parser::parse_class(self : Parser, is_decl : Bool) -> Int raise ParseError {
  let mut name_ref = -1
  if self.lx.tok == Ident {
    let s = self.lx.start
    let e = self.lx.end
    self.advance()
    if is_decl {
      name_ref = self.declare_sym(self.scope, s, e)
    } else {
      name_ref = self.add_ref(self.scope, s, e)
    }
  }
  let outer = self.scope
  let cscope = self.prog.push_scope(outer, scope_class)
  self.scope = cscope
  if !is_decl && name_ref >= 0 {
    let r = self.prog.refs[name_ref]
    let sym = self.prog.add_symbol(r.name_start, r.name_end, cscope)
    self.prog.refs[name_ref].sym = sym
  }
  let mut superclass = -1
  if self.lx.tok == Ident && self.lx.is_kw("extends") {
    self.advance()
    superclass = self.parse_postfix_expr()
  }
  self.expect(LBrace)
  let members : Array[Int] = []
  while self.lx.tok != RBrace && self.lx.tok != Eof {
    if self.eat(Semicolon) {
      continue
    }
    members.push(self.parse_class_member())
  }
  self.scope = outer
  self.expect(RBrace)
  let (off, n) = self.prog.edge_list(members)
  self.prog.classes.push({
    name: name_ref,
    superclass,
    members_off: off,
    members_count: n,
  })
  let cid = self.prog.classes.length() - 1
  self.prog.node(if is_decl { ClassDecl } else { ClassExpr }, cid, 0, 0, 0)
}

///|
fn Parser::parse_class_member(self : Parser) -> Int raise ParseError {
  let mut is_static = 0
  if self.lx.tok == Ident &&
    self.lx.is_kw("static") &&
    self.peek_tok() != LParen {
    self.advance()
    is_static = 1
  }
  let mut kind = 0
  if self.lx.tok == Ident &&
    (self.lx.is_kw("get") || self.lx.is_kw("set")) &&
    self.peek_tok() != LParen &&
    self.peek_tok() != Assign {
    kind = if self.lx.is_kw("get") { 1 } else { 2 }
    self.advance()
  }
  let mut is_async = 0
  let mut is_gen = 0
  if self.lx.tok == Ident &&
    self.lx.is_kw("async") &&
    self.peek_tok() != LParen &&
    self.peek_tok() != Assign {
    is_async = 1
    self.advance()
  }
  if self.lx.tok == Star {
    self.advance()
    is_gen = 1
  }
  let key = self.parse_property_key()
  if self.lx.tok == LParen {
    let fid = self.parse_function_rest(-1, is_async, is_gen, false)
    return self.prog.node(Method, key, fid, kind, is_static)
  }
  let init = if self.lx.tok == Assign {
    self.advance()
    self.parse_assign()
  } else {
    -1
  }
  self.eat_semi()
  self.prog.node(ClassField, key, init, kind, is_static)
}

///|
fn Parser::parse_property_key(self : Parser) -> Int raise ParseError {
  match self.lx.tok {
    Number => {
      let n = self.prog.node(NumberLit, self.lx.start, self.lx.end, 0, 0)
      self.advance()
      n
    }
    Str => {
      let n = self.prog.node(StringLit, self.lx.start, self.lx.end, 0, 0)
      self.advance()
      n
    }
    LBracket => {
      self.advance()
      let e = self.parse_assign()
      self.expect(RBracket)
      e
    }
    Ident => {
      let n = self.prog.node(StringLit, self.lx.start, self.lx.end, 0, 0)
      self.advance()
      n
    }
    _ => raise Unexpected("property key expected")
  }
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

///|
fn Parser::parse_expr(self : Parser) -> Int raise ParseError {
  let first = self.parse_assign()
  if self.lx.tok != Comma {
    return first
  }
  let items : Array[Int] = [first]
  while self.eat(Comma) {
    items.push(self.parse_assign())
  }
  let (off, n) = self.prog.edge_list(items)
  self.prog.node(Sequence, off, n, 0, 0)
}

///|
fn Parser::parse_assign(self : Parser) -> Int raise ParseError {
  let left = self.parse_cond()
  match self.lx.tok {
    Assign
    | AddAssign
    | SubAssign
    | MulAssign
    | DivAssign
    | ModAssign
    | ExpAssign
    | ShlAssign
    | ShrAssign
    | UshrAssign
    | BitAndAssign
    | BitOrAssign
    | BitXorAssign
    | LogicalAndAssign
    | LogicalOrAssign
    | NullishAssign => {
      let op = self.assign_op()
      self.advance()
      let right = self.parse_assign()
      self.prog.node(Assign, op, left, right, 0)
    }
    _ => left
  }
}

///|
fn Parser::assign_op(self : Parser) -> Int {
  let o = match self.lx.tok {
    Assign => Op::Assign
    AddAssign => Op::Add
    SubAssign => Op::Sub
    MulAssign => Op::Mul
    DivAssign => Op::Div
    ModAssign => Op::Mod
    ExpAssign => Op::Exp
    ShlAssign => Op::Shl
    ShrAssign => Op::Shr
    UshrAssign => Op::Ushr
    BitAndAssign => Op::BitAnd
    BitOrAssign => Op::BitOr
    BitXorAssign => Op::BitXor
    LogicalAndAssign => Op::LogicalAnd
    LogicalOrAssign => Op::LogicalOr
    NullishAssign => Op::Nullish
    _ => Op::Add
  }
  self.prog.op(o)
}

///|
fn Parser::parse_cond(self : Parser) -> Int raise ParseError {
  let cond = self.parse_binary(1)
  if self.lx.tok == Question {
    self.advance()
    let cons = self.parse_assign()
    self.expect(Colon)
    let alt = self.parse_assign()
    return self.prog.node(Cond, cond, cons, alt, 0)
  }
  cond
}

///|
fn Parser::bin_op_info(self : Parser) -> (Int, Int) {
  let (prec, o) = match self.lx.tok {
    StarStar => (11, Op::Exp)
    Star => (10, Op::Mul)
    Slash => (10, Op::Div)
    Percent => (10, Op::Mod)
    Plus => (9, Op::Add)
    Minus => (9, Op::Sub)
    Shl => (8, Op::Shl)
    Shr => (8, Op::Shr)
    Ushr => (8, Op::Ushr)
    Lt => (7, Op::Lt)
    Gt => (7, Op::Gt)
    Le => (7, Op::Le)
    Ge => (7, Op::Ge)
    Eq => (6, Op::Eq)
    Ne => (6, Op::Ne)
    StrictEq => (6, Op::StrictEq)
    StrictNe => (6, Op::StrictNe)
    BitAnd => (5, Op::BitAnd)
    BitXor => (4, Op::BitXor)
    BitOr => (3, Op::BitOr)
    LogicalAnd => (2, Op::LogicalAnd)
    Nullish => (1, Op::Nullish)
    LogicalOr => (1, Op::LogicalOr)
    Ident =>
      if self.lx.is_kw("in") {
        (7, Op::In)
      } else if self.lx.is_kw("instanceof") {
        (7, Op::Instanceof)
      } else {
        (-1, Op::Add)
      }
    _ => (-1, Op::Add)
  }
  (prec, self.prog.op(o))
}

///|
fn Parser::parse_binary(self : Parser, min_prec : Int) -> Int raise ParseError {
  let mut left = self.parse_unary()
  while true {
    let (prec, op) = self.bin_op_info()
    if prec < min_prec {
      break
    }
    self.advance()
    let next_min = if self.prog.ops[op] == Op::Exp { prec } else { prec + 1 }
    let right = self.parse_binary(next_min)
    let logical = self.prog.ops[op] == Op::LogicalAnd ||
      self.prog.ops[op] == Op::LogicalOr ||
      self.prog.ops[op] == Op::Nullish
    left = self.prog.node(
      if logical {
        Logical
      } else {
        Binary
      },
      op,
      left,
      right,
      0,
    )
  }
  left
}

///|
fn Parser::parse_unary(self : Parser) -> Int raise ParseError {
  match self.lx.tok {
    Not | Tilde | Plus | Minus => {
      let o = match self.lx.tok {
        Not => Op::Not
        Tilde => Op::BitNot
        Plus => Op::Plus
        _ => Op::Minus
      }
      let op = self.prog.op(o)
      self.advance()
      let e = self.parse_unary()
      self.prog.node(Unary, op, e, 0, 0)
    }
    PlusPlus | MinusMinus => {
      let o = if self.lx.tok == PlusPlus { Op::Inc } else { Op::Dec }
      let op = self.prog.op(o)
      self.advance()
      let e = self.parse_unary()
      self.prog.node(Update, op, 1, e, 0)
    }
    Ident =>
      if self.lx.is_kw("typeof") {
        let op = self.prog.op(Op::Typeof)
        self.advance()
        let e = self.parse_unary()
        self.prog.node(Unary, op, e, 0, 0)
      } else if self.lx.is_kw("void") {
        let op = self.prog.op(Op::Void)
        self.advance()
        let e = self.parse_unary()
        self.prog.node(Unary, op, e, 0, 0)
      } else if self.lx.is_kw("delete") {
        let op = self.prog.op(Op::Delete)
        self.advance()
        let e = self.parse_unary()
        self.prog.node(Unary, op, e, 0, 0)
      } else if self.lx.is_kw("await") {
        let op = self.prog.op(Op::Await)
        self.advance()
        let e = self.parse_unary()
        self.prog.node(Unary, op, e, 0, 0)
      } else {
        self.parse_postfix_expr()
      }
    _ => self.parse_postfix_expr()
  }
}

///|
fn Parser::parse_postfix_expr(self : Parser) -> Int raise ParseError {
  let mut e = self.parse_primary()
  e = self.parse_call_member_tail(e)
  if !self.lx.newline_before &&
    (self.lx.tok == PlusPlus || self.lx.tok == MinusMinus) {
    let o = if self.lx.tok == PlusPlus { Op::Inc } else { Op::Dec }
    let op = self.prog.op(o)
    self.advance()
    e = self.prog.node(Update, op, 0, e, 0)
  }
  e
}

///|
fn Parser::parse_call_member_tail(
  self : Parser,
  start : Int,
) -> Int raise ParseError {
  let mut e = start
  while true {
    match self.lx.tok {
      Dot => {
        self.advance()
        let ns = self.lx.start
        let ne = self.lx.end
        self.advance()
        e = self.prog.node(Member, e, ns, ne, 0)
      }
      QuestionDot => {
        self.advance()
        if self.lx.tok == LParen {
          let (off, n) = self.parse_arguments()
          e = self.prog.node(Call, e, off, n, 1)
        } else if self.lx.tok == LBracket {
          self.advance()
          let idx = self.parse_expr()
          self.expect(RBracket)
          e = self.prog.node(Index, e, idx, 1, 0)
        } else {
          let ns = self.lx.start
          let ne = self.lx.end
          self.advance()
          e = self.prog.node(Member, e, ns, ne, 1)
        }
      }
      LBracket => {
        self.advance()
        let idx = self.parse_expr()
        self.expect(RBracket)
        e = self.prog.node(Index, e, idx, 0, 0)
      }
      LParen => {
        let (off, n) = self.parse_arguments()
        e = self.prog.node(Call, e, off, n, 0)
      }
      TmplNoSub | TmplHead => {
        let t = self.parse_template_expr()
        e = self.prog.node(TaggedTemplate, e, t, 0, 0)
      }
      _ => break
    }
  }
  e
}

///|
fn Parser::parse_arguments(self : Parser) -> (Int, Int) raise ParseError {
  self.expect(LParen)
  let args : Array[Int] = []
  while self.lx.tok != RParen && self.lx.tok != Eof {
    if self.lx.tok == Ellipsis {
      self.advance()
      let inner = self.parse_assign()
      args.push(self.prog.node(Spread, inner, 0, 0, 0))
    } else {
      args.push(self.parse_assign())
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RParen)
  self.prog.edge_list(args)
}

///|
fn Parser::parse_primary(self : Parser) -> Int raise ParseError {
  match self.lx.tok {
    Number => {
      let n = self.prog.node(NumberLit, self.lx.start, self.lx.end, 0, 0)
      self.advance()
      n
    }
    Str => {
      let n = self.prog.node(StringLit, self.lx.start, self.lx.end, 0, 0)
      self.advance()
      n
    }
    Regex => {
      let n = self.prog.node(RegexLit, self.lx.start, self.lx.end, 0, 0)
      self.advance()
      n
    }
    TmplNoSub | TmplHead => self.parse_template_expr()
    LParen => self.parse_paren()
    LBracket =>
      if self.looks_like_pattern_assign() {
        self.parse_pattern(false, self.scope, true)
      } else {
        self.parse_array_literal()
      }
    LBrace =>
      if self.looks_like_pattern_assign() {
        self.parse_pattern(false, self.scope, true)
      } else {
        self.parse_object_literal()
      }
    Ident => self.parse_ident_expr()
    _ => raise Unexpected("expression expected, got \{self.lx.tok.to_repr()}")
  }
}

///|
fn Parser::parse_ident_expr(self : Parser) -> Int raise ParseError {
  if self.lx.is_kw("true") {
    self.advance()
    return self.prog.node(BoolLit, 1, 0, 0, 0)
  }
  if self.lx.is_kw("false") {
    self.advance()
    return self.prog.node(BoolLit, 0, 0, 0, 0)
  }
  if self.lx.is_kw("null") {
    self.advance()
    return self.prog.node(NullLit, 0, 0, 0, 0)
  }
  if self.lx.is_kw("this") {
    self.advance()
    return self.prog.node(This, 0, 0, 0, 0)
  }
  if self.lx.is_kw("super") {
    self.advance()
    return self.prog.node(Super, 0, 0, 0, 0)
  }
  if self.lx.is_kw("function") {
    self.advance()
    return self.parse_function(false)
  }
  if self.lx.is_kw("class") {
    self.advance()
    return self.parse_class(false)
  }
  if self.lx.is_kw("new") {
    self.advance()
    return self.parse_new()
  }
  if self.lx.is_kw("async") {
    let p = self.peek_tok()
    if p == Ident || p == LParen {
      let snap = self.lx.snapshot()
      self.advance()
      if self.lx.tok == Ident && self.lx.is_kw("function") {
        self.advance()
        return self.parse_function(false, is_async=1)
      }
      if self.lx.tok == Ident && self.peek_tok() == Arrow {
        return self.parse_arrow_single(1)
      }
      if self.lx.tok == LParen && self.looks_like_arrow() {
        return self.parse_arrow_parens(1)
      }
      self.lx.restore(snap)
    }
  }
  if self.lx.is_kw("yield") {
    self.advance()
    let op = self.prog.op(Op::Yield)
    let e = if self.lx.newline_before ||
      self.lx.tok == Semicolon ||
      self.lx.tok == RBrace ||
      self.lx.tok == Eof {
      -1
    } else {
      self.parse_assign()
    }
    return self.prog.node(Unary, op, e, 0, 0)
  }
  if self.peek_tok() == Arrow {
    return self.parse_arrow_single(0)
  }
  let s = self.lx.start
  let e = self.lx.end
  self.advance()
  self.prog.node(Ident, self.add_ref(self.scope, s, e), 0, 0, 0)
}

///|
fn Parser::parse_new(self : Parser) -> Int raise ParseError {
  if self.lx.tok == Dot {
    self.advance()
    let ns = self.lx.start
    let ne = self.lx.end
    self.advance()
    return self.prog.node(Member, -1, ns, ne, 2)
  }
  let callee = self.parse_member_only()
  let (off, n) = if self.lx.tok == LParen {
    self.parse_arguments()
  } else {
    (-1, 0)
  }
  self.prog.node(New, callee, off, n, 0)
}

///|
fn Parser::parse_member_only(self : Parser) -> Int raise ParseError {
  let mut e = self.parse_primary()
  while true {
    match self.lx.tok {
      Dot => {
        self.advance()
        let ns = self.lx.start
        let ne = self.lx.end
        self.advance()
        e = self.prog.node(Member, e, ns, ne, 0)
      }
      LBracket => {
        self.advance()
        let idx = self.parse_expr()
        self.expect(RBracket)
        e = self.prog.node(Index, e, idx, 0, 0)
      }
      _ => break
    }
  }
  e
}

///|
fn Parser::parse_arrow_single(
  self : Parser,
  is_async : Int,
) -> Int raise ParseError {
  let outer = self.scope
  let fscope = self.prog.push_scope(outer, scope_function)
  self.scope = fscope
  let s = self.lx.start
  let e = self.lx.end
  self.advance()
  let param = self.declare_pattern(fscope, s, e)
  let name_ref = -1
  self.expect(Arrow)
  let (body, expr_body) = self.parse_arrow_body()
  self.scope = outer
  let params : Array[Int] = [param]
  let (off, n) = self.prog.edge_list(params)
  self.prog.funcs.push({
    name: name_ref,
    params_off: off,
    params_count: n,
    body,
    flags: fn_arrow + is_async * fn_async + expr_body,
  })
  let fid = self.prog.funcs.length() - 1
  self.prog.node(ArrowExpr, fid, 0, 0, 0)
}

///|
fn Parser::parse_arrow_parens(
  self : Parser,
  is_async : Int,
) -> Int raise ParseError {
  let outer = self.scope
  let fscope = self.prog.push_scope(outer, scope_function)
  self.scope = fscope
  self.expect(LParen)
  let params : Array[Int] = []
  while self.lx.tok != RParen && self.lx.tok != Eof {
    if self.lx.tok == Ellipsis {
      self.advance()
      let inner = self.parse_pattern(true, fscope, false)
      params.push(self.prog.node(PatRest, inner, 0, 0, 0))
    } else {
      params.push(self.parse_pattern(true, fscope, true))
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RParen)
  self.expect(Arrow)
  let (body, expr_body) = self.parse_arrow_body()
  self.scope = outer
  let (off, n) = self.prog.edge_list(params)
  self.prog.funcs.push({
    name: -1,
    params_off: off,
    params_count: n,
    body,
    flags: fn_arrow + is_async * fn_async + expr_body,
  })
  let fid = self.prog.funcs.length() - 1
  self.prog.node(ArrowExpr, fid, 0, 0, 0)
}

///|
fn Parser::parse_arrow_body(self : Parser) -> (Int, Int) raise ParseError {
  if self.lx.tok == LBrace {
    (self.parse_block(), 0)
  } else {
    (self.parse_assign(), fn_expr_body)
  }
}

///|
fn Parser::parse_paren(self : Parser) -> Int raise ParseError {
  if self.looks_like_arrow() {
    return self.parse_arrow_parens(0)
  }
  self.expect(LParen)
  let e = self.parse_expr()
  self.expect(RParen)
  self.prog.node(Paren, e, 0, 0, 0)
}

///|
fn Parser::looks_like_arrow(self : Parser) -> Bool {
  let snap = self.lx.snapshot()
  let mut depth = 1
  self.lx.next(false)
  let mut ok = false
  let mut steps = 0
  while steps < 100000 {
    steps = steps + 1
    match self.lx.tok {
      Eof => break
      LParen | LBracket | LBrace => {
        depth = depth + 1
        self.lx.next(false)
      }
      RParen | RBracket | RBrace => {
        depth = depth - 1
        if depth == 0 {
          self.lx.next(false)
          ok = self.lx.tok == Arrow
          break
        }
        self.lx.next(false)
      }
      _ => self.lx.next(false)
    }
  }
  self.lx.restore(snap)
  ok
}

///|
fn Parser::parse_template_expr(self : Parser) -> Int raise ParseError {
  if self.lx.tok == TmplNoSub {
    let n = self.prog.node(TemplateNoSub, self.lx.start, self.lx.end, 0, 0)
    self.advance_regex(false)
    n
  } else {
    self.parse_template()
  }
}

///|
fn Parser::looks_like_pattern_assign(self : Parser) -> Bool {
  let snap = self.lx.snapshot()
  let mut depth = 1
  self.lx.next(false)
  let mut ok = false
  let mut steps = 0
  while steps < 100000 {
    steps = steps + 1
    match self.lx.tok {
      Eof => break
      LParen | LBracket | LBrace => {
        depth = depth + 1
        self.lx.next(false)
      }
      RParen | RBracket | RBrace => {
        depth = depth - 1
        if depth == 0 {
          self.lx.next(false)
          ok = self.lx.tok == Assign
          break
        }
        self.lx.next(false)
      }
      _ => self.lx.next(false)
    }
  }
  self.lx.restore(snap)
  ok
}

///|
fn Parser::parse_template(self : Parser) -> Int raise ParseError {
  let parts : Array[Int] = []
  let mut quasi_count = 0
  while true {
    quasi_count = quasi_count + 1
    parts.push(self.lx.quasi_start)
    parts.push(self.lx.quasi_end)
    let kind = self.lx.tok
    self.advance_regex(true)
    let e = self.parse_expr()
    parts.push(e)
    if self.lx.tok != RBrace {
      raise Unexpected("template substitution must end with }")
    }
    self.lx.rescan_template_continue()
    if self.lx.tok == TmplTail {
      quasi_count = quasi_count + 1
      parts.push(self.lx.quasi_start)
      parts.push(self.lx.quasi_end)
      self.advance_regex(false)
      break
    }
    ignore(kind)
  }
  let (off, _n) = self.prog.edge_list(parts)
  self.prog.node(Template, off, quasi_count, 0, 0)
}

///|
fn Parser::parse_array_literal(self : Parser) -> Int raise ParseError {
  self.expect(LBracket)
  let items : Array[Int] = []
  while self.lx.tok != RBracket && self.lx.tok != Eof {
    if self.lx.tok == Comma {
      self.advance()
      items.push(-1)
      continue
    }
    if self.lx.tok == Ellipsis {
      self.advance()
      let inner = self.parse_assign()
      items.push(self.prog.node(Spread, inner, 0, 0, 0))
    } else {
      items.push(self.parse_assign())
    }
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RBracket)
  let (off, n) = self.prog.edge_list(items)
  self.prog.node(ArrayLit, off, n, 0, 0)
}

///|
fn Parser::parse_object_literal(self : Parser) -> Int raise ParseError {
  self.expect(LBrace)
  let props : Array[Int] = []
  while self.lx.tok != RBrace && self.lx.tok != Eof {
    props.push(self.parse_object_prop())
    if !self.eat(Comma) {
      break
    }
  }
  self.expect(RBrace)
  let (off, n) = self.prog.edge_list(props)
  self.prog.node(ObjectLit, off, n, 0, 0)
}

///|
fn Parser::parse_object_prop(self : Parser) -> Int raise ParseError {
  if self.lx.tok == Ellipsis {
    self.advance()
    let inner = self.parse_assign()
    return self.prog.node(PatRest, inner, 0, 0, 0)
  }
  let mut kind = 0
  if self.lx.tok == Ident &&
    (self.lx.is_kw("get") || self.lx.is_kw("set")) &&
    self.peek_tok() != Colon &&
    self.peek_tok() != Comma &&
    self.peek_tok() != RBrace &&
    self.peek_tok() != LParen {
    kind = if self.lx.is_kw("get") { 1 } else { 2 }
    self.advance()
  }
  let mut is_async = 0
  let mut is_gen = 0
  if self.lx.tok == Ident &&
    self.lx.is_kw("async") &&
    self.peek_tok() != Colon &&
    self.peek_tok() != Comma &&
    self.peek_tok() != RBrace &&
    self.peek_tok() != LParen {
    is_async = 1
    self.advance()
  }
  if self.lx.tok == Star {
    self.advance()
    is_gen = 1
  }
  let was_ident = self.lx.tok == Ident
  let mut computed = 0
  let key = if self.lx.tok == LBracket {
    self.advance()
    let k = self.parse_assign()
    self.expect(RBracket)
    computed = 1
    k
  } else {
    self.parse_property_key()
  }
  if self.lx.tok == LParen {
    let fid = self.parse_function_rest(-1, is_async, is_gen, false)
    return self.prog.node(Method, key, fid, kind, computed)
  }
  if self.eat(Colon) {
    let v = self.parse_assign()
    return self.prog.node(Prop, key, v, 0, computed)
  }
  if was_ident && computed == 0 {
    let r = self.add_ref(
      self.scope,
      self.prog.nodes[key].a,
      self.prog.nodes[key].b,
    )
    let v = self.prog.node(Ident, r, 0, 0, 0)
    return self.prog.node(Prop, key, v, 1, 0)
  }
  let v = self.parse_assign()
  self.prog.node(Prop, key, v, 0, computed)
}
