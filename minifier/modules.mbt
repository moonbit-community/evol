///|
priv enum ImportBinding {
  Default(Int)
  Namespace(Int)
  // Original imported name node and local binding reference.
  Named(Int, Int)
}

///|
priv enum ModuleStatement {
  Import(Array[ImportBinding], Int, Int)
  // Local expression/original re-export name, followed by the public name.
  ExportNames(Array[(Int, Int)], Int, Int)
  ExportAll(Int, Int, Int)
  ExportDeclaration(Int)
  ExportDefault(Int)
}

///|
fn Parser::module_node(self : Parser, statement : ModuleStatement) -> Int {
  let index = self.prog.modules.length()
  self.prog.modules.push(statement)
  self.prog.node(ModuleStmt, index, 0, 0, 0)
}

///|
fn Parser::expect_keyword(
  self : Parser,
  keyword : String,
) -> Unit raise ParseError {
  guard self.lx.is_kw(keyword) else { raise Unexpected("expected " + keyword) }
  self.advance()
}

///|
fn Parser::module_name(self : Parser) -> Int raise ParseError {
  guard self.lx.tok == Ident || self.lx.tok == Str else {
    raise Unexpected("module name expected")
  }
  self.parse_property_key()
}

///|
fn Parser::import_binding(self : Parser) -> Int raise ParseError {
  guard self.lx.tok == Ident else {
    raise Unexpected("import binding expected")
  }
  let reference = self.declare_sym(0, self.lx.start, self.lx.end)
  self.advance()
  reference
}

///|
fn Parser::module_source(self : Parser) -> Int raise ParseError {
  guard self.lx.tok == Str else {
    raise Unexpected("module source string expected")
  }
  self.parse_property_key()
}

///|
fn Parser::import_attributes(self : Parser) -> Int raise ParseError {
  guard self.lx.is_kw("with") else { return -1 }
  self.advance()
  self.parse_object_literal()
}

///|
fn Parser::parse_import(self : Parser) -> Int raise ParseError {
  guard self.scope == 0 else {
    raise Unexpected("import must be at module top level")
  }
  self.expect_keyword("import")
  let bindings : Array[ImportBinding] = []
  if self.lx.tok != Str {
    let has_default = self.lx.tok == Ident
    if has_default {
      bindings.push(Default(self.import_binding()))
    }
    let has_more = !has_default || self.eat(Comma)
    if has_more {
      if self.eat(Star) {
        self.expect_keyword("as")
        bindings.push(Namespace(self.import_binding()))
      } else {
        self.expect(LBrace)
        while self.lx.tok != RBrace {
          let imported = self.module_name()
          let reference = if self.lx.is_kw("as") {
            self.advance()
            self.import_binding()
          } else {
            let name = self.prog.nodes[imported]
            guard self.prog.src[name.a] != '\'' && self.prog.src[name.a] != '"' else {
              raise Unexpected("string import name requires a local alias")
            }
            self.declare_sym(0, name.a, name.b)
          }
          bindings.push(Named(imported, reference))
          if !self.eat(Comma) {
            break
          }
        }
        self.expect(RBrace)
      }
    }
    self.expect_keyword("from")
  }
  let source = self.module_source()
  let attributes = self.import_attributes()
  self.eat_semi()
  self.module_node(Import(bindings, source, attributes))
}

///|
fn Parser::parse_export(self : Parser) -> Int raise ParseError {
  guard self.scope == 0 else {
    raise Unexpected("export must be at module top level")
  }
  self.expect_keyword("export")
  if self.lx.is_kw("default") {
    self.advance()
    let declaration = self.lx.is_kw("function") ||
      self.lx.is_kw("class") ||
      (self.lx.is_kw("async") && self.peek_is_kw("function"))
    let body = if declaration {
      self.parse_stmt()
    } else {
      self.parse_assign_in()
    }
    if !declaration {
      self.eat_semi()
    }
    return self.module_node(ExportDefault(body))
  }
  if self.eat(Star) {
    let export_name = if self.lx.is_kw("as") {
      self.advance()
      self.module_name()
    } else {
      -1
    }
    self.expect_keyword("from")
    let source = self.module_source()
    let attributes = self.import_attributes()
    self.eat_semi()
    return self.module_node(ExportAll(export_name, source, attributes))
  }
  if self.eat(LBrace) {
    let names : Array[(Int, Int)] = []
    while self.lx.tok != RBrace {
      let source_name = self.module_name()
      let exported = if self.lx.is_kw("as") {
        self.advance()
        self.module_name()
      } else {
        source_name
      }
      names.push((source_name, exported))
      if !self.eat(Comma) {
        break
      }
    }
    self.expect(RBrace)
    let source = if self.lx.is_kw("from") {
      self.advance()
      self.module_source()
    } else {
      -1
    }
    let specifiers = names.map(pair => {
      let (source_name, exported) = pair
      guard source < 0 else { return pair }
      let name = self.prog.nodes[source_name]
      let reference = self.add_ref(0, name.a, name.b)
      (self.prog.node(Ident, reference, 0, 0, 0), exported)
    })
    let attributes = if source >= 0 { self.import_attributes() } else { -1 }
    self.eat_semi()
    return self.module_node(ExportNames(specifiers, source, attributes))
  }
  let body = self.parse_stmt()
  guard self.prog.nodes[body].kind == VarDecl ||
    self.prog.nodes[body].kind == FunctionDecl ||
    self.prog.nodes[body].kind == ClassDecl else {
    raise Unexpected("export declaration expected")
  }
  self.preserve_exported_names(body)
  self.module_node(ExportDeclaration(body))
}

///|
/// Direct export declarations expose their binding spelling as the public name.
fn Parser::preserve_exported_names(self : Parser, id : Int) -> Unit {
  let node = self.prog.nodes[id]
  match node.kind {
    PatIdent => {
      let symbol = self.prog.refs[node.a].sym
      self.prog.symbols[symbol] = {
        ..self.prog.symbols[symbol],
        flags: sym_must_not_rename,
      }
    }
    PatArray | PatObject =>
      for index in 0..<node.b {
        let item = self.prog.edges[node.a + index]
        if item >= 0 {
          self.preserve_exported_names(item)
        }
      }
    PatProp => self.preserve_exported_names(node.b)
    PatRest | PatDefault | Declarator => self.preserve_exported_names(node.a)
    VarDecl =>
      for index in 0..<node.c {
        self.preserve_exported_names(self.prog.edges[node.b + index])
      }
    FunctionDecl | ClassDecl => {
      let reference = if node.kind == FunctionDecl {
        self.prog.funcs[node.a].name
      } else {
        self.prog.classes[node.a].name
      }
      if reference >= 0 {
        let symbol = self.prog.refs[reference].sym
        self.prog.symbols[symbol] = {
          ..self.prog.symbols[symbol],
          flags: sym_must_not_rename,
        }
      }
    }
    _ => ()
  }
}

///|
fn Printer::module_source(
  self : Printer,
  source : Int,
  attributes : Int,
) -> Unit raise ParseError {
  self.expr(source)
  if attributes >= 0 {
    self.write_str("with", true)
    self.expr(attributes)
  }
}

///|
fn Printer::module_stmt(self : Printer, index : Int) -> Unit raise ParseError {
  match self.prog.modules[index] {
    Import(bindings, source, attributes) => {
      self.write_str("import", true)
      let mut has_prefix = false
      let mut named = false
      for binding in bindings {
        match binding {
          Default(reference) => {
            self.write_ref(reference)
            has_prefix = true
          }
          Namespace(reference) => {
            if has_prefix {
              self.write_str(",", false)
            }
            self.write_str("*", false)
            self.write_str("as", true)
            self.write_ref(reference)
          }
          Named(imported, reference) => {
            if named {
              self.write_str(",", false)
            } else {
              if has_prefix {
                self.write_str(",", false)
              }
              self.write_str("{", false)
              named = true
            }
            self.expr(imported)
            self.write_str("as", true)
            self.write_ref(reference)
          }
        }
      }
      if named {
        self.write_str("}", false)
      }
      if bindings.length() > 0 {
        self.write_str("from", true)
      }
      self.module_source(source, attributes)
      self.write_str(";", false)
    }
    ExportNames(specifiers, source, attributes) => {
      self.write_str("export", true)
      self.write_str("{", false)
      for index in 0..<specifiers.length() {
        if index > 0 {
          self.write_str(",", false)
        }
        let (source_name, exported) = specifiers[index]
        self.expr(source_name)
        self.write_str("as", true)
        self.expr(exported)
      }
      self.write_str("}", false)
      if source >= 0 {
        self.write_str("from", true)
        self.module_source(source, attributes)
      }
      self.write_str(";", false)
    }
    ExportAll(export_name, source, attributes) => {
      self.write_str("export", true)
      self.write_str("*", false)
      if export_name >= 0 {
        self.write_str("as", true)
        self.expr(export_name)
      }
      self.write_str("from", true)
      self.module_source(source, attributes)
      self.write_str(";", false)
    }
    ExportDeclaration(body) => {
      self.write_str("export", true)
      self.stmt(body)
    }
    ExportDefault(body) => {
      self.write_str("export", true)
      self.write_str("default", true)
      self.expr(body)
      self.write_str(";", false)
    }
  }
}
