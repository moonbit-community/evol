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
  let index = self.program.modules.length()
  self.program.modules.push(statement)
  self.program.node(ModuleStatement, index, 0, 0, 0)
}

///|
fn Parser::expect_keyword(
  self : Parser,
  keyword : String,
) -> Unit raise ParseError {
  guard self.lexer.is_keyword(keyword) else {
    raise Unexpected("expected " + keyword)
  }
  self.advance()
}

///|
fn Parser::module_name(self : Parser) -> Int raise ParseError {
  guard self.lexer.token == Identifier || self.lexer.token == StringLiteral else {
    raise Unexpected("module name expected")
  }
  self.parse_property_key()
}

///|
fn Parser::import_binding(self : Parser) -> Int raise ParseError {
  guard self.lexer.token == Identifier else {
    raise Unexpected("import binding expected")
  }
  let reference = self.declare_symbol(0, self.lexer.start, self.lexer.end)
  self.advance()
  reference
}

///|
fn Parser::module_source(self : Parser) -> Int raise ParseError {
  guard self.lexer.token == StringLiteral else {
    raise Unexpected("module source string expected")
  }
  self.parse_property_key()
}

///|
fn Parser::import_attributes(self : Parser) -> Int raise ParseError {
  guard self.lexer.is_keyword("with") else { return -1 }
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
  if self.lexer.token != StringLiteral {
    let has_default = self.lexer.token == Identifier
    if has_default {
      bindings.push(Default(self.import_binding()))
    }
    let has_more = !has_default || self.eat(Comma)
    if has_more {
      if self.eat(Asterisk) {
        self.expect_keyword("as")
        bindings.push(Namespace(self.import_binding()))
      } else {
        self.expect(LeftBrace)
        while self.lexer.token != RightBrace {
          let imported = self.module_name()
          let reference = if self.lexer.is_keyword("as") {
            self.advance()
            self.import_binding()
          } else {
            let name = self.program.nodes[imported]
            guard self.program.source[name.a] != '\'' &&
              self.program.source[name.a] != '"' else {
              raise Unexpected("string import name requires a local alias")
            }
            self.declare_symbol(0, name.a, name.b)
          }
          bindings.push(Named(imported, reference))
          if !self.eat(Comma) {
            break
          }
        }
        self.expect(RightBrace)
      }
    }
    self.expect_keyword("from")
  }
  let source = self.module_source()
  let attributes = self.import_attributes()
  self.eat_semicolon()
  self.module_node(Import(bindings, source, attributes))
}

///|
fn Parser::parse_export(self : Parser) -> Int raise ParseError {
  guard self.scope == 0 else {
    raise Unexpected("export must be at module top level")
  }
  self.expect_keyword("export")
  if self.lexer.is_keyword("default") {
    self.advance()
    let declaration = self.lexer.is_keyword("function") ||
      self.lexer.is_keyword("class") ||
      (self.lexer.is_keyword("async") && self.peek_is_keyword("function"))
    let body = if declaration {
      self.parse_statement()
    } else {
      self.parse_assignment_allowing_in()
    }
    if !declaration {
      self.eat_semicolon()
    }
    return self.module_node(ExportDefault(body))
  }
  if self.eat(Asterisk) {
    let export_name = if self.lexer.is_keyword("as") {
      self.advance()
      self.module_name()
    } else {
      -1
    }
    self.expect_keyword("from")
    let source = self.module_source()
    let attributes = self.import_attributes()
    self.eat_semicolon()
    return self.module_node(ExportAll(export_name, source, attributes))
  }
  if self.eat(LeftBrace) {
    let names : Array[(Int, Int)] = []
    while self.lexer.token != RightBrace {
      let source_name = self.module_name()
      let exported = if self.lexer.is_keyword("as") {
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
    self.expect(RightBrace)
    let source = if self.lexer.is_keyword("from") {
      self.advance()
      self.module_source()
    } else {
      -1
    }
    let specifiers = names.map(pair => {
      let (source_name, exported) = pair
      guard source < 0 else { return pair }
      let name = self.program.nodes[source_name]
      let reference = self.add_reference(0, name.a, name.b)
      (self.program.node(Identifier, reference, 0, 0, 0), exported)
    })
    let attributes = if source >= 0 { self.import_attributes() } else { -1 }
    self.eat_semicolon()
    return self.module_node(ExportNames(specifiers, source, attributes))
  }
  let body = self.parse_statement()
  guard self.program.nodes[body].kind == VariableDeclaration ||
    self.program.nodes[body].kind == FunctionDeclaration ||
    self.program.nodes[body].kind == ClassDeclaration else {
    raise Unexpected("export declaration expected")
  }
  self.preserve_exported_names(body)
  self.module_node(ExportDeclaration(body))
}

///|
/// Direct export declarations expose their binding spelling as the public name.
fn Parser::preserve_exported_names(self : Parser, id : Int) -> Unit {
  let node = self.program.nodes[id]
  match node.kind {
    PatternIdentifier => {
      let symbol = self.program.references[node.a].symbol
      self.program.symbols[symbol] = {
        ..self.program.symbols[symbol],
        flags: symbol_must_not_rename,
      }
    }
    PatternArray | PatternObject =>
      for index in 0..<node.b {
        let item = self.program.edges[node.a + index]
        if item >= 0 {
          self.preserve_exported_names(item)
        }
      }
    PatternProperty => self.preserve_exported_names(node.b)
    PatternRest | PatternDefault | Declarator =>
      self.preserve_exported_names(node.a)
    VariableDeclaration =>
      for index in 0..<node.c {
        self.preserve_exported_names(self.program.edges[node.b + index])
      }
    FunctionDeclaration | ClassDeclaration => {
      let reference = if node.kind == FunctionDeclaration {
        self.program.functions[node.a].name
      } else {
        self.program.classes[node.a].name
      }
      if reference >= 0 {
        let symbol = self.program.references[reference].symbol
        self.program.symbols[symbol] = {
          ..self.program.symbols[symbol],
          flags: symbol_must_not_rename,
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
  self.expression(source)
  if attributes >= 0 {
    self.write_string("with", true)
    self.expression(attributes)
  }
}

///|
fn Printer::module_stmt(self : Printer, index : Int) -> Unit raise ParseError {
  match self.program.modules[index] {
    Import(bindings, source, attributes) => {
      self.write_string("import", true)
      let mut has_prefix = false
      let mut named = false
      for binding in bindings {
        match binding {
          Default(reference) => {
            self.write_reference(reference)
            has_prefix = true
          }
          Namespace(reference) => {
            if has_prefix {
              self.write_string(",", false)
            }
            self.write_string("*", false)
            self.write_string("as", true)
            self.write_reference(reference)
          }
          Named(imported, reference) => {
            if named {
              self.write_string(",", false)
            } else {
              if has_prefix {
                self.write_string(",", false)
              }
              self.write_string("{", false)
              named = true
            }
            self.expression(imported)
            self.write_string("as", true)
            self.write_reference(reference)
          }
        }
      }
      if named {
        self.write_string("}", false)
      }
      if bindings.length() > 0 {
        self.write_string("from", true)
      }
      self.module_source(source, attributes)
      self.write_string(";", false)
    }
    ExportNames(specifiers, source, attributes) => {
      self.write_string("export", true)
      self.write_string("{", false)
      for index in 0..<specifiers.length() {
        if index > 0 {
          self.write_string(",", false)
        }
        let (source_name, exported) = specifiers[index]
        self.expression(source_name)
        self.write_string("as", true)
        self.expression(exported)
      }
      self.write_string("}", false)
      if source >= 0 {
        self.write_string("from", true)
        self.module_source(source, attributes)
      }
      self.write_string(";", false)
    }
    ExportAll(export_name, source, attributes) => {
      self.write_string("export", true)
      self.write_string("*", false)
      if export_name >= 0 {
        self.write_string("as", true)
        self.expression(export_name)
      }
      self.write_string("from", true)
      self.module_source(source, attributes)
      self.write_string(";", false)
    }
    ExportDeclaration(body) => {
      self.write_string("export", true)
      self.statement(body)
    }
    ExportDefault(body) => {
      self.write_string("export", true)
      self.write_string("default", true)
      self.expression(body)
      self.write_string(";", false)
    }
  }
}
