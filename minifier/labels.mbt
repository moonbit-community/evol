///|
priv struct LabelNames {
  mut active : Array[(Int, String)]
  by_depth : Array[String]
  generator : NameGenerator
}

///|
fn LabelNames::new(alphabet : NameAlphabet) -> LabelNames {
  {
    active: [],
    by_depth: [],
    generator: NameGenerator::new(alphabet, keywords),
  }
}

///|
fn Printer::label_reference(
  self : Printer,
  name : Int,
  start : Int,
  end : Int,
) -> Unit {
  for index = self.labels.active.length() - 1 {
    guard index >= 0 else {
      self.span(start, end, true)
      break
    }
    let (original, renamed) = self.labels.active[index]
    guard original == name else { continue index - 1 }
    self.text(renamed, true)
    break
  }
}

///|
fn Printer::label_name(self : Printer) -> String {
  let depth = self.labels.active.length()
  if depth == self.labels.by_depth.length() {
    self.labels.by_depth.push(self.labels.generator.next_name())
  }
  self.labels.by_depth[depth]
}

///|
/// Functions and class static blocks have independent label namespaces.
fn Printer::isolated_block(self : Printer, id : Int) -> Unit {
  let outer = self.labels.active
  self.labels.active = []
  self.block(id)
  self.labels.active = outer
}
