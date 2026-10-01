///|
priv struct NameAlphabet {
  first : String
  rest : String
}

///|
const NAME_FIRST : String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$"

///|
const NAME_REST : String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$0123456789"

///|
const ASCII_CHARACTER_COUNT : Int = 128

///|
// Inverse of NAME_REST. A missing character has no frequency bucket.
let name_character_slots : Array[Int] = {
  let slots = Array::make(ASCII_CHARACTER_COUNT, -1)
  for slot in 0..<NAME_REST.length() {
    slots[NAME_REST[slot].to_int()] = slot
  }
  slots
}

///|
priv struct NameGenerator {
  alphabet : NameAlphabet
  reserved : @hashset.HashSet[String]
  mut index : Int
}

///|
fn NameGenerator::new(
  alphabet : NameAlphabet,
  reserved : @hashset.HashSet[String],
) -> NameGenerator {
  { alphabet, reserved, index: 0, }
}

///|
fn NameGenerator::next_name(self : NameGenerator) -> String {
  for index = self.index {
    let name = self.alphabet.short_name(index)
    guard !self.reserved.contains(name) else { continue index + 1 }
    self.index = index + 1
    break name
  }
}

///|
fn count_characters(
  source : String,
  start : Int,
  end : Int,
  counts : Array[Int],
  delta : Int,
) -> Unit {
  for index in start..<end {
    let slot = name_character_slots.get(source[index].to_int()).unwrap_or(-1)
    guard slot >= 0 else { continue }
    counts[slot] += delta
  }
}

///|
fn Program::name_alphabet(program : Program) -> NameAlphabet {
  let counts = program.unchanged_character_counts()
  alphabet_by_frequency(counts)
}

///|
fn Program::unchanged_character_counts(program : Program) -> Array[Int] {
  let counts = program.character_counts.copy()
  let counted_removals : @hashset.HashSet[Int] = @hashset.HashSet([])
  let subtract_name = (start, end) => {
    guard counted_removals.add_and_check(start) else { return }
    count_characters(program.source, start, end, counts, -1)
  }
  for reference in program.references {
    guard reference.symbol >= 0 &&
      (program.symbols[reference.symbol].flags & symbol_preserved) == 0 else {
      continue
    }
    subtract_name(reference.name_start, reference.name_end)
  }
  for node in program.nodes {
    guard (node.kind == Labeled || node.kind == Break || node.kind == Continue) &&
      node.a >= 0 else {
      continue
    }
    subtract_name(node.a, node.b)
  }
  for id, _ in program.property_names {
    let node = program.nodes[program.property_leaf(id)]
    subtract_name(node.a, node.b)
  }
  for id, _ in program.member_names {
    let node = program.nodes[id]
    subtract_name(node.b, node.c)
  }
  counts
}

///|
fn alphabet_by_frequency(counts : Array[Int]) -> NameAlphabet {
  let order = Array::makei(NAME_REST.length(), index => index)
  order.sort_by((left, right) => {
    let difference = counts[right] - counts[left]
    guard difference == 0 else { return difference }
    left - right
  })
  let first = StringBuilder()
  let rest = StringBuilder()
  for index in order {
    let character = NAME_REST.get_char(index).unwrap()
    rest.write_char(character)
    guard index < NAME_FIRST.length() else { continue }
    first.write_char(character)
  }
  { first: first.to_string(), rest: rest.to_string(), }
}
