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
const LATIN_ALPHABET_SIZE : Int = 26

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
    let name = short_name(index, self.alphabet)
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
    guard source.get_char(index) is Some(character) else { continue }
    guard name_character_index(character) is Some(slot) else { continue }
    counts[slot] += delta
  }
}

///|
/// Indices follow NAME_REST: lowercase, uppercase, underscore, dollar, digits.
fn name_character_index(character : Char) -> Int? {
  let slot = if character.is_ascii_lowercase() {
    character.to_int() - 'a'.to_int()
  } else if character.is_ascii_uppercase() {
    character.to_int() - 'A'.to_int() + LATIN_ALPHABET_SIZE
  } else if character == '_' {
    LATIN_ALPHABET_SIZE * 2
  } else if character == '$' {
    LATIN_ALPHABET_SIZE * 2 + 1
  } else if character.is_ascii_digit() {
    character.to_int() - '0'.to_int() + NAME_FIRST.length()
  } else {
    return None
  }
  Some(slot)
}

///|
fn name_alphabet(program : Program) -> NameAlphabet {
  let counts = unchanged_character_counts(program)
  alphabet_by_frequency(counts)
}

///|
fn unchanged_character_counts(program : Program) -> Array[Int] {
  let counts = program.character_counts.copy()
  let counted_removals : Map[Int, Bool] = Map([])
  let subtract_name = (start, end) => {
    guard !counted_removals.contains(start) else { return }
    count_characters(program.source, start, end, counts, -1)
    counted_removals[start] = true
  }
  for reference in program.references {
    if reference.symbol >= 0 &&
      (program.symbols[reference.symbol].flags & symbol_preserved) == 0 {
      subtract_name(reference.name_start, reference.name_end)
    }
  }
  for node in program.nodes {
    if (node.kind == Labeled || node.kind == Break || node.kind == Continue) &&
      node.a >= 0 {
      subtract_name(node.a, node.b)
    }
  }
  for id, _ in program.property_names {
    let node = program.nodes[property_leaf(program, id)]
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
    if difference == 0 {
      left - right
    } else {
      difference
    }
  })
  let first = StringBuilder()
  let rest = StringBuilder()
  for index in order {
    let character = NAME_REST.get_char(index).unwrap()
    rest.write_char(character)
    if index < NAME_FIRST.length() {
      first.write_char(character)
    }
  }
  { first: first.to_string(), rest: rest.to_string(), }
}
