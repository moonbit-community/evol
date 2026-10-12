///|
#valtype
priv struct IdentifierEntry {
  spelling : StringView
  hash : Int
  id : Int
}

///|
/// Append-only interning table. Slots index dense entries; -1 marks an empty
/// slot. Capacity is a power of two, with at least one quarter kept empty.
priv struct IdentifierTable {
  entries : Array[IdentifierEntry]
  mut slots : Array[Int]
}

///|
fn IdentifierTable::new() -> IdentifierTable {
  let table : IdentifierTable = { entries: [], slots: Array::make(16, -1), }
  table.insert("arguments", NAME_ARGUMENTS)
  table.insert("eval", NAME_EVAL)
  table.insert("__proto__", NAME_PROTO)
  table.insert("undefined", NAME_UNDEFINED)
  table
}

///|
const FNV_OFFSET : UInt = 2166136261U

///|
const FNV_PRIME : UInt = 16777619U

///|
const IDENTIFIER_HASH_WINDOW : Int = 8

///|
/// Hashes select buckets only; find_slot still compares the complete spelling.
/// Long generated names need only fixed windows to distribute their entries.
fn identifier_hash(spelling : StringView) -> Int {
  let length = spelling.length()
  guard length > IDENTIFIER_HASH_WINDOW * 3 else {
    return identifier_hash_range(spelling, 0, length, FNV_OFFSET).reinterpret_as_int()
  }
  let seed = (FNV_OFFSET ^ length.reinterpret_as_uint()) * FNV_PRIME
  let first_hash = identifier_hash_range(
    spelling,
    0,
    IDENTIFIER_HASH_WINDOW,
    seed,
  )
  let middle_start = (length - IDENTIFIER_HASH_WINDOW) / 2
  let middle_hash = identifier_hash_range(
    spelling,
    middle_start,
    middle_start + IDENTIFIER_HASH_WINDOW,
    first_hash,
  )
  identifier_hash_range(
    spelling,
    length - IDENTIFIER_HASH_WINDOW,
    length,
    middle_hash,
  ).reinterpret_as_int()
}

///|
fn identifier_hash_range(
  spelling : StringView,
  start : Int,
  end : Int,
  seed : UInt,
) -> UInt {
  let mut hash = seed
  for index in start..<end {
    hash = (hash ^ spelling[index].to_uint()) * FNV_PRIME
  }
  hash
}

///|
fn IdentifierTable::find_slot(
  self : IdentifierTable,
  spelling : StringView,
  hash : Int,
) -> Int {
  let mask = self.slots.length() - 1
  for slot = hash & mask {
    let index = self.slots[slot]
    guard index >= 0 else { break slot }
    let entry = self.entries[index]
    guard entry.hash == hash && entry.spelling == spelling else {
      continue (slot + 1) & mask
    }
    break slot
  }
}

///|
fn IdentifierTable::get(self : IdentifierTable, spelling : StringView) -> Int? {
  self.get_hashed(spelling, identifier_hash(spelling))
}

///|
fn IdentifierTable::get_hashed(
  self : IdentifierTable,
  spelling : StringView,
  hash : Int,
) -> Int? {
  let slot = self.find_slot(spelling, hash)
  let index = self.slots[slot]
  guard index >= 0 else { return None }
  Some(self.entries[index].id)
}

///|
fn IdentifierTable::insert(
  self : IdentifierTable,
  spelling : StringView,
  id : Int,
) -> Unit {
  self.insert_hashed(spelling, identifier_hash(spelling), id)
}

///|
fn IdentifierTable::insert_hashed(
  self : IdentifierTable,
  spelling : StringView,
  hash : Int,
  id : Int,
) -> Unit {
  if (self.entries.length() + 1) * 4 >= self.slots.length() * 3 {
    self.grow()
  }
  let slot = self.find_slot(spelling, hash)
  let entry = { spelling, hash, id, }
  let index = self.slots[slot]
  guard index < 0 else {
    self.entries[index] = entry
    return
  }
  self.slots[slot] = self.entries.length()
  self.entries.push(entry)
}

///|
fn IdentifierTable::grow(self : IdentifierTable) -> Unit {
  let slots = Array::make(self.slots.length() * 2, -1)
  let mask = slots.length() - 1
  for index, entry in self.entries {
    for slot = entry.hash & mask {
      guard slots[slot] < 0 else { continue (slot + 1) & mask }
      slots[slot] = index
      break
    }
  }
  self.slots = slots
}
