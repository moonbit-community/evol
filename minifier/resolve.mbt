///|
fn resolve(prog : Program) -> Unit {
  let n = prog.refs.length()
  for i in 0..<n {
    let r = prog.refs[i]
    if r.sym < 0 {
      let name = prog.src_span(r.name_start, r.name_end)
      let mut scope = r.scope
      let mut found = -1
      while scope >= 0 && found < 0 {
        match prog.scopes[scope].members.get(name) {
          Some(id) => found = id
          None => scope = prog.scopes[scope].parent
        }
      }
      r.sym = found
      if found >= 0 {
        prog.symbols[found].use_count = prog.symbols[found].use_count + 1
      }
    }
  }
}
