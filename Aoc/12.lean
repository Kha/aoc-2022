import Aoc.Util

open Stream

def cardinals := #[(1, 0), (0, -1), (-1, 0), (0, 1)]

aoc (input : Lines String) => Id.run do
  let input := input.map toArray
  let .some s := indexed input |> findSome? fun (line, y) =>
    indexed line |> findSome? fun
      | ('S', x) => some (Int.ofNat x, Int.ofNat y)
      | _ => none
    | unreachable!
  let _ := @lexOrd
  let mut q := Std.RBSet.single (cmp := compare) (0, s)
  let mut r := input.map (·.map (fun _ => none))
  while !q.isEmpty do
    let (w, p) := q.min!
    let c := input[p]!
    if c == 'E' then
      return some w
    let c := if c == 'S' then 'a' else c
    r := r.modify p.2.toNat (·.set! p.1.toNat (some w))
    q := q.erase (compare (w, p))
    for d in cardinals do
      let p' := p + d
      if let some c' := input[p']? then
        let c' := if c' == 'E' then 'z' else c'
        if let none := r[p']! then
          if c'.toNat - c.toNat <= 1 then
            q := q.insert (w + 1, p')
  none

aoc (input : Lines String) => Id.run do
  let input := input.map toArray
  let starts := indexed input |> toArray |>.concatMap fun (line, y) =>
    indexed line |> toArray |>.filterMap fun
      | ('S', x) => some (Int.ofNat x, Int.ofNat y)
      | ('a', x) => some (Int.ofNat x, Int.ofNat y)
      | _ => none
  let _ := @lexOrd
  let mut q : Std.RBSet (Nat × (Int × Int)) compare := collect (starts.map (0, ·))
  let mut r := input.map (·.map (fun _ => none))
  while !q.isEmpty do
    let (w, p) := q.min!
    let c := input[p]!
    if c == 'E' then
      return some w
    let c := if c == 'S' then 'a' else c
    r := r.modify p.2.toNat (·.set! p.1.toNat (some w))
    q := q.erase (compare (w, p))
    for d in cardinals do
      let p' := p + d
      if let some c' := input[p']? then
        let c' := if c' == 'E' then 'z' else c'
        if let none := r[p']! then
          if c'.toNat - c.toNat <= 1 then
            q := q.insert (w + 1, p')
  none
