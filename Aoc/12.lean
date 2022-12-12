import Aoc.Util

open Stream

namespace Stream

structure Indexed (ρ : Type) where
  s : ρ
  i : Nat

def indexed [ToStream α ρ] (s : α) : Indexed ρ := ⟨toStream s, 0⟩

instance [Stream ρ α] : Stream (Indexed ρ) (α × Nat) where
  next? i := next? i.s |>.map fun (a, s') => ((a, i.i), ⟨s', i.i + 1⟩)

def findSomeM? [Stream ρ α] [Monad m] (f : α → m (Option β)) (s : ρ) : m (Option β) := do
  for a in s do
    match (← f a) with
    | some b => return b
    | _      => pure ⟨⟩
  return none

def findSome? [Stream ρ α] (p : α → Option β) (s : ρ) : Option β :=
  findSomeM? p s |> Id.run

end Stream

def cardinals := #[(1, 0), (0, -1), (-1, 0), (0, 1)]

instance [Coe α β] : Coe (α × α) (β × β) where
  coe := fun (a, b) => (a, b)

aoc (input : Lines String) => Id.run do
  let input := input.map toArray
  let .some s := indexed input |> findSome? fun (line, y) =>
    indexed line |> findSome? fun
      | ('S', x) => some (x, y)
      | _ => none
    | unreachable!
  let _ := @lexOrd
  let mut q := Std.RBSet.single (cmp := compare) (0, s)
  let mut r := input.map (·.map (fun _ => none))
  while !q.isEmpty do
    let (w, p) := q.min!
    let c := input[p.2]![p.1]!
    if c == 'E' then
      return some w
    let c := if c == 'S' then 'a' else c
    r := r.modify p.2 (·.set! p.1 (some w))
    q := q.erase (compare (w, p))
    for d in cardinals do
      let p' := p + d
      if p'.1 >= 0 && p'.1 < input[0]!.size &&
         p'.2 >= 0 && p'.2 < input.size then
        let p' := (p'.1.toNat, p'.2.toNat)
        let c' := input[p'.2]![p'.1]!
        let c' := if c' == 'E' then 'z' else c'
        if let none := r[p'.2]![p'.1]! then
          if c'.toNat - c.toNat <= 1 then
            q := q.insert (w + 1, p')
  none

aoc (input : Lines String) => Id.run do
  let input := input.map toArray
  let starts := indexed input |> toArray |>.concatMap fun (line, y) =>
    indexed line |> toArray |>.filterMap fun
      | ('S', x) => some (x, y)
      | ('a', x) => some (x, y)
      | _ => none
  let _ := @lexOrd
  let mut q : Std.RBSet (Nat × (Nat × Nat)) compare := collect (starts.map (0, ·))
  let mut r := input.map (·.map (fun _ => none))
  while !q.isEmpty do
    let (w, p) := q.min!
    let c := input[p.2]![p.1]!
    if c == 'E' then
      return some w
    let c := if c == 'S' then 'a' else c
    r := r.modify p.2 (·.set! p.1 (some w))
    q := q.erase (compare (w, p))
    for d in cardinals do
      let p' := p + d
      if p'.1 >= 0 && p'.1 < input[0]!.size &&
         p'.2 >= 0 && p'.2 < input.size then
        let p' := (p'.1.toNat, p'.2.toNat)
        let c' := input[p'.2]![p'.1]!
        let c' := if c' == 'E' then 'z' else c'
        if let none := r[p'.2]![p'.1]! then
          if c'.toNat - c.toNat <= 1 then
            q := q.insert (w + 1, p')
  none
