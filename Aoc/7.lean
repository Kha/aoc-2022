import Aoc.Util

open Stream

inductive Entry where
  | file (sz : Nat)
  | dir (ents : Std.RBNode (String × Entry))
  deriving Repr, Inhabited

def Entry.insert : Entry → List String → Entry → Entry
  | .dir ents, [n], e =>
    .dir (ents.insert (Std.byKey Prod.fst compare) (n, e))
  | .dir ents, n::p, e =>
    .dir (ents.modify (Std.byKey Prod.fst compare (n, e)) (fun (_, e') => (n, e'.insert p e)))
  | _, _, _ => unreachable!

partial def Entry.sumSmall : Entry → StateM Nat Nat
  | .file sz => return sz
  | .dir ents => do
    let sums ← ents.toArray.mapM (·.2.sumSmall)
    let sum := sum sums
    if sum <= 100000 then
      modify (· + sum)
    return sum

variable (minSize : Nat) in
partial def Entry.findDirLe : Entry → StateM Nat Nat
  | .file sz => return sz
  | .dir ents => do
    let sums ← ents.toArray.mapM (·.2.findDirLe)
    let sum := sum sums
    if sum >= minSize then
      modify (min · sum)
    return sum

aoc (input : Split "$ " (Lines Words)) => Id.run do
  let mut fs := Entry.dir ∅
  let mut pwd := []
  for inout in input[1:] do
    let cmd := inout[0]!
    let output := inout[1:]
    match cmd with
    | #["cd", "/"] => pwd := []
    | #["cd", ".."] => pwd := pwd.tail!
    | #["cd", dir] => pwd := dir :: pwd
    | #["ls"] =>
      for ent in output do
        match ent with
        | #["dir", n] => fs := fs.insert (n :: pwd).reverse (.dir ∅)
        | #[size, n]  => fs := fs.insert (n :: pwd).reverse (.file size.toNat!)
        | _ => unreachable!
    | _ => unreachable!
  let (total, smalls) := fs.sumSmall.run 0
  (smalls, fs.findDirLe (30000000 - (70000000 - total)) |>.run 70000000 |>.2)
