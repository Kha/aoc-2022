import Aoc.Util

open Stream

aoc (input : Lines Words) => Id.run do
  let mut cycle := 0
  let mut x : Int := 1
  let mut outs : Array Int := #[]
  for cmd in input do
    let (cycle', x') := match cmd with
      | #["noop"] => (cycle + 1, x)
      | #["addx", n] => (cycle + 2, x + n.toInt!)
      | _ => unreachable!
    for c in [cycle+1:cycle'+1] do
      if (c + 20) % 40 == 0 then
        outs := outs.push (c * x)
    (cycle, x) := (cycle', x')
  return sum outs

instance : Repr (Array String) where
  reprPrec ss _ := "\n".intercalate ss.toList

aoc (input : Lines Words) => Id.run do
  let mut cycle := 0
  let mut x : Int := 1
  let mut crt : Array (Array Char) := mkArray 6 (mkArray 40 '.')
  for cmd in input do
    let (cycle', x') := match cmd with
      | #["noop"] => (cycle + 1, x)
      | #["addx", n] => (cycle + 2, x + n.toInt!)
      | _ => unreachable!
    for c in [cycle:cycle'] do
      let p := (c % 40, c / 40)
      if abs (Int.ofNat p.1 - x) <= 1 then
        crt := crt.modify p.2 (·.set! p.1 '#')
    (cycle, x) := (cycle', x')
  crt.map (String.mk ∘ Array.toList)
