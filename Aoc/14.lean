import Aoc.Util
import Lean.Parser.Module

open Stream

instance : Parse Int where
  parse s := s.toNat!

instance [Mul α] : HMul α (α × α) (α × α) where
  hMul a p := (a * p.1, a * p.2)

instance [Div α] : HDiv (α × α) α (α × α) where
  hDiv p d := (p.1 / d, p.2 / d)

aoc (input : Lines (Split " -> " (Pair Int "," Int))) => Id.run do
  let maxY := input.map (·.map (·.2) |> max!) |> max!
  let _ := @lexOrd
  let mut a := Std.mkRBSet (Int × Int) compare
  for line in input do
    for (p1, p2) in line.toList.windowed do
      let δ := p2 - p1
      let δlen := sqrLen δ |>.toNat.sqrt
      for i in [0:δlen+1] do
        let p' := p1 + Int.ofNat i * δ / (Int.ofNat δlen)
        a := a.insert p'
  let mut nSand := 0
  while true do
    let mut p := (500, 0)
    while p.2 <= maxY do
      let mut newP? := none
      for dx in [0, -1, 1] do
        if !a.contains (p + (dx, 1)) then
          newP? := some <| p + (dx, 1)
          break
      if let some newP := newP? then
        p := newP
      else
        a := a.insert p
        nSand := nSand + 1
        break
    if p.2 > maxY then
      break
  nSand

aoc (input : Lines (Split " -> " (Pair Int "," Int))) => Id.run do
  let maxY := input.map (·.map (·.2) |> max!) |> max!
  let _ := @lexOrd
  let mut a := Std.mkRBSet (Int × Int) compare
  for line in input do
    for (p1, p2) in line.toList.windowed do
      let δ := p2 - p1
      let δlen := sqrLen δ |>.toNat.sqrt
      for i in [0:δlen+1] do
        let p' := p1 + Int.ofNat i * δ / (Int.ofNat δlen)
        a := a.insert p'
  let mut nSand := 0
  while true do
    let mut p := (500, 0)
    while p.2 <= maxY do
      let mut newP? := none
      for dx in [0, -1, 1] do
        if !a.contains (p + (dx, 1)) then
          newP? := some <| p + (dx, 1)
          break
      if let some newP := newP? then
        p := newP
      else
        a := a.insert p
        nSand := nSand + 1
        break
    if p.2 > maxY then
      a := a.insert p
      nSand := nSand + 1
    if p = (500, 0) then
      break
  nSand
