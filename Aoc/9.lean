import Aoc.Util

open Stream

aoc (input : Lines (Pair Char " " Nat)) => Id.run do
  let mut h : Int × Int := (0, 0)
  let mut t : Int × Int := (0, 0)
  let _ := @lexOrd
  let mut hist : Std.RBSet (Int × Int) compare := collect [t]
  for (dir, n) in input do
    for _ in [0:n] do
      h := h + match dir with
        | 'U' => (0, 1)
        | 'D' => (0, -1)
        | 'L' => (-1, 0)
        | 'R' => (1, 0)
        | _ => unreachable!
      let δ := h - t
      if sqrLen δ == 4 then
        t := t + δ / (2 : Int)
      else if sqrLen δ > 2 then
        t := t + δ.map (clamp (-1) 1) (clamp (-1) 1)
      hist := hist.insert t
  hist.size

aoc (input : Lines (Pair Char " " Nat)) => Id.run do
  let mut rope : Array (Int × Int) := mkArray 10 (0, 0)
  let _ := @lexOrd
  let mut hist : Std.RBSet (Int × Int) compare := collect [rope[0]!]
  for (dir, n) in input do
    for _ in [0:n] do
      rope := rope.modify 0 (· + match dir with
        | 'U' => (0, 1)
        | 'D' => (0, -1)
        | 'L' => (-1, 0)
        | 'R' => (1, 0)
        | _ => unreachable!)
      for i in [1:rope.size] do
        let mut t := rope[i]!
        let δ := rope[i-1]! - t
        if sqrLen δ == 4 then
          t := t + δ / (2 : Int)
        else if sqrLen δ > 2 then
          t := t + δ.map (clamp (-1) 1) (clamp (-1) 1)
        rope := rope.set! i t
      hist := hist.insert rope[rope.size-1]!
  hist.size
