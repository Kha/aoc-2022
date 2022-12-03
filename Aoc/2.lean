import Aoc.Util

aoc (lines : Lines (Char × Char)) =>
  lines.map (fun (a, b) =>
    -- opponent play, 0-2
    let a := a.toNat - 'A'.toNat
    -- response, 0-2
    let b := b.toNat - 'X'.toNat
    let shapeScore := b + 1
    -- 0 = loss, 1 = draw, 2 = win
    let outcome := (b + 4 - a) % 3
    shapeScore + outcome * 3)
  |>.sum

aoc (lines : Lines (Char × Char)) =>
  lines.map (fun (a, outcome) =>
    let a := a.toNat - 'A'.toNat
    let outcome := outcome.toNat - 'X'.toNat
    let b := (a + (outcome+2)) % 3
    let shapeScore := b + 1
    shapeScore + outcome * 3)
  |>.sum
