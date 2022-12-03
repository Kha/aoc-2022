import Aoc.Util

open Stream (sum)

/-! Encode shapes as `Fin 3` and compute answer using implicit modulo. -/
def Shape (_base : Char) := Fin 3
instance : Inhabited (Shape base) := inferInstanceAs (Inhabited (Fin 3))
instance : Parse (Shape base) where
  parse s := .ofNat <| (Parse.parse s : Char).toNat - base.toNat

aoc (lines : Lines (Shape 'A' × Shape 'X')) =>
  lines.map (fun (a, b) =>
    let shapeScore := b.val + 1
    -- 0 = loss, 1 = draw, 2 = win
    let outcome := b - a
    shapeScore + outcome.val * 3)
  |> sum

aoc (lines : Lines (Shape 'A' × Shape 'X')) =>
  lines.map (fun (a, outcome) =>
    -- 2 = win, remember?
    let b := a + (outcome+2)
    let shapeScore := b.val + 1
    shapeScore + outcome.val * 3)
  |> sum
