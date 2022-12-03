import Aoc.Util

open Stream (sum max!)

/-! Take maximum sum. -/
aoc (blocks : Split "\n\n" (Lines Nat)) =>
  blocks.map sum |> max!

/-! Take subarray of largest three sums and sum it. -/
aoc (blocks : Split "\n\n" (Lines Nat)) =>
  blocks.map sum |>.qsort (· > ·) |> (·[:3]) |> sum
