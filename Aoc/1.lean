import Aoc.Util

aoc (blocks : Split "\n\n" (Lines Nat)) =>
  blocks.map (·.sum) |>.max

aoc (blocks : Split "\n\n" (Lines Nat)) =>
  blocks.map (·.sum) |>.qsort (· > ·) |> (·[:3]) |>.toArray.sum
