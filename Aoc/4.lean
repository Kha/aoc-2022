import Aoc.Util

aoc (pairs : Lines (Pair (RangeInclusive Nat) "," (RangeInclusive Nat))) =>
  pairs.filter (fun (r1, r2) => r1.subset r2 || r2.subset r1)
  |>.size

aoc (pairs : Lines (Pair (RangeInclusive Nat) "," (RangeInclusive Nat))) =>
  pairs.filter (fun (r1, r2) => r1.overlaps r2)
  |>.size
