import Aoc.Util

aoc (lines : Lines String) =>
  lines.map (fun line => Id.run do
    let line := line.data.toArray
    let a := line[0:line.size/2]
    let b := line[line.size/2:]
    let inA : Std.RBSet _ compare := Stream.collect a
    let c := b.toArray.find? (inA.contains) |>.get!
    if c.isLower then c.toNat - 'a'.toNat + 1 else c.toNat - 'A'.toNat + 27)
  |>.sum

aoc (blocks : Chopped 3 "\n" String) =>
  blocks.map (fun block => Id.run do
    let #[a, b, c] := block | unreachable!
    let inA : Std.RBSet _ compare := Stream.collect a
    let inB : Std.RBSet _ compare := Stream.collect b
    let c := c.data.toArray.find? (fun c => inA.contains c && inB.contains c) |>.get!
    if c.isLower then c.toNat - 'a'.toNat + 1 else c.toNat - 'A'.toNat + 27)
  |>.sum
