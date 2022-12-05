import Aoc.Util

aoc (input : Pair (Lines String) "\n\n" (Lines Words)) => Id.run do
  let crates := input.1.pop
  let numsLine := input.1[input.1.size-1]!
  let numCols := numsLine.splitOn |>.filter (!·.isEmpty) |>.length
  let mut stacks :=
    List.range numCols |>.toArray.map (fun col =>
      crates.filterMap (·.get? ⟨1 + 4*col⟩ |>.filter (· != ' ')) |>.toList)

  for cmd in input.2 do
    let #[_, n, _, from_, _, to] := cmd | unreachable!
    let from_ := from_.toNat! - 1
    let to := to.toNat! - 1
    for _ in [0:n.toNat!] do
      let .cons c rest := stacks[from_]! | unreachable!
      stacks := stacks.set! from_ rest |>.modify to (c :: ·)
  List.range numCols |>.map (stacks[·]!.head!) |> String.mk

aoc (input : Pair (Lines String) "\n\n" (Lines Words)) => Id.run do
  let crates := input.1.pop
  let numsLine := input.1[input.1.size-1]!
  let numCols := numsLine.splitOn |>.filter (!·.isEmpty) |>.length
  let mut stacks :=
    List.range numCols |>.toArray.map (fun col =>
      crates.filterMap (·.get? ⟨1 + 4*col⟩ |>.filter (· != ' ')) |>.toList)

  for cmd in input.2 do
    let #[_, n, _, from_, _, to] := cmd | unreachable!
    let from_ := from_.toNat! - 1
    let to := to.toNat! - 1
    let n := n.toNat!
    stacks := stacks.modify from_ (·.drop n) |>.modify to (stacks[from_]!.take n ++ ·)
  List.range numCols |>.map (stacks[·]!.head!) |> String.mk
