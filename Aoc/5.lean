import Aoc.Util

aoc (input : Pair (Lines String) "\n\n" (Lines Words)) => Id.run do
  let crates := input.1.pop
  let numsLine := input.1[input.1.size-1]!
  let numCols := numsLine.splitOn |>.filter (!·.isEmpty) |>.length
  let mut stacks :=
    List.range numCols |>.map (fun col =>
      crates.filterMap (fun line =>
        match line.get? ⟨1 + 4*col⟩ with
        | some ' ' => none
        | c? => c?)
      |>.toList)
    |>.toArray

  for cmd in input.2 do
    let #[_, n, _, from_, _, to] := cmd | unreachable!
    let from_ := from_.toNat! - 1
    let to := to.toNat! - 1
    for _ in [0:n.toNat!] do
      let .cons c rest := stacks[from_]! | unreachable!
      stacks := stacks.set! from_ rest |>.set! to (c :: stacks[to]!)
  List.range numCols |>.map (fun col => stacks[col]!.head!) |> String.mk

aoc (input : Pair (Lines String) "\n\n" (Lines Words)) => Id.run do
  let crates := input.1.pop
  let numsLine := input.1[input.1.size-1]!
  let numCols := numsLine.splitOn |>.filter (!·.isEmpty) |>.length
  let mut stacks :=
    List.range numCols |>.map (fun col =>
      crates.filterMap (fun line =>
        match line.get? ⟨1 + 4*col⟩ with
        | some ' ' => none
        | c? => c?)
      |>.toList)
    |>.toArray

  for cmd in input.2 do
    let #[_, n, _, from_, _, to] := cmd | unreachable!
    let from_ := from_.toNat! - 1
    let to := to.toNat! - 1
    let n := n.toNat!
    stacks := stacks.set! from_ (stacks[from_]!.drop n) |>.set! to (stacks[from_]!.take n ++ stacks[to]!)
  List.range numCols |>.map (fun col => stacks[col]!.head!) |> String.mk
