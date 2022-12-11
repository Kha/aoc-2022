import Aoc.Util

open Stream

structure Monkey where
  op : Array String
  divTest : Nat
  yesTarget : Nat
  noTarget : Nat
  deriving Repr, Inhabited

aoc (input : Split "\n\n" (Lines Words)) => Id.run do
  let monkeys := input.map fun block => {
    op := block[2]![3:]
    divTest := block[3]![3]!.toNat!
    yesTarget := block[4]![5]!.toNat!
    noTarget := block[5]![5]!.toNat!
    : Monkey
  }
  let commonDiv := monkeys.map (·.divTest) |> prod
  let mut items := input.map fun block =>
    block[1]![2:].toArray.map (fun s => (if s.endsWith "," then s.dropRight 1 else s).toNat!)
  let mut activity := mkArray monkeys.size 0
  for _round in [0:10000] do
    for m in [0:monkeys.size] do
      let monkey := monkeys[m]!
      activity := activity.modify m (· + items[m]!.size)
      for item in items[m]! do
        let op s := if s == "old" then item else s.toNat!
        let item := (match monkey.op[1]! with
          | "+" => (· + ·)
          | "*" => (· * ·)
          | _ => unreachable!) (op monkey.op[0]!) (op monkey.op[2]!)
        --let item := item / 3
        let item := item % commonDiv
        items := items.modify (if item % monkey.divTest == 0 then monkey.yesTarget else monkey.noTarget) (·.push item)
      items := items.set! m #[]
  activity := activity.qsort (· > ·)
  activity[0]! * activity[1]!
