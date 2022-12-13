import Aoc.Util
import Lean.Parser.Module

open Stream

inductive Packet where
  | nat (n : Nat)
  | list (ps : List Packet)
  deriving Inhabited, Repr

instance : ToString Packet where
  toString p := repr p |> toString

partial def Packet.parse (env : Lean.Environment) (s : String) : Packet :=
  go (Lean.Parser.runParserCategory env `term s |>.toOption.get! |> Lean.TSyntax.mk)
where go : Lean.Term → Packet
  | `($n:num) => .nat n.getNat
  | `([$ps,*]) => .list (ps.getElems.toList.map go)
  | _ => unreachable!

protected partial def Packet.compare : Packet → Packet → Ordering
  | .nat n, .nat m => compare n m
  | .list (p::ps), .list (q::qs) => p.compare q |>.then ((Packet.list ps).compare (.list qs))
  | .list [], .list [] => .eq
  | .list [], .list _ => .lt
  | .list _, .list [] => .gt
  | p, .nat m => p.compare (.list [.nat m])
  | .nat n, q => (Packet.list [.nat n]).compare q

instance : Ord Packet := ⟨Packet.compare⟩
instance : LE Packet := leOfOrd

def go (env : Lean.Environment) (input : Split "\n\n" (Lines String)) := Id.run do
  let input : Array (Array String) := input
  return input |> indexed |> toArray
    |>.filter (fun (block, _) => Packet.parse env block[0]! <= Packet.parse env block[1]!)
    |>.map (·.2 + 1)
    |> sum

open Lean in
#eval show MetaM _ from do
  let input ← IO.FS.readFile "Aoc/13.input"
  let env ← getEnv
  IO.println <| repr (go env (Parse.parse input))

instance : LT Packet := ltOfOrd
instance : BEq Packet := ⟨(compare · · == .eq)⟩

def go2 (env : Lean.Environment) (input : Lines String) := Id.run do
  let input := input.map (Packet.parse env)
  let distr1 := Packet.list [.list [.nat 2]]
  let distr2 := Packet.list [.list [.nat 6]]
  let input := input.push distr1 |>.push distr2
  let sorted := input.qsort (· < ·)
  let idx1 := sorted.findIdx? (· == distr1) |>.get!
  let idx2 := sorted.findIdx? (· == distr2) |>.get!
  (idx1 + 1) * (idx2 + 1)

open Lean in
#eval show MetaM _ from do
  let input ← IO.FS.readFile "Aoc/13.input"
  let env ← getEnv
  IO.println <| repr (go2 env (Parse.parse input))
