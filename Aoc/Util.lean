import Std.Data.HashMap
import Std.Data.RBMap
import Lean.Meta.Reduce
open Std

/-!
## Type-guided parsing

No unsuccessful or partial parses.
-/


class Parse (α : Type) where
  parse : String → α

instance : Parse String where
  parse s := s
instance : Parse Nat where
  parse s := s.toNat!
instance : Parse Char where
  parse s := if s.length == 1 then s.get 0 else panic! s!"parse char {s}"

def Split (_sep : String) (α : Type) := Array α
instance [Parse α] : Parse (Split sep α) where
  parse s := s.splitOn sep |>.filter (· != "") |>.toArray.map Parse.parse
instance : Inhabited (Split sep α) where
  default := #[]
abbrev Lines := Split "\n"
abbrev Words := Split " " String

def Pair (α : Type) (_sep : String) (β : Type) := α × β
instance [Inhabited α] [Inhabited β] : Inhabited (Pair α sep β) :=
  inferInstanceAs (Inhabited (α × β))

instance [Parse α] [Parse β] [Inhabited α] [Inhabited β] : Parse (Pair α sep β) where
  parse s := Id.run do
    let [a, b] := s.splitOn sep | panic! s!"prod split {s}"
    (Parse.parse a, Parse.parse b)

/-!
## Interactive setup

Infer input files from current filename and run given expression after parsing.
-/

elab "get_filename" : term => do
  let fn := (← IO.getEnv "LEAN_FILENAME").getD (← Lean.MonadLog.getFileName)
  Lean.Meta.mkAppM ``System.FilePath.mk #[Lean.mkStrLit fn]

elab "reduced" e:term : term => do
  Lean.Meta.reduce (← Lean.Elab.Term.elabTerm e none) (skipTypes := false)

macro "aoc" "(" id:ident ":" t:term ")" "=>" body:term : command => `(
  def go := (fun (__x : $t) => let $id : reduced $t := __x; $body) ∘ Parse.parse
  def main (args : List String) : IO Unit := do
    let input ← IO.FS.readFile ⟨args.head!⟩
    IO.println <| repr <| go input.trimRight

  #eval main [get_filename |>.withExtension "ex" |>.toString]
  #eval main [get_filename |>.withExtension "input" |>.toString]
)

/-! ## Upstream? -/

def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1

partial def List.perms [DecidableEq α] : List α → List (List α)
  | [] => [[]]
  | as => as.bind (fun a => perms (as.filter (· ≠ a)) |>.bind (fun perm => [a::perm]))

partial def List.windowed : List α → List (α × α)
  | a::b::cs => (a, b) :: windowed (b::cs)
  | _ => []

partial def List.pairs (as : List α) (bs : List β) : List (α × β) :=
  as.bind (fun a => bs.map ((a, ·)))

partial def Array.chop (as : Array α) (k : Nat) : Array (Subarray α) :=
  if as.isEmpty then #[]
  else #[as[:k]] ++ as[k:].toArray.chop k

-- oops, not very composable
def Chopped (_n : Nat) (_sep : String) (α : Type) := Array (Array α)
instance [Parse α] : Parse (Chopped n sep α) where
  parse s := s.splitOn sep |>.toArray.chop n |>.map (·.toArray.map Parse.parse)

instance [Add α] : Add (Array α) where
  add as bs := as.zipWith bs Add.add

instance [Sub α] : Sub (Array α) where
  sub as bs := as.zipWith bs Sub.sub

instance [Mul α] : HMul α (Array α) (Array α) where
  hMul a bs := bs.map (a * ·)

def manhattan (p : Array Int) := abs p[0]! + abs p[1]! + abs p[2]!

def groupBy [Hashable β] [DecidableEq β] (f : α → β) (as : Array α) : HashMap β (Array α) := Id.run do
  let mut map := HashMap.empty
  for a in as do
    let b := f a
    map := map.insert b (map.findD b #[] |>.push a)
  map

--def dot [Add α] [Mul α] [OfNat α 0] (as bs : Array α) := as.zipWith bs Mul.mul |>.sum
--def cross (as bs : Array Int) := #[as[1]! * bs[2]! - as[2]! * bs[1]!, as[2]! * bs[0]! - as[0]! * bs[2]!, as[0]! * bs[1]! - as[1]! * bs[0]!]
--def cardinals := #[#[1, 0, 0], #[-1, 0, 0], #[0, 1, 0], #[0, -1, 0], #[0, 0, 1], #[0, 0, -1]]

def Array.foldMap (f : α → Array β) (as : Array α) : Array β :=
  as.map f |>.foldl (· ++ ·) #[]

instance : Inhabited (Subarray α) where
  default := #[].toSubarray

instance [Stream ρ α] : ToStream ρ ρ where
  toStream s := s

def Stream.fold [ToStream ρ ρ'] [Stream ρ' α] (s : ρ) (f : β → α → β) (init : β) : β := Id.run do
  let mut b := init
  for a in toStream s do
    b := f b a
  b

class Collect (α β : Type) where
  collect : α → β

instance [Stream ρ α] : Collect ρ (Std.RBSet α cmp) where
  collect s := Stream.fold s (·.insert) ∅

instance [Stream ρ α] : Collect ρ (Array α) where
  collect s := Stream.fold s (·.push) ∅

instance [Stream ρ α] : Collect ρ (List α) where
  collect s := Stream.fold s (fun as a => a :: as) ∅ |>.reverse

def Stream.collect [ToStream α β] [Collect β γ] (a : α) : γ :=
  Collect.collect (toStream a)

def Stream.toArray [ToStream α β] [Stream β γ] [Collect β (Array γ)] (a : α) : (Array γ) :=
  Stream.collect a

def Stream.sum [ToStream α β] [Stream β γ] [Add γ] [OfNat γ 0] (a : α) : γ :=
  Stream.fold a (· + ·) 0

def Stream.prod [ToStream α β] [Stream β γ] [Mul γ] [OfNat γ 1] (a : α) : γ :=
  Stream.fold a (· * ·) 1

def Stream.head! [ToStream α β] [Stream β γ] [Inhabited β] [Inhabited γ] (a : α) : γ :=
  Stream.next? (toStream a) |>.get!.1

def Stream.min! [ToStream α β] [Stream β γ] [Inhabited β] [Inhabited γ] [Min γ] (a : α) : γ :=
  Stream.fold a min (Stream.head! a)

def Stream.max! [ToStream α β] [Stream β γ] [Inhabited β] [Inhabited γ] [Max γ] (a : α) : γ :=
  Stream.fold a max (Stream.head! a)

structure RangeInclusive (α : Type) where
  start : α
  stop : α

namespace RangeInclusive

infix:10 "..=" => RangeInclusive.mk

instance [Inhabited α] : Inhabited (RangeInclusive α) where
  default := default..=default

instance [Repr α] : Repr (RangeInclusive α) where
  reprPrec r _ := s!"{repr r.start}..={repr r.stop}"

variable [LE α] [DecidableRel (@LE.le α _)]

def subset (r1 r2 : RangeInclusive α) : Bool :=
  r1.start >= r2.start && r1.stop <= r2.stop

def contains (r1 : RangeInclusive α) (a : α) : Bool :=
  r1.start <= a && a <= r1.stop

def overlaps (r1 r2 : RangeInclusive α) : Bool :=
  r1.contains r2.start || r1.contains r2.stop || r1.subset r2

end RangeInclusive

instance [Parse α] [Inhabited α] : Parse (RangeInclusive α) where
  parse s := Id.run do
    let [a, b] := s.splitOn "-" | unreachable!
    Parse.parse a..=Parse.parse b

instance [Add α] : Add (α × α) where
  add a b := (a.1 + b.1, a.2 + b.2)

instance [Sub α] : Sub (α × α) where
  sub a b := (a.1 - b.1, a.2 - b.2)

instance [HDiv α β γ] : HDiv (α × α) β (γ × γ) where
  hDiv a b := (a.1 / b, a.2 / b)

def sqrLen (p : Int × Int) : Int := p.1 * p.1 + p.2 * p.2

def clamp (low high i : Int) : Int := max low (min high i)

namespace Stream

structure Indexed (ρ : Type) where
  s : ρ
  i : Nat

def indexed [ToStream α ρ] (s : α) : Indexed ρ := ⟨toStream s, 0⟩

instance [Stream ρ α] : Stream (Indexed ρ) (α × Nat) where
  next? i := next? i.s |>.map fun (a, s') => ((a, i.i), ⟨s', i.i + 1⟩)

def findSomeM? [Stream ρ α] [Monad m] (f : α → m (Option β)) (s : ρ) : m (Option β) := do
  for a in s do
    match (← f a) with
    | some b => return b
    | _      => pure ⟨⟩
  return none

def findSome? [Stream ρ α] (p : α → Option β) (s : ρ) : Option β :=
  findSomeM? p s |> Id.run

end Stream

instance [Inhabited α] : GetElem (Array (Array α)) (Int × Int) α
    (fun m (x, y) => 0 <= y && y < m.size && 0 <= x && x < m[y.toNat]!.size) where
  getElem := fun m (x, y) _ => m[y.toNat]![x.toNat]!
