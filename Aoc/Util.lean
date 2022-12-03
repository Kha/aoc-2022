import Std.Data.HashMap
import Lean.Meta.Reduce
open Std

/-! # Type-guided parsing -/

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
  parse s := s.splitOn sep |>.toArray.map Parse.parse
abbrev Lines := Split "\n"

instance [Parse α] [Parse β] [Inhabited α] [Inhabited β] : Parse (α × β) where
  parse s := Id.run do
    let [a, b] := s.splitOn | panic! s!"prod split {s}"
    (Parse.parse a, Parse.parse b)

/-! # Interactive setup -/

elab "get_filename" : term => do
  let fn ← Lean.MonadLog.getFileName
  Lean.Meta.mkAppM ``System.FilePath.mk #[Lean.mkStrLit fn]

elab "reduced" e:term : term => do
  Lean.Meta.reduce (← Lean.Elab.Term.elabTerm e none)

macro "aoc" "(" id:ident ":" t:term ")" "=>" body:term : command => `(
  def go := (fun ($id : reduced $t) => $body) ∘ Parse.parse
  def main (args : List String) : IO Unit := do
    let input ← IO.FS.readFile ⟨args.head!⟩
    IO.println <| repr <| go input.trim

  #eval main [get_filename |>.withExtension "ex" |>.toString]
  #eval main [get_filename |>.withExtension "input" |>.toString])

/-! # Upstream? -/

notation g "≫" f => f ∘ g
def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum [Add α] [OfNat α 0] (a : Array α) : α := a.foldl (· + ·) 0
def Array.prod [Mul α] [OfNat α 1] (a : Array α) : α := a.foldl (· * ·) 1
def Array.min (a : Array Nat) : Nat := a.foldl Min.min a[0]!
def Array.max (a : Array Nat) : Nat := a.foldl Max.max 0

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

def sqrLen (p : Array Int) := p[0]! * p[0]! + p[1]! * p[1]! + p[2]! * p[2]!
def manhattan (p : Array Int) := abs p[0]! + abs p[1]! + abs p[2]!

def groupBy [Hashable β] [DecidableEq β] (f : α → β) (as : Array α) : HashMap β (Array α) := Id.run do
  let mut map := HashMap.empty
  for a in as do
    let b := f a
    map := map.insert b (map.findD b #[] |>.push a)
  map

def dot [Add α] [Mul α] [OfNat α 0] (as bs : Array α) := as.zipWith bs Mul.mul |>.sum
def cross (as bs : Array Int) := #[as[1]! * bs[2]! - as[2]! * bs[1]!, as[2]! * bs[0]! - as[0]! * bs[2]!, as[0]! * bs[1]! - as[1]! * bs[0]!]
def cardinals := #[#[1, 0, 0], #[-1, 0, 0], #[0, 1, 0], #[0, -1, 0], #[0, 0, 1], #[0, 0, -1]]

def Array.foldMap (f : α → Array β) (as : Array α) : Array β :=
  as.map f |>.foldl (· ++ ·) #[]
