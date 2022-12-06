import Aoc.Util

open Stream

aoc (input : String) => Id.run do
  let input : Array Char := collect input
  let n := 4
  for i in [n:input.size] do
    let cs := input[i-n:i]
    let cs : Std.RBSet Char compare := collect cs
    if cs.size == n then
      return some i
  return none

aoc (input : String) => Id.run do
  let input : Array Char := collect input
  let n := 14
  for i in [n:input.size] do
    let cs := input[i-n:i]
    let cs : Std.RBSet Char compare := collect cs
    if cs.size == n then
      return some i
  return none
