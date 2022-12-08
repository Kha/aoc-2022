import Aoc.Util

open Stream

aoc (input : Lines String) => Id.run do
  let input := input.map (·.toList.toArray.map (·.toNat - '0'.toNat))
  let markVis lineIdcs vis := Id.run do
    let mut min : Int := -1
    let mut vis := vis
    for (x, y) in lineIdcs do
      let h := input[y]![x]!
      if h > min then
        vis := vis.modify y (·.set! x true)
        min := h
    vis

  let h := input.size
  let w := input[0]!.size
  let mut vis := input.map fun _ => Array.mkArray w false
  for y in [0:h] do
    vis := markVis (List.range w |>.map ((·, y))) vis
    vis := markVis (List.range w |>.map ((w-1-·, y))) vis

  for x in [0:w] do
    vis := markVis (List.range h |>.map ((x, ·))) vis
    vis := markVis (List.range h |>.map ((x, h-1-·))) vis
  return vis.map (·.filter id |>.size) |> sum

-- TODO: use actual 2D matrix
instance [Inhabited α] : GetElem (Array (Array α)) (Nat × Nat) α (fun _ _ => true) where
  getElem := fun m (x, y) _ => m[y]![x]!

aoc (input : Lines String) => Id.run do
  let input := input.map (·.toList.toArray.map (·.toNat - '0'.toNat))
  let getViewDist lineIdcs := Id.run do
    let t := input[lineIdcs.head!]!
    match lineIdcs.tail!.findIdx? (input[·]! >= t) with
      | some i => i + 1
      | none   => lineIdcs.length - 1

  let h := input.size
  let w := input[0]!.size
  let mut maxScore := 0
  for x in [0:w] do
    for y in [0:h] do
      maxScore := max maxScore <| prod #[
        getViewDist (List.range (w-x) |>.map ((x+·, y))),
        getViewDist (List.range (x+1) |>.map ((x-·, y))),
        getViewDist (List.range (h-y) |>.map ((x, y+·))),
        getViewDist (List.range (y+1) |>.map ((x, y-·)))
      ]
  maxScore
