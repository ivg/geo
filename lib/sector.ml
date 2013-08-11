open Angle
type a = Angle.t

type t = a * a

let create a a' = a,a'
let of_radian (a,a') = of_radian a, of_radian a'
let to_radian (a,a') = to_radian a, to_radian a'

let of_bisect ~direct ~length =
  direct - length / 2, direct + length / 2

let start (s,_) = s
let finish (_,f) = f

let rec contains b = function
  | a, a' when a <= a' -> b >= a && b <= a'
  | a, a' -> not(contains b (a', a)) || b = a' || b = a

let length (a,a') = a' - a
let radian_of_length s = Angle.to_radian (length s)
let degree_of_length s = Angle.to_degree (length s)
let sort (a, a') = if a < a' then a ,a' else a',a
let bisector (a,a') = bisector a a'
let includes s s' = contains (start s) s' && contains (finish s) s'
let intersects s s' =
  contains (start s) s' || contains (finish s) s' || includes s' s

type borders = Inner | Outer

let borders brd s s' =
  if intersects s s' then
    let sort_by_contains (a,b) =
      if (contains a s) && (contains a s') then b,a
      else a,b in
    let b,b' = sort_by_contains ((start s), (start s')) in
    let e',e = sort_by_contains ((finish s),(finish s')) in
    match brd with
      | Inner -> b',e
      | Outer -> b ,e'
  else
    invalid_arg "Sector.borders: sectors must intersect"

let union sec sec' =
  let borders = borders Outer in
  borders sec sec'

let intersection sec sec' =
  let borders = borders Inner in
  borders sec sec'
