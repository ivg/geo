module A = Angle
type a = A.t

type t = a * a

let zero = A.of_radian 0.


let create a a' = a,a'

let of_radian (a,a') =
  let a  = A.of_radian a  in
  let a' = A.of_radian a' in
  a,a'

let to_radian (a,a') =
  let a  = A.to_radian a   in
  let a' = A.to_radian a'  in
  a,a'


let of_bisect ~direct ~length =
  let a,r = to_radian (direct,length) in
  let r2 = r /. 2. in
  let b = A.of_radian (a -. r2) in
  let e = A.of_radian (a +. r2) in
  b,e

let start (s,_) = s
let finish (_,f) = f

let rec contains b = function
  | a, a' when a <= a' -> b >= a && b <= a'
  | a, a' -> not(contains b (a', a)) || b = a' || b = a

let length (a,a') =
  let (-) a a' =
    let a,a' = to_radian (a, a') in
    A.of_radian (a -. a') in
  match (a,a') with
  | a,a' when a < a' -> a' - a
  | a,a' ->  A.double_pi - (a - a')

let radian_of_length s = A.to_radian (length s)

let degree_of_length s = A.to_degree (length s)

let sort (a, a') = if a < a' then a ,a' else a',a

let bisect ((a,a') as s) =
  let add_half a a' =
    let a,a' = to_radian (a,a') in
    A.of_radian (a +. a' /. 2.) in
  add_half a (length s)

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
