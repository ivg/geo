module A = Angle
type a = A.t

type t =
  | Empty
  | Full
  | Horizontal of (a * a)
  | Vertical   of (a * a)

let pi_2 = A.of_radian (2. *. atan(1.))
let pi_m2 = A.of_radian (8. *. atan(1.))
let zero = A.of_radian 0.

let valid_ver a =  a >= zero  && a <= pi_2

let empty () = Empty
let full () = Full

let vertical a a' =
  if (valid_ver a) && (valid_ver a') then
    Vertical (a,a')
  else
    invalid_arg "Sector.vertical"

let horizontal a a' = Horizontal (a,a')

let of_radian (a,a') =
  let a  = (A.of_radian a) in
  let a' = A.of_radian a' in
  a,a'

let to_radian (a,a') =
  let a  = A.normalized_radian (A.to_radian a) in
  let a' = A.normalized_radian (A.to_radian a') in
  a,a'


let of_bisect a r =
  let a,r = to_radian (a,r) in
  let r2 = r /. 2. in
  let b = A.of_radian (a -. r2) in
  let e = A.of_radian (a +. r2) in
  horizontal b e

let start = function
  | Horizontal (a,_) | Vertical (a,_) -> a
  | _ -> A.of_radian 0.


let finish = function
  | Horizontal (_,a') | Vertical (_,a') -> a'
  | _ -> A.of_radian 0.

let rec contains b = function
  | Full -> true
  | Empty -> false
  | Vertical (a,a') | Horizontal (a, a')
      when a <= a' -> b >= a && b <= a'
  | Horizontal (a, a') ->
      not (contains b (Horizontal (a', a)))
      || b = a' || b = a
  | _ -> failwith "contains"

let length = function
  | Empty -> zero
  | Full  -> pi_m2
  | Vertical (a,a')
  | Horizontal (a, a') ->
      let (-) a a' =
        let a,a' = to_radian (a, a') in
        A.of_radian (a -. a') in
      match (a,a') with
        | a,a' when a < a' -> a' - a
        | a,a' ->  pi_m2 - (a - a')

let radian_of_length = function
  | Full -> 8. *. atan(1.)
  | s -> A.to_radian (length s)

let degree_of_length = function
  | Full -> 360.
  | s -> A.to_degree (length s)

let sort (a, a') = if a < a'
  then a ,a'
  else a',a


let bisect = function
  | Empty | Full -> zero
  | Vertical (a,a') | Horizontal (a, a') as s ->
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
  match (sec,sec') with
    | Empty, s | s, Empty -> s
    | Full, s  | s, Full  -> Full
    | Horizontal s, Horizontal s' -> Horizontal (borders sec sec')
    | Vertical s, Vertical s'     -> Vertical   (borders sec sec')
    | _ ->  invalid_arg "Sector.union: sectors mismatch"

let intersection sec sec' =
  let borders = borders Inner in
  match (sec,sec') with
    | Empty, s | s, Empty -> Empty
    | Full, s  | s, Full  -> s
    | Horizontal s, Horizontal s' -> Horizontal (borders sec sec')
    | Vertical s, Vertical s'     -> Vertical   (borders sec sec')
    | _ -> invalid_arg "Sector.intersection: sectors must match"
