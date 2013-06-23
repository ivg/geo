type t = float

let pi = 4. *. atan 1.
let double_pi = 2. *. pi
let half_pi = pi /. 2.

let normalized_degree deg = mod_float (deg +. 360.) 360.
let normalized_radian rad = mod_float (rad +. double_pi) (double_pi)
let radian_of_degree deg = normalized_radian (deg *. atan(1.) /. 45.)
let degree_of_radian rad = normalized_degree (rad *.  45. /. atan(1.))

let of_radian r = normalized_radian r
let to_radian r = normalized_radian r
let of_degree = radian_of_degree
let to_degree = degree_of_radian

let to_dms rad =
  let deg = degree_of_radian rad in
  let f,d = modf deg in
  let f,m = modf (f *. 60.) in
  let s = f *. 60. in
  int_of_float d,
  int_of_float m,
  s

let of_dms (d, m, s) =
  of_degree
    ((float_of_int d) +.
    ((float_of_int m) /. 60.) +. s /. 3600.)

let compare a a' =
  let r = a -. a' in
  let eps = sqrt epsilon_float in
  if   (abs_float r) < eps then 0
  else match r with
    | r when r > 0. -> 1
    | _ -> -1


let difference a a' = normalized_radian (a -. a')
let sum a a' = normalized_radian (a +. a')
let bisector a a' = normalized_radian ((sum a a') /. 2.)
let bisector a a' = normalized_radian (a +. (difference a' a) /. 2.)
let multiply a n = normalized_radian (a *. n)
let reverse a =  normalized_radian (a -. pi)

let is_acute a = compare a half_pi = -1
let is_obtuse a = compare a half_pi = 1


let cos = cos
let sin = sin
let tan = tan
let acos = acos
let asin = asin
let atan = atan
let atan2 = atan2

let ( + ) a a' = sum a a'
let ( - ) a a' = difference a a'
let ( * ) a n = multiply a n

let (<) a a' = -1 = (compare a a')
let (<=) a a' = not (a' < a)
let (>) a a' = 1 = (compare a a')
let (>=) a a' = not (a < a')
let (=) a a' = 0 = (compare a a')
let (<>) a a' = not (a = a')
