type t = float

let pi = 4. *. atan 1.
let double_pi = 2. *. pi
let half_pi = pi /. 2.

let radian_of_degree deg = deg *. atan 1. /. 45.
let degree_of_radian rad = rad *. 45. /. atan 1.

let of_radian r = r
let to_radian r = r
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

let compare ?epsilon:(eps=sqrt epsilon_float) a a' =
  match sin (a -. a') with
  | n when abs_float n < eps ->  0
  | n when n > 0.            ->  1
  | _                        -> -1

let difference a a' = a -. a'
let sum a a' = a +. a'

let bisector a a' = atan2  (sin a +. sin a') (cos a +. cos a')
let multiply a n = a *. n
let reverse a =  a -. pi

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

let less a a' = compare a a' = -1
let lesseq a a' = not(a' < a)
let great a a' = compare a a' = 1
let greateq a a' = not(a < a')
let equal a a' = compare a a' = 0
let not_equal a a' = not(equal a a')


let (<) = less
let (<=) = lesseq
let (>) = great
let (>=) = greateq
let (=) = equal
let (<>) = not_equal

let is_acute a = a < half_pi
let is_obtuse a = a > half_pi
