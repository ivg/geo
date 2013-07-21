type t = float

let pi = 4. *. atan 1.
let null   = 0.
let full   = 2. *. pi
let half   = pi
let right  = pi /. 2.
let radian = 1.
let degree = full /. 360.
let minute = degree /. 60.
let second = minute /. 60.
let rad = radian
let deg = degree
let grad = full /. 400.
let point = full /. 32.
let right = pi /. 2.
let straight = pi
let north = null
let south = pi
let east  = pi /. 2.
let west  = -. pi /. 2.
let north_west = ~-. (atan 1.)
let north_east = atan 1.
let south_west = west -. atan 1.
let south_east = east +. atan 1.

let radian_of_degree x = x *. atan 1. /. 45.
let degree_of_radian x = x *. 45. /. atan 1.

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

let of_dms (d, m, s) = float d *. degree +. float m *. minute +. s *. second

let make f a a' = atan2 (sin (f a a')) (cos (f a a'))
let difference = make (-.)
let sum = make (+.)
let bisector a a' = atan2  (sin a +. sin a') (cos a +. cos a')
let multiply = make ( *. )
let divide = make ( /. )
let times a b = multiply (float a) b
let fraction a b = divide a (float b)
let reverse a =  difference a pi
let complement a = difference right a
let supplement a = difference pi a
let explement a = reverse a

let compare ?epsilon:(eps=sqrt epsilon_float) a a' =
  match sin (a -. a') with
  | n when abs_float n < eps ->  0
  | n when n > 0.            ->  1
  | _                        ->  -1


let cos = cos
let sin = sin
let tan = tan
let acos = acos
let asin = asin
let atan = atan
let atan2 = atan2

let ( + ) a a' = sum a a'
let ( - ) a a' = difference a a'
let ( *. ) n a = multiply n a
let ( * ) n a = times n a
let ( /. ) a n = divide a n
let ( / ) a n = fraction a n
let (~-) = reverse

let identical a a' = a = a'
let (==) = identical
let not_identical a a' = not (identical a a')
let (!=) = not_identical

let less a a' = compare a a' = -1
let (<) = less
let lesseq a a' = not(a' < a)
let (<=) = lesseq
let great a a' = compare a a' = 1
let (>) = great
let greateq a a' = not(a < a')
let (>=) = greateq
let equal a a' = compare a a' = 0
let (=) = equal
let not_equal a a' = not(equal a a')
let (<>) = not_equal

type angle_type =
| Acute                                 (** less than pi/2 *)
| Obtuse                                (** between pi/2 and pi *)
| Right                                 (** exactly pi/2 *)
| Straight                              (** exactly pi *)
| Reflex                                (** greater than pi *)
| Full                                  (** exactly 2*pi *)

let category = function
  | a when a = right            -> Right
  | a when a = straight         -> Straight
  | a when a = full             -> Full
  | a when a < right && a > 0.    -> Acute
  | a when a > right && a < pi   -> Obtuse
  | _                           -> Reflex
