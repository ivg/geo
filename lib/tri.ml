
exception No_solution

let pi = 4. *. atan 1.

module Rect =
struct
  let hypot x y =
    let t = min x y in
    let x = max x y in
    let t = t /. x  in
    x *. sqrt ( 1. +. t**2.)
end

let sss a b c =
  acos ((c**2. +. b**2. -. a**2.) /. (2. *. b *. c))

let sas a phi b =
  sqrt (a**2.  +. b**2. -. 2. *. a *. b *. cos phi)

let ssa b c phi =
  let d = c /. b *. sin phi in
  match d with
    | d when d > 1. -> raise No_solution
    | d when d = 1. -> sqrt (c**2. -. b**2.),None
    | d -> if b >= c
      then
        let psi = pi -. phi -. asin d in
        b *. sin psi /. sin phi,None
      else
        let psi  = pi -. phi -. asin d in
        let psi' = pi -. psi -. (pi -. asin d) in
        b *. sin psi  /. sin phi,
        Some (b *. sin psi' /. sin phi)

module Spherical =
struct
  let sss c b a =
    acos ((cos c -. cos a *. cos b) /.  (sin a *. sin b))


  let sas a _C b = acos(sin(a)*.sin(b)*.cos(_C)+.cos(a)*.cos(b))
end
