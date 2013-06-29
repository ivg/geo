open OUnit
open Printf

module A = Angle
module S = Sector

let d0   = A.of_degree 0.
let d15  = A.of_degree 15.
let d30  = A.of_degree 30.
let d45  = A.of_degree 45.
let d60  = A.of_degree 60.
let d90  = A.of_degree 90.
let d180 = A.of_degree 180.
let d270 = A.of_degree 270.
let d300 = A.of_degree 300.
let d315 = A.of_degree 315.
let d330 = A.of_degree 330.
let d345 = A.of_degree 345.
let d360 = A.of_degree 360.

let md0   = A.of_degree ~-.0.
let md15  = A.of_degree ~-.15.
let md30  = A.of_degree ~-.30.
let md45  = A.of_degree ~-.45.
let md90  = A.of_degree ~-.90.
let md180 = A.of_degree ~-.180.
let md270 = A.of_degree ~-.270.
let md300 = A.of_degree ~-.300.
let md315 = A.of_degree ~-.315.
let md330 = A.of_degree ~-.330.
let md345 = A.of_degree ~-.345.
let md360 = A.of_degree ~-.360.

let pretty_angle a =
  (A.to_degree a)

let string_of_angle a=
  sprintf "%.0f°" (pretty_angle a)
