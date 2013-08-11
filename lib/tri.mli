(** Solution of triangles.
    This module provides solutions to common triangle problems.
*)

exception No_solution


(** {2 General triangles}  *)

val sss: float -> float -> float -> float
(** [sss a b c] solves side-side-side problem.
    Given three sides calculates angle opposite to [a]
*)

val sas: float -> float -> float -> float
(** [sas a phi b] solves side-angle-side problem.
    Given two sides and the included angle calculates another
    (opposite) side.
*)

val ssa: float -> float -> float -> float * float option
(** [ssa b c phi] solves side-side-angle problem.

    Given two sides and angle between [c] and [a] returns the third
    side.
    There can be zero, one or two solutions.
    In a case of no soltion exception [No_solution] is raisen.
*)


(** {2 Rectangluar triangles}  *)
module Rect : sig
  val hypot: float -> float -> float
  (** [hypot b c] finds hypotenus  *)
end

module Spherical : sig
  val sss: float -> float -> float -> float
  val sas: float -> float -> float -> float
end
