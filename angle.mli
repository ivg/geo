(** Typesafe angles.

    This module abstracts an angle.
*)
(** {6 Data types} *)

type t                                  (** the angle *)

(** {6 Predefined constant}  *)

val pi: t      (** the ratio of a circle's circumference to its diameter  *)

val double_pi: t                        (** 2 x pi  *)
val half_pi: t                          (** pi / 2  *)

(** {6 Converting to or from floats} *)

val of_radian: float -> t
val to_radian: t -> float
val of_degree: float -> t
val to_degree: t -> float
val of_dms: int * int * float -> t
val to_dms: t -> int * int * float


(** {6 Conversion degree/radian}  *)
val radian_of_degree: float -> float
(** [radian_of_degree rad] returns an angle, measured in degrees, that is equal
    to angle [rad]. The returned value is normalized. *)
val degree_of_radian: float -> float
(** [degree_of_radian deg] returns an angle, measured in radians, that is equal
    to angle [rad]. The returned value is normalized. *)

(** {6 Compare}  *)

val compare: ?epsilon:float ->  t -> t -> int
(** [compare t t'] returns [-1] when angle [t] is more acute, [+1] if more obtuse
    and [0] if they're equal.  *)
val equal: t -> t -> bool
val less: t -> t -> bool
val lesseq: t -> t -> bool
val great: t -> t -> bool
val greateq: t -> t -> bool

val is_acute: t -> bool
val is_obtuse: t -> bool


(** {6 Angular mathematics}  *)
(** { 7 Trigonometry }  *)

val cos: t -> float
val sin: t -> float
val tan: t -> float
val acos: float -> t
val asin: float -> t
val atan: float -> t
val atan2: float -> float -> t

(** {7 Arithmetics}   *)
val bisector: t -> t -> t
val sum: t -> t -> t
val difference: t -> t -> t
val multiply: t -> float -> t
val reverse: t -> t

(** {6 Infixes} *)
val ( + ): t -> t -> t
val ( - ): t -> t -> t
val ( * ): t -> float -> t

val (<): t -> t -> bool
val (<=): t -> t -> bool
val (>): t -> t -> bool
val (>=): t -> t -> bool
val (=): t -> t -> bool
val (<>): t -> t -> bool
