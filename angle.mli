(** Typesafe angles.

    This module abstracts an angle.

    {2 On function principality}

    All functions in this module return values that lies in [-pi,pi]
    interval. We call this values principal.

    The only non principal value is [full] that is equal to full
    revolution or 2xpi. There is no way to create exactly this value by
    yourself. Indeed, [Angle.(full == 2 * pi)] is [false]. Because
    they're not identical.

    {2 On angular ordering}

    When comparing to angles we define an arbitary ordering:
    the angle, that lies counter clockwise to another angle is the
    bigger one. This works fine,


*)
(** {6 Data types} *)

type t                            (** the angle *)

(** {6 Predefined constants}  *)

val radian: t                     (** one radian (approx. 57.3 *)
val degree: t                     (** 1/360 if a circle  *)
val minute: t                     (** 1/60  of a degree  *)
val second: t                     (** 1/60  of a minute  *)

val rad: t                        (** a shortcut for radian  *)
val deg: t                        (** a shortcut for degree  *)

val grad: t                       (** 1/400 of a circle  *)
val point: t                      (** 1/32  of a full circle *)

val pi:t                          (** half of a circle  *)

val null: t                       (** null angle  *)
val full: t                       (** full circle (2*pi)  *)
val right: t                      (** quarter of a circle *)
val straight: t                   (** half of a circle  *)

val north : t
val north_west: t
val north_east: t
val south: t
val south_west: t
val south_east: t
val east: t
val west: t


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
    to angle [rad]. *)
val degree_of_radian: float -> float
(** [degree_of_radian deg] returns an angle, measured in radians, that is equal
    to angle [rad]. *)

(** {6 Compare}  *)

val compare: ?epsilon:float ->  t -> t -> int
(** [compare t t']   *)
val identical: t -> t -> bool
val not_identical: t -> t -> bool
val equal: t -> t -> bool
val less: t -> t -> bool
val lesseq: t -> t -> bool
val great: t -> t -> bool
val greateq: t -> t -> bool

(** {6 Angle typing}  *)
type angle_type =
| Acute                                 (** less than pi/2 *)
| Obtuse                                (** between pi/2 and pi *)
| Right                                 (** exactly pi/2 *)
| Straight                              (** exactly pi *)
| Reflex                                (** greater than pi *)
| Full                                  (** exactly 2*pi *)

val category: t -> angle_type


(** {6 Angular mathematics and transformations}  *)

val bisector: t -> t -> t
val complement: t -> t
val supplement: t -> t
val explement:  t -> t
val non_negative: t -> t


(** { 7 Trigonometry }  *)

val cos: t -> float
val sin: t -> float
val tan: t -> float
val acos: float -> t
val asin: float -> t
val atan: float -> t
val atan2: float -> float -> t

(** {7 Arithmetics}   *)
val sum: t -> t -> t
val difference: t -> t -> t
val multiply: float -> t -> t
val times: int -> t -> t
val divide: t -> float -> t
val fraction: t -> int -> t
val reverse: t -> t

(** {6 Infixes} *)
val ( + ): t -> t -> t
val ( - ): t -> t -> t
val ( *. ): float -> t -> t
val ( * ): int -> t -> t
val ( / ): t -> int -> t
val ( /. ): t -> float -> t
val ( ~-): t -> t

(** {6 Comparasion}  *)
val (<): t -> t -> bool
val (<=): t -> t -> bool
val (>): t -> t -> bool
val (>=): t -> t -> bool
val (=): t -> t -> bool
val (<>): t -> t -> bool
val (==): t -> t -> bool
val (!=): t -> t -> bool
