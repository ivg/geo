(** Geographic coordinates *)

(** {6 Data types} *)

type t            (** coordinate *)
type datum        (** geodetic datum *)
type a = Angle.t  (** a type safe angle *)

(** {6 Predefined constants} *)

val wgs84: datum                        (** World Geodetic System 1984  *)
val kras1940: datum                     (** Krasovsky 1940 *)
val earth_radius: float        (** Mean earth radius  *)

(** {6 Constructing} *)
val create: lat:a -> lon:a -> t

val of_degrees: lat:float -> lon:float -> t
val of_radians: lat:float -> lon:float -> t

(** {6 Access} *)

val latitude: t -> a
val longitude: t -> a

val degree_of_latitude: t -> float
val degree_of_longitude: t -> float
val radian_of_latitude: t -> float
val radian_of_longitude: t -> float

(** {6 String formatings} *)
val strfmt: string -> t -> string

(** {6 Operations} *)

val bearing: t -> t -> a
(** [bearing f t] a bearing from [f] to [t] *)
val distance: t -> t -> float
(** [distance f t] a earth distance from [f] to [t]*)
val destination: a -> float -> t -> t
(** [destination b d p] returns [p'] such that [bearing p p' = b] and
    [distance p p' = d].
*)

val to_mercator: ?datum:datum -> t -> float * float
(** [to_mercator ~datum p] returns [x,y] coordinates in mercator projection.
    Default datum is wgs84. *)
