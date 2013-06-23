(** Sector. *)

(** {6 Типы данных} *)
type t
type a = Angle.t

(** {6 Creation} *)
val create: a -> a -> t
val of_bisect: direct:a -> length:a -> t
(** [bisect b r]   *)


(**{6 Access} *)
val start: t -> a
(** [start s] [s]*)
val finish: t -> a
(** [finish s] [s] *)
val contains: a -> t -> bool
(** [contains a s] [s] *)
val radian_of_length: t -> float
(** [radians_of_length s] [s] *)
val degree_of_length: t -> float
(** [degrees_of_length s]  [s] *)
val bisect: t -> a
(** [bisect s] [s] *)

(** {6 Операции между секторами} *)
val includes: t -> t -> bool
(** [includes inner outer] [inner] [outer].
*)

val intersects: t -> t -> bool
(** [intersects s s'] [s] и [s']*)

val union: t -> t -> t
(** [union s s'] *)

val intersection: t -> t -> t
(** [intersection] *)
