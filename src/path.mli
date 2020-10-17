(** Representation of path *)

type t

val equal : t -> t -> bool

val of_string : string -> t

val to_string : t -> string

val delimiter : char

val is_absolute : t -> bool

val compare : t -> t -> int

val dirname : t -> string

val extname : t -> string

val basename : t -> string

val join : t -> t -> t

val ( / ) : t -> string -> t

val relative : t -> string -> t

val relative_all : t -> string list -> t

(** [append t s] simply appends [t] and [s] (without any checks 
    or adding delimiters) *)
val append : t -> string -> t

val parent : t -> t option

val is_root : t -> bool
