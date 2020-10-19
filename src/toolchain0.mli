open Import

type t

val detect : unit -> (t, Error.t) result Promise.t
