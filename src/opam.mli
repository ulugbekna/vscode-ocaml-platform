module Switch : sig
  type t = private
    | Local of Path.t
    | Named of string

  val make : switch_name:string -> t

  val name : t -> string

  val is_local : t -> bool
end

type t

val bin : Cmd.spawn

val make : unit -> t option Promise.t

val switch_list : t -> Switch.t list Promise.t

val exec : t -> switch:Switch.t -> args:string list -> Cmd.t

val exists : t -> switch:Switch.t -> bool Promise.t
