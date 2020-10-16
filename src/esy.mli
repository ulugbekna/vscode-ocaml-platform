open Import

type t

val make : unit -> t option Promise.t

type discover =
  { file : Path.t
  ; status : (unit, string) result
  }

val discover : dir:Path.t -> discover list Promise.t

val exec : t -> manifest:Path.t -> args:string list -> Cmd.t

val setup_toolchain : t -> manifest:Path.t -> (unit, string) result Promise.t
