(** Interface for running system commands *)

open Import

type shell = private string

type spawn = private
  { bin : Path.t
  ; args : string list
  }

type t =
  | Shell of shell
  | Spawn of spawn

type stdout = string

type stderr = string

(** create [spawn]. Example:
  [bin "opam"] creates [{bin: "opam"; args: []}] *)
val bin : string -> spawn

(** append arguments to spawn *)
val ( % ) : spawn -> string -> spawn

val append : spawn -> string list -> spawn

(** alias to append *)
val ( %% ) : spawn -> string list -> spawn

(** tries to run a binary with empty arguments

    @return true if program is runnable *)
val is_runnable : spawn -> bool Promise.t

val check_spawn : spawn -> (spawn, stderr) result Promise.t

val check : t -> (t, stderr) result Promise.t

val log : ?result:ChildProcess.return -> t -> unit

val run : ?stdin:string -> t -> (stdout, stderr) result Promise.t
