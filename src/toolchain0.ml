(* 
  detect:
    let automatic_detection (): (sandbox, [Opam_esy_global_ocaml_not_found]) result =
      if opam is installed then 
        let current_switch = $ opam switch 
        if current_switch is local 
          return Opam current_swich 
        else 
          if esy is installed AND (there is esy.json OR _esy folder in the project root) then 
            return Esy sandbox
          else
            return current_switch
      else
        if esy is installed AND (there is esy.json OR _esy folder in the project root) then 
          return Esy sandbox 
        else 
          if global ocaml available then 
            global_switch
          else 
            Opam_esy_global_not_found
    in
    let detect_from_settings (): (sandbox, [No_such_setting | Invalid_settings]) result =
      if there is settings && settings contain "ocaml.sandbox" then 
        match (lint && parse ocaml.sandbox from settings) with 
        | Error _ -> Error Invalid_settings
        | Ok sandbox -> Ok sandbox
      else 
        No_such_setting
    in
    if there is settings file && settings file includes "ocaml.sandbox" then
      if "ocaml.sandbox" is parse-able and is valid then 
        return parsed sandbox 
      else 
        show a message that the given settings is invalid and going with automatic detection;
        automatic_detection () 
      
*)
open Import

(** for extension state element that needs to be persisted in workspace settings *)
module type PERSISTED_IN_WSPACE_SETTINGS = sig
  type t

  type parse_error =
    [ `Parse_error of
      [ `Missing_field of string | `Unexpected_key_value of string * string ]
    ]

  type settings_error =
    [ parse_error
    | `No_such_setting
    ]

  val from_settings : unit -> (t, settings_error) result

  (* val save_to_settings : unit -> unit Promise.t *)
end

module type SANDBOX = sig
  type t

  include PERSISTED_IN_WSPACE_SETTINGS with type t := t
end

module Esy = struct
  module Sandbox = struct
    type t

    let make _ = failwith "not implemented"

    let init () = failwith "not implemented"
  end
end

module Sandbox : SANDBOX = struct
  type t =
    | Global
    | Opam of Opam.Switch.t
    | Esy of Esy.Sandbox.t
    | Custom of string

  type kind =
    [ `Opam
    | `Esy
    | `Global
    | `Custom
    ]

  type parse_error =
    [ `Parse_error of
      [ `Missing_field of string | `Unexpected_key_value of string * string ]
    ]

  let parse_kind setting : (kind, parse_error) result =
    let open Jsonoo.Decode in
    let kind_field = "kind" in
    match field kind_field string setting with
    | exception _ -> Error (`Parse_error (`Missing_field kind_field))
    | "opam" -> Ok `Opam
    | "esy" -> Ok `Esy
    | "global" -> Ok `Global
    | "custom" -> Ok `Custom
    | other -> Error (`Parse_error (`Unexpected_key_value (kind_field, other)))

  let parse_sandbox setting (kind : kind) : (t, parse_error) result =
    let open Jsonoo.Decode in
    let field f =
      Result.try_with (fun () -> field f string setting)
      |> Result.map_error ~f:(fun _ -> `Parse_error (`Missing_field f))
    in
    let open Result.Monad_infix in
    match kind with
    | `Opam ->
      field "switch" >>| fun switch_name -> Opam (Opam.Switch.make ~switch_name)
    | `Esy -> field "root" >>| fun root -> Esy (Esy.Sandbox.make root)
    | `Global -> Ok Global
    | `Custom -> field "template" >>| fun template -> Custom template
    | _ -> assert false

  type settings_error =
    [ parse_error
    | `No_such_setting
    ]

  let from_settings () : (t, settings_error) result =
    let open Result.Monad_infix in
    let ocaml_config =
      Workspace.getConfiguration ~section:Global_consts.Settings.main ()
    in
    let open Result.Monad_infix in
    WorkspaceConfiguration.get ocaml_config
      ~section:Global_consts.Settings.sandbox ()
    |> Result.of_option ~error:(`No_such_setting :> settings_error)
    >>= fun sandbox_setting ->
    parse_kind sandbox_setting
    |> Result.map_error ~f:(fun e -> (e :> settings_error))
    >>= fun kind ->
    parse_sandbox sandbox_setting kind
    |> Result.map_error ~f:(fun e -> (e :> settings_error))
end

let is_opam_available : bool Lazy.t Promise.t = lazy (is_runnable Opam.bin)

let is_esy_available : bool Promise.t Lazy.t =
  lazy
    ( failwith "not impl" |> ignore;
      true )

let detect_sandbox_automatically () : (Sandbox.t, Error.t) result Promise.t =
  let (lazy is_opam_available) = is_opam_available in

  let detect_esy_sandbox () :
      (Esy.Sandbox.t, [ `Esy_not_installed | `No_sandbox_traces ]) result
      Promise.t =
    let (lazy is_esy_available) = is_esy_available in
    if is_esy_available then
      let open Promise.Syntax in
      Promise.List.find_map
        (fun file ->
          Promise.map (fun b -> Option.some_if b ()) (Fs.exists file))
        [ "esy.json"; "_esy" ]
      (* FIXME check works for directories *)
      >>| function
      | Some () ->
        (* if esy status used, one needs to remove _esy folder created afterwards *)
        Ok (Esy.Sandbox.init ())
      | None -> Error `No_sandbox_traces
    else
      Promise.return @@ Error `Esy_not_installed
  in

  if is_opam_available then
    let curr_switch =
      let switch_name = Cmd.(run (v "opam" % "switch" % "show")) in
      Opam.Switch.make ~switch_name
    in
    if Opam.Switch.is_local curr_switch then
      Ok (Opam curr_switch)
    else
      match detect_esy_sandbox () with
      | Ok sandbox -> Ok (Esy sandbox)
      | Error (`Esy_not_installed | `No_sandbox_traces) -> Ok (Opam curr_switch)
  else
    match detect_esy_sandbox () with
    | Ok sandbox -> Ok sandbox
    | Error (`Esy_not_installed | `No_sandbox_traces) ->
      if is_global_ocaml_available then
        Ok Global
      else
        Error `Opam_esy_global_not_available

let detect () : (Sandbox.t, Error.t) result Promise.t =
  Promise.return
  @@
  match Sandbox.from_settings () with
  | Ok sandbox ->
    (* FIXME
        try to remove settings; what are the conditions for
        settings removability?
    *)
    Ok sandbox
  | Error `No_such_setting -> failwith "not implemented"
  | Error (`Parse_error (`Missing_field _ | `Unexpected_key_value _)) ->
    message `Warn Global_consts.Settings.invalid_sandbox_message;
    failwith "not implemented"

let save_sandbox_info () : unit =
  (* save sandbox info on user changing the sandbox
     if can be opam switch linked - link
     otherwise likely need to save to settings
  *)
  failwith "not implemented"
