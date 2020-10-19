open Import

type 'a t =
  { key : string
  ; to_json : 'a -> Jsonoo.t
  ; of_json : Jsonoo.t -> 'a
  ; scope : ConfigurationTarget.t
  }

let create ~scope ~key ~of_json ~to_json = { scope; key; to_json; of_json }

let get ?section t =
  let section = Workspace.getConfiguration ?section () in
  match WorkspaceConfiguration.get section ~section:t.key () with
  | None -> Error `No_such_setting
  | Some v -> (
    match t.of_json v with
    | s -> Ok s
    | exception Jsonoo.Decode_error msg -> Error (`Parse_error msg) )

let set ?section t v =
  let section = Workspace.getConfiguration ?section () in
  match Workspace.name () with
  | None -> Promise.return ()
  | Some _ ->
    WorkspaceConfiguration.update section ~section:t.key ~value:(t.to_json v)
      ~configurationTarget:(`ConfigurationTarget t.scope) ()

let string =
  let to_json = Jsonoo.Encode.string in
  let of_json = Jsonoo.Decode.string in
  create ~of_json ~to_json
