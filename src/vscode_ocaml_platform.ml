open Import

let client_options () : LanguageClient.ClientOptions.t =
  let documentSelector =
    LanguageClient.DocumentSelector.
      [| language "ocaml"
       ; language "ocaml.interface"
       ; language "ocaml.ocamllex"
       ; language "ocaml.menhir"
       ; language "reason"
      |]
  in
  let (lazy outputChannel) = Output.language_server_output_channel in
  let revealOutputChannelOn = LanguageClient.RevealOutputChannelOn.Never in
  LanguageClient.ClientOptions.create ~documentSelector ~outputChannel
    ~revealOutputChannelOn ()

let server_options (toolchain : Toolchain.resources) :
    LanguageClient.ServerOptions.t =
  let command = Toolchain.get_lsp_command toolchain in
  Cmd.log command;
  match command with
  | Shell command ->
    let options = LanguageClient.ExecutableOptions.create ~shell:true () in
    LanguageClient.Executable.create ~command ~options ()
  | Spawn { bin; args } ->
    let command = Path.to_string bin in
    let options = LanguageClient.ExecutableOptions.create ~shell:false () in
    LanguageClient.Executable.create ~command ~args ~options ()

let select_sandbox_command_id = "ocaml.select-sandbox"

let restart_command_id = "ocaml.server.restart"

let open_terminal_command_id = "ocaml.open-terminal"

let open_terminal_select_command_id = "ocaml.open-terminal-select"

let switch_impl_intf_command_id = "ocaml.switch-impl-intf"

module Instance = struct
  type t =
    { mutable toolchain : Toolchain.resources option
    ; mutable client : LanguageClient.t option
    ; mutable ocaml_lsp_capabilities : Ocaml_lsp.t option
    ; mutable status_bar_item : StatusBarItem.t option
    ; dune_formatter : Dune_formatter.t
    ; dune_task_provider : Dune_task_provider.t
    }

  let create () =
    { toolchain = None
    ; client = None
    ; ocaml_lsp_capabilities = None
    ; status_bar_item = None
    ; dune_formatter = Dune_formatter.create ()
    ; dune_task_provider = Dune_task_provider.create ()
    }

  let stop t =
    Dune_formatter.dispose t.dune_formatter;
    Dune_task_provider.dispose t.dune_task_provider;

    Option.iter t.status_bar_item ~f:(fun status_bar_item ->
        StatusBarItem.dispose status_bar_item;
        t.status_bar_item <- None);

    Option.iter t.client ~f:(fun client ->
        LanguageClient.stop client;
        t.client <- None);

    t.ocaml_lsp_capabilities <- None;
    t.toolchain <- None

  let start t toolchain =
    t.toolchain <- Some toolchain;

    Dune_formatter.register t.dune_formatter toolchain;
    Dune_task_provider.register t.dune_task_provider toolchain;

    let status_bar_item =
      Window.createStatusBarItem ~alignment:StatusBarAlignment.Left ()
    in
    let package_manager = Toolchain.package_manager toolchain in
    let status_bar_text =
      let package_icon =
        "$(package)"
        (* see https://code.visualstudio.com/api/references/icons-in-labels *)
      in
      Printf.sprintf "%s %s" package_icon
      @@ Toolchain.Package_manager.to_pretty_string package_manager
    in
    StatusBarItem.set_text status_bar_item status_bar_text;
    StatusBarItem.set_command status_bar_item
      (`String select_sandbox_command_id);
    StatusBarItem.show status_bar_item;
    t.status_bar_item <- Some status_bar_item;

    let open Promise.Result.Syntax in
    Toolchain.run_setup toolchain >>= fun () ->
    let serverOptions = server_options toolchain in
    let clientOptions = client_options () in
    let client =
      LanguageClient.make ~id:"ocaml" ~name:"OCaml Language Server"
        ~serverOptions ~clientOptions ()
    in
    t.client <- Some client;
    LanguageClient.start client;

    let open Promise.Syntax in
    LanguageClient.readyInitializeResult client >>| fun initialize_result ->
    let ocaml_lsp = Ocaml_lsp.of_initialize_result initialize_result in
    t.ocaml_lsp_capabilities <- Some ocaml_lsp;
    if
      (not (Ocaml_lsp.has_interface_specific_lang_id ocaml_lsp))
      || not (Ocaml_lsp.can_handle_switch_impl_intf ocaml_lsp)
    then
      message `Warn
        "The installed version of ocamllsp is out of date. Some features may \
         be unavailable or degraded in functionality: switching between \
         implementation and interface files, functionality in mli sources. \
         Consider updating ocamllsp.";
    Ok ()

  let open_terminal toolchain =
    let open Option.Monad_infix in
    match toolchain |> Terminal_sandbox.create >>| Terminal_sandbox.show with
    | Some _ -> ()
    | None ->
      message `Error
        "Could not open a terminal in the current sandbox. The toolchain may \
         not have loaded yet."

  let disposable t = Disposable.make ~dispose:(fun () -> stop t)
end

let select_sandbox (instance : Instance.t) () =
  let set_toolchain =
    let open Promise.Syntax in
    Toolchain.select_and_save () >>= function
    | None -> Promise.Result.return ()
    | Some t ->
      Instance.stop instance;
      let t = Toolchain.make_resources t in
      Instance.start instance t
  in
  let (_ : unit Promise.t) =
    Promise.Result.iter set_toolchain ~error:(message `Error "%s")
  in
  ()

let restart_instance (instance : Instance.t) () =
  let (_ : unit Promise.t) =
    let open Promise.Syntax in
    Toolchain.of_settings () >>= function
    | None ->
      select_sandbox instance ();
      Promise.return ()
    | Some toolchain ->
      Instance.stop instance;
      Instance.start instance (Toolchain.make_resources toolchain)
      |> Promise.Result.iter ~error:(message `Error "%s")
  in
  ()

let select_sandbox_and_open_terminal () =
  let (_ : unit Promise.t) =
    Toolchain.select ()
    |> Promise.Option.iter (fun toolchain ->
           let toolchain = Toolchain.make_resources toolchain in
           Instance.open_terminal toolchain)
  in
  ()

let open_terminal (instance : Instance.t) () =
  match instance.toolchain with
  | None -> select_sandbox_and_open_terminal ()
  | Some toolchain -> Instance.open_terminal toolchain

let switch_impl_intf (instance : Instance.t) () =
  let try_switching () =
    let open Option.Monad_infix in
    Window.activeTextEditor () >>| TextEditor.document >>= fun document ->
    instance.client >>= fun client ->
    (* extension needs to be activated; otherwise, just ignore the switch try *)
    instance.ocaml_lsp_capabilities >>| fun ocaml_lsp ->
    (* same as for instance.client; ignore the try if it's None *)
    if Ocaml_lsp.can_handle_switch_impl_intf ocaml_lsp then
      Switch_impl_intf.request_switch client document
    else
      (* if, however, ocamllsp doesn't have the capability, recommend updating ocamllsp*)
      Promise.return
      @@ message `Warn
           "The installed version of ocamllsp does not support switching \
            between implementation and interface files. Consider updating \
            ocamllsp."
  in
  let (_ : unit Promise.t option) = try_switching () in
  ()

let suggest_to_setup_toolchain instance =
  let open Promise.Syntax in
  Window.showInformationMessage
    ~message:
      "Extension is unable to find ocamllsp automatically. Please select \
       package manager you used to install ocamllsp for this project."
    ~choices:[ ("Select package manager", ()) ]
    ()
  >>| function
  | None -> ()
  | Some () -> select_sandbox instance ()

let register_commands extension =
  List.iter ~f:(function command, callback ->
      let callback ~args:_ = callback () in
      ExtensionContext.subscribe extension
        ~disposable:(Commands.registerCommand ~command ~callback))

let activate (extension : ExtensionContext.t) =
  Process.Env.set "OCAML_LSP_SERVER_LOG" "-";
  let instance = Instance.create () in
  register_commands extension
    [ (select_sandbox_command_id, select_sandbox instance)
    ; (restart_command_id, restart_instance instance)
    ; (open_terminal_command_id, open_terminal instance)
    ; (open_terminal_select_command_id, select_sandbox_and_open_terminal)
    ; (switch_impl_intf_command_id, switch_impl_intf instance)
    ];
  ExtensionContext.subscribe extension
    ~disposable:(Instance.disposable instance);
  let open Promise.Syntax in
  let toolchain =
    Toolchain.of_settings () >>| fun pm ->
    let resources, is_fallback =
      match pm with
      | Some toolchain -> (toolchain, false)
      | None ->
        let (_ : unit Promise.t) = suggest_to_setup_toolchain instance in
        (Toolchain.Package_manager.Global, true)
    in
    (Toolchain.make_resources resources, is_fallback)
  in
  toolchain >>= fun (toolchain, is_fallback) ->
  Instance.start instance toolchain
  |> Promise.Result.iter ~error:(fun e ->
         if not is_fallback then message `Error "%s" e)
  |> Promise.catch ~rejected:(fun e ->
         let error_message = Node.JsError.message e in
         message `Error "Error: %s" error_message;
         Promise.return ())

let () =
  let open Js_of_ocaml.Js in
  export "activate" (wrap_callback activate)
