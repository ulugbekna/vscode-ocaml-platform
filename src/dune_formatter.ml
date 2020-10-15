open Import

let get_formatter toolchain ~document ~options:_ ~token:_ =
  let endLine = TextDocument.lineCount document - 1 in
  let endCharacter =
    TextDocument.lineAt document ~line:endLine |> TextLine.text |> String.length
  in
  let select_entire_doc =
    Range.makeCoordinates ~startLine:0 ~startCharacter:0 ~endLine ~endCharacter
  in
  let entire_doc = TextDocument.getText document ~range:select_entire_doc () in
  let command = Toolchain.get_dune_command toolchain [ "format-dune-file" ] in
  let output =
    let open Promise.Result.Syntax in
    Cmd.check command >>= fun command -> Cmd.output ~stdin:entire_doc command
  in
  let open Promise.Syntax in
  `Promise
    (output >>| function
     | Ok newText -> Some [ TextEdit.replace ~range:select_entire_doc ~newText ]
     | Error msg ->
       message `Error "Dune formatting failed: %s" msg;
       Some [])

type t = Disposable.t list ref

let create () = ref []

let register t toolchain =
  t :=
    [ "dune"; "dune-project"; "dune-workspace" ]
    |> List.map ~f:(fun language ->
           let selector =
             `Filter (DocumentFilter.create ~scheme:"file" ~language ())
           in
           let provider =
             DocumentFormattingEditProvider.create
               ~provideDocumentFormattingEdits:(get_formatter toolchain)
           in
           Languages.registerDocumentFormattingEditProvider ~selector ~provider)

let dispose t =
  List.iter ~f:Disposable.dispose !t;
  t := []
