module Settings = struct
  let main = "ocaml"

  let invalid_sandbox_message =
    "The settings for OCaml sandbox seem invalid. If you haven't modified the \
     settings manually and you see this eeror, please report it to \
     vscode-ocaml-platform GitHub repository available \
     [here](https://github.com/ocamlabs/vscode-ocaml-platform) with your \
     current workspace settings available at '.vscode/settings.json'"

  let sandbox = "sandbox"
end

module Commands = struct end
