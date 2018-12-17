open Pipeline_js

let () =
  try
    print_endline "constructing page";
    let w = Page_editor.page () in
    ignore @@ new Ui_templates.Page.t (`Static [ w ]) ()
  with e ->
    Printexc.to_string e
    |> Printf.printf "the error is: %s\n"

