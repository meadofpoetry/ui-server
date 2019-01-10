let () =
  print_endline "making home page";
  ignore @@ (new Ui_templates.Page.t (`Static []) () : Ui_templates.Page.t)
