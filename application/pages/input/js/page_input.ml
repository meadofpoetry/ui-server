open Js_of_ocaml
open Application_types
open Components

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread = Ui_templates.Tabbed_page.init () in
  let _thread =
    try
      let input =
        Topology.topo_input_of_yojson
        @@ Yojson.Safe.from_string
        @@ Js.to_string Js.Unsafe.global##.input
      in
      match input with
      | Ok input ->
          Page_log.init ~input ();
          Lwt.return_ok ()
      | Error e -> Lwt.return_error (`Msg e)
    with e -> Lwt.return_error (`Msg (Printexc.to_string e))
  in
  let _loader =
    Components_lab.Loader.make_loader
      ~on_success:(fun _ x -> scaffold#set_on_destroy x#finalize)
      ~elt:scaffold#app_content_inner
      thread
  in
  ()
