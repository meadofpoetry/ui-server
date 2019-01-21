open Js_of_ocaml
open Components

let log (x : 'a) : unit =
  Js.Unsafe.global##.console##log x

let make_drawer () =
  let content = new Typography.Text.t ~text:"fake drawer" () in
  Drawer.make ~content:[content] ()

let make_top_app_bar () =
  let title = new Top_app_bar.Title.t (`Text "Demo page") () in
  Top_app_bar.make ~title ()

let onload _ =
  let page = new Ui_templates.Page.t (`Static []) () in
  let top_app_bar = make_top_app_bar () in
  let drawer = make_drawer () in
  let scaffold =
    Ui_templates.Scaffold.make
      ~top_app_bar
      ~drawer
      ~drawer_breakpoints:(Permanent, [])
      () in
  let div = Widget.create_div ~widgets:[scaffold] () in
  div#add_class "demo-frame";
  page#arbitrary#append_child div;
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
