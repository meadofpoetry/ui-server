open Js_of_ocaml
open Components

let log (x : 'a) : unit =
  Js.Unsafe.global##.console##log x

let make_drawer () =
  let content = new Typography.Text.t ~text:"fake drawer" () in
  Drawer.make ~content:[content] ()

let make_side_sheet () =
  let content = new Typography.Text.t ~text:"fake side sheet" () in
  Side_sheet.make ~content:[content] ()

let make_top_app_bar () =
  let title = new Top_app_bar.Title.t (`Text "Demo page") () in
  let icon = Icon.SVG.(create_simple Path.tune) in
  let action = new Icon_button.t ~icon () in
  Top_app_bar.make ~title ~actions:[action] (), action

let onload _ =
  let root = Dom_html.getElementById "root" in
  let slider = Slider.make ~discrete:true ~markers:true ~step:5. () in
  let div = Widget.create_div ~widgets:[slider] () in
  div#add_class "slider-wrapper";
  let page = Scaffold.attach root in
  page#set_body div;
  slider#layout ();
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
