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
  let page = new Ui_templates.Page.t (`Static []) () in
  (* let root = Dom_html.getElementById "root" in
   * let page = Ui_templates.Scaffold.attach root in
   * begin match page#drawer_elevation with
   * | None -> print_endline "no drawer found!!!"
   * | Some Full_height -> print_endline "drawer is full-height"
   * | Some Clipped -> print_endline "drawer is clipped"
   * end; *)
  let top_app_bar, action = make_top_app_bar () in
  let drawer = make_drawer () in
  let side_sheet = make_side_sheet () in
  let scaffold =
    Ui_templates.Scaffold.make
      ~top_app_bar
      ~drawer
      ~drawer_breakpoints:(Permanent, [])
      ~drawer_elevation:Full_height
      ~side_sheet
      ~side_sheet_elevation:Clipped
      ~side_sheet_breakpoints:(Dismissible, [])
      () in
  action#listen_click_lwt' (fun _ _ -> side_sheet#toggle_await ());
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
