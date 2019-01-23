open Js_of_ocaml
open Application_js
open Components
open Lwt_result.Infix

let on_settings (side_sheet : #Side_sheet.Parent.t)
      (content : #Widget.t)
      (set_title : string -> unit) =
  fun ((widget : Widget.t), (name : string)) ->
  content#set_empty ();
  content#append_child widget;
  set_title name;
  Lwt.Infix.(
    side_sheet#toggle_await ()
    >|= (fun () -> if not side_sheet#is_open then widget#destroy ()))

let () =
  let side_sheet, side_sheet_content, set_side_sheet_title =
    Topo_drawer.make ~title:"" () in
  let on_settings =
    on_settings side_sheet side_sheet_content set_side_sheet_title in
  let body =
    Ui_templates.Loader.create_widget_loader
      (Requests.HTTP.get_topology ()
       >|= (fun init ->
         let page = Page_topology.create init in
         Lwt_react.(E.keep @@ E.map on_settings page#e_settings);
         page)
       |> Lwt_result.map_err Api_js.Requests.err_to_string) in
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  scaffold#set_side_sheet
    ~typ:Dismissible
    ~elevation:Full_height
    side_sheet;
  scaffold#set_body body
