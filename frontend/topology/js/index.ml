open Lwt_result.Infix
open Js_of_ocaml
open Application_js
open Components

let on_settings (side_sheet : #Side_sheet.Parent.t)
      (content : #Widget.t)
      (set_title : string -> unit) =
  fun (prev : Widget.t option)
      ((widget : Widget.t), (name : string)) ->
  begin match prev with
  | None -> ()
  | Some w -> w#destroy ()
  end;
  content#set_empty ();
  content#append_child widget;
  set_title name;
  Lwt.Infix.(
    side_sheet#show_await ()
    >|= (fun () -> Some widget))

let () =
  let side_sheet, side_sheet_content, set_side_sheet_title =
    Topo_drawer.make ~title:"" () in
  let on_settings =
    on_settings side_sheet side_sheet_content set_side_sheet_title in
  let body =
    Ui_templates.Loader.create_widget_loader
      (Requests.HTTP.get_topology ()
       >|= (fun init ->
         let page = Topo.create init in
         Lwt_react.(E.keep @@ E.fold_s on_settings None page#e_settings);
         page)
       |> Lwt_result.map_err Api_js.Requests.err_to_string) in
  body#add_class Layout_grid.Markup.base_class;
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  scaffold#set_side_sheet ~elevation:Full_height side_sheet;
  scaffold#set_body body
