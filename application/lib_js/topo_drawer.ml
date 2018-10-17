open Containers
open Components

let base_class = "topology__drawer"

let make_header ~title () =
  let close = new Icon_button.t ~icon:Icon.SVG.(create_simple Path.close) () in
  let title = new Typography.Text.t ~adjust_margin:false ~font:Headline_5 ~text:title () in
  let box = new Hbox.t ~widgets:[title#widget;close#widget] () in
  let () = box#add_class @@ Markup.CSS.add_element base_class "header" in
  let () = title#add_class @@ Markup.CSS.add_element base_class "title" in
  let () = close#add_class @@ Markup.CSS.add_element base_class "close" in
  box, close, title#set_text

let make ?(anchor=`Right) ~title () =
  let header, close, set_title = make_header ~title () in
  let divider = new Divider.t () in
  let box = new Box.t
              ~widgets:[]
              ~direction:(match anchor with
                          | `Bottom | `Top   -> `Row
                          | `Left   | `Right -> `Column)
              () in
  let content = [header#widget;divider#widget;box#widget] in
  let drawer = new Drawer.t ~anchor ~content () in
  close#listen_lwt Widget.Event.click (fun _ _ ->
      drawer#hide ();
      Lwt.return_unit) |> Lwt.ignore_result;
  let () = drawer#add_class base_class in
  let () = box#add_class @@ Markup.CSS.add_element base_class "body" in
  drawer, box, set_title
