open Js_of_ocaml
open Components
open Dashboard_common
open Dashboard_item

let drag_type = "application/dashboard-item"

class t (info : info) () =
  let data = info_to_yojson info |> Yojson.Safe.to_string |> Js.string in
  let typ = drag_type in
  let title = Typography.Text.make info.title in
  let description = Typography.Text.make info.description in
  let text_box = Box.make ~dir:`Column [title#widget;description#widget] in
  let thumbnail = match info.thumbnail with
    | `Icon icon ->
       Icon.Font.make icon
       |> (fun x ->
           x#add_class Dashboard_tyxml.CSS.Add_item.thumbnail_icon;
           x#widget)
  in
  let box = Box.make ~dir:`Row [thumbnail;text_box#widget] in
  let s, push = React.S.create false in
  object(self)
    val mutable _dragstart_listener = None
    val mutable _dragend_listener = None

    inherit Widget.t box#root () as super
    inherit Touch_draggable.t ~data ~typ box#root ()

    method! init () : unit =
      super#init ();
      (Events.dragstarts super#root (fun e _ ->
           push true;
           self#add_class Dashboard_tyxml.CSS.Add_item.dragging;
           e##.dataTransfer##setData (Js.string typ) data;
           Lwt.return_unit))
      |> (fun x -> _dragstart_listener <- Some x);
      (Events.dragends super#root (fun e _ ->
           push false;
           (* let drop_effect = e##.dataTransfer##.dropEffect |> Js.to_string in *)
           self#remove_class Dashboard_tyxml.CSS.Add_item.dragging;
           Dom.preventDefault e;
           Lwt.return_unit))
      |> (fun x -> _dragend_listener <- Some x);
      super#set_attribute "draggable" "true";
      title#add_class Dashboard_tyxml.CSS.Add_item.title;
      description#add_class Dashboard_tyxml.CSS.Add_item.description;
      text_box#add_class Dashboard_tyxml.CSS.Add_item.text_box;
      thumbnail#add_class Dashboard_tyxml.CSS.Add_item.thumbnail;
      super#add_class Dashboard_tyxml.CSS.Add_item.root

    method! destroy () : unit =
      super#destroy ();
      React.S.stop ~strong:true s;
      Utils.Option.iter Lwt.cancel _dragstart_listener;
      _dragstart_listener <- None;
      Utils.Option.iter Lwt.cancel _dragend_listener;
      _dragend_listener <- None

    method s_dragging = s

  end
