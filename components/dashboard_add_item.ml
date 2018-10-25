open Containers
open Dashboard_common
open Dashboard_item

let drag_type = "application/dashboard-item"

class t (info : info) () =
  let adjust_margin = false in
  let data = info_to_yojson info |> Yojson.Safe.to_string |> Js.string in
  let typ = drag_type in
  let title = new Typography.Text.t ~adjust_margin ~text:info.title () in
  let description = new Typography.Text.t ~adjust_margin ~text:info.description () in
  let text_box = new Vbox.t ~widgets:[title#widget;description#widget] () in
  let thumbnail = match info.thumbnail with
    | `Icon icon ->
       new Icon.Font.t ~icon ()
       |> fun x -> x#add_class Markup.Add_item.thumbnail_icon_class; x#widget
  in
  let box = new Hbox.t ~widgets:[thumbnail;text_box#widget] () in
  let s, push = React.S.create false in
  object(self)
    val mutable _dragstart_listener = None
    val mutable _dragend_listener = None

    inherit Widget.t box#root () as super
    inherit Touch_draggable.t ~data ~typ box#root ()

    method init () : unit =
      super#init ();
      self#listen Widget.Event.dragstart (fun _ e ->
          push true;
          self#add_class Markup.Add_item.dragging_class;
          e##.dataTransfer##setData (Js.string typ) data;
          true)
      |> (fun x -> _dragstart_listener <- Some x);
      self#listen Widget.Event.dragend (fun _ e ->
          push false;
          (* let drop_effect = e##.dataTransfer##.dropEffect |> Js.to_string in *)
          self#remove_class Markup.Add_item.dragging_class;
          false)
      |> (fun x -> _dragend_listener <- Some x);
      self#set_attribute "draggable" "true";
      title#add_class Markup.Add_item.title_class;
      description#add_class Markup.Add_item.description_class;
      text_box#add_class Markup.Add_item.text_box_class;
      thumbnail#add_class Markup.Add_item.thumbnail_class;
      self#add_class Markup.Add_item._class

    method destroy () : unit =
      super#destroy ();
      React.S.stop ~strong:true s;
      Option.iter Dom_events.stop_listen _dragstart_listener;
      _dragstart_listener <- None;
      Option.iter Dom_events.stop_listen _dragend_listener;
      _dragend_listener <- None

    method s_dragging = s

  end
