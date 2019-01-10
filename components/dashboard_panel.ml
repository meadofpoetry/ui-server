open Containers
open Dashboard_common

class t ~(title : string) ~widgets () =
  let title = new Typography.Text.t ~adjust_margin:false ~text:title () in
  let content = title#widget :: List.map Widget.coerce widgets in
  object
    inherit Side_sheet.t  (`Content content) () as super

    method! init () : unit =
      super#init ();
      title#add_class Markup.Panel.title_class;
      super#add_class Markup.Panel._class
  end

class add ~(widgets : Dashboard_add_item.t list) () =
  let s = React.S.merge ~eq:Equal.bool
            (fun acc x -> if acc then acc else x) false
            (List.map (fun x -> x#s_dragging) widgets) in
  object
    val mutable _s_state = None
    inherit t ~title:"Добавить виджет" ~widgets () as super
    method! init () : unit =
      super#init ();
      (* timeout needed to prevent d&d cancellation *)
      let s' =
        React.S.map (fun x ->
            if x then ignore @@ Utils.set_timeout super#hide 0.) s in
      _s_state <- Some s'
  end
