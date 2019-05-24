open Js_of_ocaml_lwt
open Components
open Dashboard_common

let ( >>= ) = Lwt.( >>= )

class t ~(title : string) ~widgets () =
  let title = Typography.Text.make title in
  let content = title#widget :: List.map Widget.coerce widgets in
  let elt = Side_sheet.make_element content in
  object
    inherit Side_sheet.t elt () as super


    method! init () : unit =
      super#init ();
      title#add_class Dashboard_tyxml.CSS.Panel.title;
      super#add_class Dashboard_tyxml.CSS.Panel.root
  end

class add ~(widgets : Dashboard_add_item.t list) () =
  let e =
    List.map (fun x -> x#s_dragging) widgets
    |> React.S.merge ~eq:(=) (||) false
    |> React.S.Bool.rise in
  object
    val mutable _e = None
    inherit t ~title:"Добавить виджет" ~widgets () as super

    method! init () : unit =
      super#init ();
      (* timeout needed to prevent d&d cancellation *)
      _e <- Some (React.E.map (fun () -> Lwt_js.yield () >>= super#hide) e)

    method! destroy () : unit =
      super#destroy ();
      Utils.Option.iter (React.E.stop ~strong:true) _e;
      _e <- None;
  end
