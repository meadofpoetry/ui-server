open Containers
open Dashboard_common

let _class      = Markup.CSS.add_element base_class "panel"
let title_class = Markup.CSS.add_element _class "title"

class t ~(title:string) ~widgets () =
  let title  = new Typography.Text.t ~adjust_margin:false ~text:title () in
  object(self)
    inherit Drawer.t ~anchor:`Right ~content:(title#widget :: List.map Widget.coerce widgets) ()
    initializer
      title#add_class title_class;
      self#add_class _class
  end

class add ~(widgets:Dashboard_add_item.t list) () =
  let s = React.S.merge ~eq:Equal.bool (fun acc x -> if acc then acc else x) false
                        (List.map (fun x -> x#s_dragging) widgets) in
  object(self)
    inherit t ~title:"Добавить виджет" ~widgets ()
    val mutable _s_state = None
    initializer
      (* timeout needed to prevent d&d cancellation *)
      _s_state <- Some (React.S.map (fun x -> if x then Dom_html.setTimeout self#hide 0. |> ignore) s)
  end
