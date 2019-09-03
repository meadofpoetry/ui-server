open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Drawer
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Parent =
  Side_sheet.Make_parent(struct
      include CSS
      let name = "drawer"
      let slide = `Leading
    end)

class drawer (elt : Dom_html.element Js.t) () =
object
  inherit Parent.t elt ()

  method! private focus_active_navigation_item () : unit =
    "." ^ Components_tyxml.Item_list.CSS.item_activated
    |> Element.query_selector elt
    |> (fun i -> Option.iter (fun e -> e##focus) i)

end

include (Parent : module type of Parent with type t = drawer)

(** Creates new widget from scratch *)
let make (widgets : #Widget.t list) () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create (List.map Widget.to_markup widgets) () in
  new t elt ()

(** Attach widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
