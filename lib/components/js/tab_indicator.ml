open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Tab_indicator
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t (elt : Dom_html.element Js.t) () = object(self)

  val content = Utils.find_element_by_class_exn elt CSS.content

  inherit Widget.t elt () as super

  method fade : bool =
    super#has_class CSS.fade

  method set_fade (x : bool) : unit =
    super#toggle_class ~force:x CSS.fade

  method active : bool =
    super#has_class CSS.active

  method set_active ?(previous : 'self option) (x : bool) : unit =
    if x
    then (if self#fade
          then super#add_class CSS.active
          else self#activate_slide previous)
    else super#remove_class CSS.active

  (* Private methods *)

  method private activate_slide : t option -> unit = function
    | None -> super#add_class CSS.active
    | Some prev ->
      let old_content = Utils.find_element_by_class_exn prev#root CSS.content in
      let old = old_content##getBoundingClientRect in
      let cur = content##getBoundingClientRect in
      (* Calculate the dimensions based on the dimensions of the previous
         indicator *)
      let width_delta =
        match Js.Optdef.to_option old##.width,
              Js.Optdef.to_option old##.width with
        | Some pw, Some cw -> pw /. cw
        | _ -> failwith "mdc_tab_indicator: width property not found" in
      let x_position  = old##.left -. cur##.left in
      super#add_class CSS.no_transition;
      let s = Printf.sprintf "translateX(%gpx) scaleX(%g)" x_position width_delta in
      content##.style##.transform := Js.string s;
      (* Force repaint before updating classes and transform to ensure
         the transform properly takes effect *)
      content##getBoundingClientRect |> ignore;
      super#remove_class CSS.no_transition;
      super#add_class CSS.active;
      content##.style##.transform := Js.string ""
end

let make ?fade ?active ?icon () : t =
  let icon = Option.map Widget.to_markup icon in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?active ?fade (Markup.create_content ?icon ()) () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
