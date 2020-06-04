open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Page_timedate_settings_tyxml.Timezone
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let name = "Timezone"

module Selector = struct
  let tz = "#" ^ tz_id
end

class t (elt : Dom_html.element Js.t) =
object (self)

  val tz_selector : string Select.t =
    let tz_elt = Element.query_selector_exn elt Selector.tz in
    Select.attach tz_elt

  inherit Widget.t elt () as super

  method value = "Europe/Moscow"

  method set_by_user = false

end

let make (zones : string list)
      (init : Pc_control_types.Timedate_config.t) : t =
  let open Pc_control_types.Timedate_config in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ D.create zones init
  in
  new t elt
