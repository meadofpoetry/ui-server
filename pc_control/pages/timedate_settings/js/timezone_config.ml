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
  let changed, push = React.E.create () in
object (self)

  inherit Widget.t elt () as super

  val mutable _handlers = []

  val mutable _set_by_user = false

  val tz_selector : string Select.t =
    let tz_elt = Element.query_selector_exn elt Selector.tz in
    Select.attach tz_elt
      ~on_change:(fun _ -> push ())
  
  method! init () : unit =
      super#init ()

  method! initial_sync_with_dom () : unit =
    _handlers <-
      [
        React.E.map (fun () ->
            _set_by_user <- true)
          changed;
      ];
    super#initial_sync_with_dom ()

  method! destroy () : unit =
    tz_selector#destroy ();
    List.iter (React.E.stop ~strong:true) _handlers;
    super#destroy ()
  
  method value =
    tz_selector#value_as_string

  method set_value nv =
    tz_selector#set_value_as_string nv;
    _set_by_user <- false
  
  method set_by_user = _set_by_user

end

let make (zones : string list)
      (init : Pc_control_types.Timedate_config.t) : t =
  let open Pc_control_types.Timedate_config in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ D.create zones init
  in
  new t elt
