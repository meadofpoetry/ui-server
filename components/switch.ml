open Containers
open Tyxml_js

module Markup = Components_markup.Switch.Make(Xml)(Svg)(Html)

class t ?state ?input_id () =
  let elt = Markup.create ?input_id () |> Tyxml_js.To_dom.of_div in
  let input_elt = elt##querySelector (Js.string ("." ^ Markup.native_control_class))
                  |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in
  object
    inherit Widget.radio_or_cb_widget ?state ~input_elt elt ()
  end
