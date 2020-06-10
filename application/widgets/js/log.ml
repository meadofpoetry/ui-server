open Js_of_ocaml
open Components
include Application_widgets_tyxml.Log
module D = Make (Ptime_clock) (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Ptime_clock) (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

module Selector = struct
  let has_more = Printf.sprintf ".%s" CSS.has_more
end

class t (elt : Dom_html.element Js.t) () =
  object
    val has_more : Button.t =
      Button.attach @@ Element.query_selector_exn elt Selector.has_more

    inherit Widget.t elt ()

    method has_more_button : Button.t = has_more

    method has_more : bool = has_more#disabled

    method set_has_more (x : bool) : unit = has_more#set_disabled (not x)
  end

let attach elt : t = new t (elt :> Dom_html.element Js.t) ()
