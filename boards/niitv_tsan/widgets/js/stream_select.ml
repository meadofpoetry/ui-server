open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Board_niitv_tsan_widgets_tyxml.Stream_select
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

module Selector = struct
  let select = Printf.sprintf ".%s" CSS.select
end

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val select : Application_types.Stream.t Select.t =
      Select.attach @@ Element.query_selector_exn elt Selector.select

    inherit Widget.t elt () as super
  end
