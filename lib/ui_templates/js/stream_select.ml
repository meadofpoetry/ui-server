open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Components
include Ui_templates_tyxml.Stream_select
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

let stream_select_validation =
  Select.
    {
      to_string = Yojson.Safe.to_string % Stream.to_yojson;
      of_string =
        (fun json ->
          try Stream.of_yojson @@ Yojson.Safe.from_string json
          with Yojson.Json_error s -> Error s);
    }

module Selector = struct
  let select = Printf.sprintf ".%s" CSS.select
end

class t ?on_change (elt : Dom_html.element Js.t) () =
  object (self)
    val select : Stream.t Select.t =
      Select.attach ?on_change ~validation:(Custom stream_select_validation)
      @@ Element.query_selector_exn elt Selector.select

    inherit Widget.t elt () as super

    method! layout () : unit = select#layout ()

    method select = select
  end

let attach ?on_change elt = new t ?on_change elt ()
