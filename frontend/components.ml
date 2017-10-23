module Widgets = Common.Components.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

[@@@ocaml.warning "-60"]

module Button = struct

  include Widgets.Button

end

module Checkbox = struct

  class type t = object

                 inherit Dom_html.element

                 method is_checked : unit -> bool Js.t Js.meth

  end

  let create ?classes ?style ?id ?input_id ?disabled ?js ?attrs () : t Js.t =
    let (obj : t Js.t) = Widgets.Checkbox.create ?classes ?style ?id ?input_id ?disabled ?js ?attrs ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    obj##is_checked := Js.wrap_callback (fun () -> Js.bool true); obj

end


