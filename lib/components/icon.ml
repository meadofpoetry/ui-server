open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Icon
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Font = struct
  module Markup = Markup.Font

  class t (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t elt () as super

    method icon : string =
      Js.Opt.map super#root##.textContent Js.to_string
      |> fun x -> Js.Opt.get x (fun () -> "")

    method set_icon (i : string) : unit =
      super#root##.textContent := Js.some (Js.string i)
  end

  let make (icon : string) : t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element
      @@ Markup.create ~icon () in
    new t elt ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()
end

module SVG = struct
  module Markup = Markup.SVG

  module Path = Components_tyxml.Svg_icons

  (* paths variable is passed to avoid double allocation of paths objects *)
  class t (elt : Dom_html.element Js.t) () =
  object(self)
    inherit Widget.t elt () as super

    method paths_d : string list =
      List.map (fun (x : Dom_svg.pathElement Js.t) ->
          Js.Opt.map (x##getAttribute (Js.string "d")) Js.to_string
          |> fun x -> Js.Opt.get x (fun () -> "")) self#paths

    method paths : Dom_svg.pathElement Js.t list =
      List.filter_map (fun (x : Dom_html.element Js.t) ->
          match String.lowercase_ascii @@ Js.to_string x##.nodeName with
          | "path" -> Some (Js.Unsafe.coerce x)
          | _ -> None)
      @@ Element.children super#root
  end

  let make_path ?fill d =
    Markup.create_path ?fill d ()

  let make ?size paths () : t =
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Markup.create ?size paths () in
    new t elt ()

  let make_simple ?fill ?size (d : string) : t =
    make ?size [make_path ?fill d] ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()
end
