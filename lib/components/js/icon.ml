open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Icon
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

module SVG = struct
  module Markup_js = Markup_js.SVG
  module Path = Components_tyxml.Svg_icons

  (* paths variable is passed to avoid double allocation of paths objects *)
  class t (elt : Dom_html.element Js.t) () =
    object (self)
      inherit Widget.t elt () as super

      method paths_d : string list =
        List.map
          (fun (x : Dom_svg.pathElement Js.t) ->
            Js.Opt.map (x##getAttribute (Js.string "d")) Js.to_string
            |> fun x -> Js.Opt.get x (fun () -> ""))
          self#paths

      method paths : Dom_svg.pathElement Js.t list =
        List.filter_map (fun (x : Dom_html.element Js.t) ->
            match String.lowercase_ascii @@ Js.to_string x##.nodeName with
            | "path" -> Some (Js.Unsafe.coerce x)
            | _ -> None)
        @@ Element.children super#root
    end

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

  let make ?classes ?attrs ?size ?fill ?d ?children () : t =
    Markup_js.create ?classes ?attrs ?size ?fill ?d ?children ()
    |> Tyxml_js.To_dom.of_element
    |> attach
end
