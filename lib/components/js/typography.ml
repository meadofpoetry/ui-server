open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Typography
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( % ) f g x = f (g x)

let remove elt =
  List.iter (fun class' ->
      match BEM.get_block class' with
      | Some s when String.equal s CSS.root -> Element.remove_class elt class'
      | _ -> ())
  @@ Element.classes elt

let set ~font elt =
  remove elt;
  Element.add_class elt CSS.root;
  Element.add_class elt (font_to_class font)

module Text = struct
  class t (elt : Dom_html.element Js.t) () =
    object
      inherit Widget.t elt () as super

      method font : font option =
        let rec aux = function
          | [] -> None
          | hd :: tl -> (
            match BEM.split hd with
            | Ok (b, None, Some font) when String.equal b CSS.root -> (
              match font with
              | "headline1" -> Some Headline_1
              | "headline2" -> Some Headline_2
              | "headline3" -> Some Headline_3
              | "headline4" -> Some Headline_4
              | "headline5" -> Some Headline_5
              | "headline6" -> Some Headline_6
              | "subtitle1" -> Some Subtitle_1
              | "subtitle2" -> Some Subtitle_2
              | "body1" -> Some Body_1
              | "body2" -> Some Body_2
              | "button" -> Some Button
              | "caption" -> Some Caption
              | "overline" -> Some Overline
              | _ -> aux tl)
            | _ -> aux tl)
        in
        aux @@ Element.classes super#root

      method set_font (x : font) : unit =
        remove super#root;
        super#add_class @@ font_to_class x

      method text : string = Js.to_string @@ (Js.Unsafe.coerce super#root)##.innerText

      method set_text (s : string) : unit =
        super#root##.textContent := Js.some @@ Js.string s
    end

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

  let make ?classes ?a ?font ?text () =
    D.typography ?classes ?a ?font ?text () |> Tyxml_js.To_dom.of_element |> attach

  let make_r ?classes ?a ?font ?text () =
    R.typography ?classes ?a ?font ?text () |> Tyxml_js.To_dom.of_element |> attach
end
