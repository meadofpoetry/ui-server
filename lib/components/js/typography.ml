open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Typography
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

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
        Element.remove_children super#root;
        List.iter (Dom.appendChild super#root % Tyxml_js.To_dom.of_element)
        @@ Markup_js.create_inner s
    end

  let make ?font (text : string) : t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element @@ Markup_js.create ?font text
    in
    new t elt ()

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()
end
