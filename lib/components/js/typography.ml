open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Typography
module Markup = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( % ) f g x = f (g x)

let remove (w : #Widget.t) : unit =
  List.iter (fun x -> if Utils.String.prefix ~pre:CSS.root x then w#remove_class x)
  @@ Element.classes w#root

let set ~font (w : #Widget.t) =
  remove w;
  w#add_class CSS.root;
  w#add_class @@ font_to_class font

module Text = struct
  class t (elt : Dom_html.element Js.t) () =
    object
      inherit Widget.t elt () as super

      method font : font option =
        let rec aux = function
          | [] -> None
          | hd :: tl -> (
            match Utils.String.chop_prefix ~pre:(CSS.root ^ "--") hd with
            | Some "headline1" -> Some Headline_1
            | Some "headline2" -> Some Headline_2
            | Some "headline3" -> Some Headline_3
            | Some "headline4" -> Some Headline_4
            | Some "headline5" -> Some Headline_5
            | Some "headline6" -> Some Headline_6
            | Some "subtitle1" -> Some Subtitle_1
            | Some "subtitle2" -> Some Subtitle_2
            | Some "body1" -> Some Body_1
            | Some "body2" -> Some Body_2
            | Some "button" -> Some Button
            | Some "caption" -> Some Caption
            | Some "overline" -> Some Overline
            | None | Some _ -> aux tl)
        in
        aux @@ Element.classes super#root

      method set_font (x : font) : unit =
        List.iter (function
            | s when Utils.String.prefix ~pre:(CSS.root ^ "--") s -> super#remove_class s
            | _ -> ())
        @@ Element.classes super#root;
        super#add_class @@ font_to_class x

      method text : string = Js.to_string @@ (Js.Unsafe.coerce super#root)##.innerText

      method set_text (s : string) : unit =
        Element.remove_children super#root;
        List.iter (Dom.appendChild super#root % Tyxml_js.To_dom.of_element)
        @@ Markup.make_inner s
    end

  let make ?font (text : string) : t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element @@ Markup.make ?font text
    in
    new t elt ()

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()
end
