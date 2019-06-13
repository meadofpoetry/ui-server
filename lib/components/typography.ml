open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Typography

type font =
  | Headline_1
  | Headline_2
  | Headline_3
  | Headline_4
  | Headline_5
  | Headline_6
  | Subtitle_1
  | Subtitle_2
  | Body_1
  | Body_2
  | Button
  | Caption
  | Overline

let font_to_class : font -> string = function
  | Headline_1 -> CSS.headline1
  | Headline_2 -> CSS.headline2
  | Headline_3 -> CSS.headline3
  | Headline_4 -> CSS.headline4
  | Headline_5 -> CSS.headline5
  | Headline_6 -> CSS.headline6
  | Subtitle_1 -> CSS.subtitle1
  | Subtitle_2 -> CSS.subtitle2
  | Body_1 -> CSS.body1
  | Body_2 -> CSS.body2
  | Button -> CSS.button
  | Caption -> CSS.caption
  | Overline -> CSS.overline

let remove (w : #Widget.t) : unit =
  List.iter (fun x -> if String.prefix ~pre:CSS.root x then w#remove_class x)
  @@ Element.classes w#root

let set ~font (w : #Widget.t) =
  remove w;
  w#add_class CSS.root;
  w#add_class @@ font_to_class font

module Text = struct
  class t (elt : Dom_html.element Js.t) () =
  object(self)

    inherit Widget.t elt () as super

    method font : font option =
      let rec aux = function
        | [] -> None
        | hd :: tl ->
           match String.chop_prefix ~pre:(CSS.root ^ "--") hd with
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
           | None | Some _ -> aux tl in
      aux @@ Element.classes super#root

    method set_font (x : font) : unit =
      List.iter (function
          | s when String.prefix ~pre:(CSS.root ^ "--") s -> super#remove_class s
          | _ -> ()) @@ Element.classes super#root;
      super#add_class @@ font_to_class x

    method text : string =
      Js.to_string @@ (Js.Unsafe.coerce super#root)##.innerText

    method set_text (s : string) : unit =
      super#root##.innerHTML := Js.string (self#to_inner_html s)

    (* Private methods *)

    method private to_inner_html (text : string) : string =
      let open Tyxml.Html in
      let inner =
        let rec aux acc = function
          | [] -> List.rev acc
          | [x] -> List.rev ((txt x) :: acc)
          | x :: tl -> aux (br () :: txt x :: acc) tl in
        aux [] (String.split_on_char '\n' text) in
      String.concat "" @@ List.map (Format.asprintf "%a" (pp_elt ())) inner

  end

  let make_element ?font text =
    let font_class = match font with
      | None -> None
      | Some x -> Some (font_to_class x) in
    let classes = List.cons_maybe font_class [CSS.root] in
    Tyxml_js.To_dom.of_span
    @@ Tyxml_js.Html.(span ~a:[a_class classes] [txt text])

  let make ?font (text : string) : t =
    new t (make_element ?font text) ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()

end
