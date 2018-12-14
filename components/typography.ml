open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Typography.Make(Xml)(Svg)(Html)

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

let font_to_class = function
  | Headline_1 -> Markup.headline1_class
  | Headline_2 -> Markup.headline2_class
  | Headline_3 -> Markup.headline3_class
  | Headline_4 -> Markup.headline4_class
  | Headline_5 -> Markup.headline5_class
  | Headline_6 -> Markup.headline6_class
  | Subtitle_1 -> Markup.subtitle1_class
  | Subtitle_2 -> Markup.subtitle2_class
  | Body_1 -> Markup.body1_class
  | Body_2 -> Markup.body2_class
  | Button -> Markup.button_class
  | Caption -> Markup.caption_class
  | Overline -> Markup.overline_class

let remove (elt : #Widget.t) =
  List.iter (fun x -> if String.prefix ~pre:Markup.base_class x
                      then elt#remove_class x)
    elt#classes

let set ?(adjust_margin = true) ~font (elt : #Widget.t) =
  remove elt;
  elt#add_class Markup.base_class;
  elt#add_class @@ font_to_class font;
  if adjust_margin then elt#add_class Markup.adjust_margin_class

module Text = struct

  class t ?(split = false) ?(adjust_margin = true) ?font ~text () =
    object(self)

      inherit Widget.t Dom_html.(createSpan document) () as super

      val mutable _text : string = text
      val mutable _font : font option = font

      method! init () : unit =
        super#init ();
        self#set_text text;
        self#add_class Markup.base_class;
        self#set_adjust_margin adjust_margin;
        Option.iter Fun.(self#add_class % font_to_class) font

      method font : font option = _font
      method set_font (x : font) : unit =
        Option.iter (fun x -> self#remove_class @@ font_to_class x) font;
        self#add_class @@ font_to_class x;
        _font <- Some x

      method adjust_margin = self#has_class Markup.adjust_margin_class
      method set_adjust_margin = function
        | true  -> self#add_class Markup.adjust_margin_class
        | false -> self#remove_class Markup.adjust_margin_class

      method text = _text
      method set_text s =
        _text <- s;
        self#set_inner_html (self#_to_inner_html s)

      (* Private methods *)

      method _to_inner_html text =
        let open Tyxml.Html in
        let inner =
          if split
          then List.(map (fun s -> [txt s; br ()]) (String.lines text)
                     |> flatten |> rev |> drop 1 |> rev)
          else [txt text] in
        String.concat "" @@ List.map (Format.asprintf "%a" (pp_elt ())) inner

    end

end
