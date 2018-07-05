open Containers
open Tyxml_js

module Markup = Components_markup.Typography.Make(Xml)(Svg)(Html)

type font = Display_4
          | Display_3
          | Display_2
          | Display_1
          | Headline
          | Title
          | Subheading_2
          | Subheading_1
          | Body_2
          | Body_1
          | Caption
          | Button

let font_to_class = function
  | Display_4    -> Markup.display4_class
  | Display_3    -> Markup.display3_class
  | Display_2    -> Markup.display2_class
  | Display_1    -> Markup.display1_class
  | Headline     -> Markup.headline_class
  | Title        -> Markup.title_class
  | Subheading_2 -> Markup.subheading2_class
  | Subheading_1 -> Markup.subheading1_class
  | Body_2       -> Markup.body2_class
  | Body_1       -> Markup.body1_class
  | Caption      -> Markup.caption_class
  | Button       -> Markup.button_class

let remove (elt:#Widget.t) =
  List.iter (fun x -> if String.prefix ~pre:Markup.base_class x then elt#remove_class x)
            elt#classes

let set ?(adjust_margin=true) ~font (elt:#Widget.t) =
  remove elt;
  elt#add_class Markup.base_class;
  elt#add_class @@ font_to_class font;
  if adjust_margin then elt#add_class Markup.adjust_margin_class

module Text = struct

  class t ?(split=false) ?(adjust_margin=true) ?font ~text () =

    let elt =
      let text = if split
                 then List.flatten @@ List.map (fun s -> Tyxml_js.Html.([pcdata s; br ()])) @@ String.lines text
                 else Tyxml_js.Html.([ pcdata text ])in
      Tyxml_js.Html.(span text) |> Tyxml_js.To_dom.of_element in

    object(self)

      inherit Widget.t elt ()

      val mutable font : font option = font

      method font       = font
      method set_font x =
        Option.iter (fun x -> self#remove_class @@ font_to_class x) font;
        self#add_class @@ font_to_class x;
        font <- Some x

      method adjust_margin     = self#has_class Markup.adjust_margin_class
      method set_adjust_margin = function
        | true  -> self#add_class Markup.adjust_margin_class
        | false -> self#remove_class Markup.adjust_margin_class

      method text       = self#text_content |> Option.get_or ~default:""
      method set_text s = self#set_text_content s

      initializer
        self#add_class Markup.base_class;
        self#set_adjust_margin adjust_margin;
        Option.iter (fun x -> self#add_class @@ font_to_class x) font

    end

end
