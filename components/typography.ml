open Markup

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
  | Display_4    -> Typography.display4_class
  | Display_3    -> Typography.display3_class
  | Display_2    -> Typography.display2_class
  | Display_1    -> Typography.display1_class
  | Headline     -> Typography.headline_class
  | Title        -> Typography.title_class
  | Subheading_2 -> Typography.subheading2_class
  | Subheading_1 -> Typography.subheading1_class
  | Body_2       -> Typography.body2_class
  | Body_1       -> Typography.body1_class
  | Caption      -> Typography.caption_class
  | Button       -> Typography.button_class


let remove (elt:#Widget.widget) =
  CCList.iter (fun x -> if CCString.prefix ~pre:Typography.base_class x then elt#remove_class x)
              elt#classes

let set ?(adjust_margin=true) ~font (elt:#Widget.widget) =
  remove elt;
  elt#add_class Typography.base_class;
  elt#add_class @@ font_to_class font;
  if adjust_margin then elt#add_class Typography.adjust_margin_class
