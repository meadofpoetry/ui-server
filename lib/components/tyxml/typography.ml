module CSS = struct
  (** Sets the font to Roboto. *)
  let root = "mdc-typography"

  (** Sets font properties as Headline 1 *)
  let headline1 = BEM.add_modifier root "headline1"

  (** Sets font properties as Headline 2 *)
  let headline2 = BEM.add_modifier root "headline2"

  (** Sets font properties as Headline 3 *)
  let headline3 = BEM.add_modifier root "headline3"

  (** Sets font properties as Headline 4 *)
  let headline4 = BEM.add_modifier root "headline4"

  (** Sets font properties as Headline 5 *)
  let headline5 = BEM.add_modifier root "headline5"

  (** Sets font properties as Headline 6 *)
  let headline6 = BEM.add_modifier root "headline6"

  (** Sets font properties as Subtitle 1 *)
  let subtitle1 = BEM.add_modifier root "subtitle1"

  (** Sets font properties as Subtitle 2 *)
  let subtitle2 = BEM.add_modifier root "subtitle2"

  (** Sets font properties as Body 1 *)
  let body1 = BEM.add_modifier root "body1"

  (** Sets font properties as Body 2 *)
  let body2 = BEM.add_modifier root "body2"

  (** Sets font properties as Button *)
  let button = BEM.add_modifier root "button"

  (** Sets font properties as Caption *)
  let caption = BEM.add_modifier root "caption"

  (** Sets font properties as Overline *)
  let overline = BEM.add_modifier root "overline"
end

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

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let typography
      ?(classes = return [])
      ?(a = [])
      ?(font : font option)
      ?text
      ?(children = nil ())
      () =
    let font_class =
      match font with
      | None -> None
      | Some x -> Some (font_to_class x)
    in
    let classes = fmap (Utils.cons_option font_class % List.cons CSS.root) classes in
    span
      ~a:(a_class classes :: a)
      (match text with
      | None -> children
      | Some text -> cons (return (txt text)) children)
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
