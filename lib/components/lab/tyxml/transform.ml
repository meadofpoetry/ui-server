open Components_tyxml

module CSS = struct
  let root = "transform"
  let resizer = BEM.add_element root "resizer"
  let circle = BEM.add_element root "circle"
end

type direction =
  | N
  | E
  | S
  | W
  | NW
  | NE
  | SE
  | SW

let direction_to_string = function
  | N -> "n"
  | E -> "e"
  | S -> "s"
  | W -> "w"
  | NW -> "nw"
  | NE -> "ne"
  | SE -> "se"
  | SW -> "sw"

let direction_of_string s =
  match String.lowercase_ascii s with
  | "n" -> Some N
  | "e" -> Some E
  | "s" -> Some S
  | "w" -> Some W
  | "nw" -> Some NW
  | "ne" -> Some NE
  | "se" -> Some SE
  | "sw" -> Some SW
  | _ -> None

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  let content_of_direction = function
    | N | E | S | W -> []
    | NW | NE | SW | SE -> [div ~a:[a_class [CSS.circle]] []]

  let create_resizer ?(classes = []) ?(attrs = []) direction : 'a elt =
    let classes = CSS.resizer :: classes in
    div ~a:([ a_class classes
            ; a_role ["slider"]
            ; a_user_data "direction" (direction_to_string direction)
            ] @ attrs)
      (content_of_direction direction)

  let create ?(tabindex = -1) ?(classes = []) ?(attrs = []) () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([ a_class classes
            ; a_tabindex tabindex
            ; a_role ["slider"]] @ attrs)
      [ create_resizer N
      ; create_resizer E
      ; create_resizer S
      ; create_resizer W
      ; create_resizer NW
      ; create_resizer NE
      ; create_resizer SW
      ; create_resizer SE
      ]
end
