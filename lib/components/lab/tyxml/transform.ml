open Components_tyxml

module CSS = struct
  let root = "mdc-transform"

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

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( @:: ) = cons

  let content_of_direction = function
    | N | E | S | W -> nil ()
    | NW | NE | SW | SE ->
        singleton (return (div ~a:[a_class (return [CSS.circle])] (nil ())))

  let transform_resizer ?(classes = return []) ?(a = []) direction =
    let classes = fmap (fun x -> CSS.resizer :: x) classes in
    div
      ~a:
        (a_class classes
        :: a_role (return ["slider"])
        :: a_user_data "direction" (return (direction_to_string direction))
        :: a)
      (content_of_direction direction)

  let transform ?(tabindex = return (-1)) ?(classes = return []) ?(a = []) () : 'a elt =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    div
      ~a:(a_class classes :: a_tabindex tabindex :: a_role (return ["slider"]) :: a)
      (return (transform_resizer N)
      @:: return (transform_resizer E)
      @:: return (transform_resizer S)
      @:: return (transform_resizer W)
      @:: return (transform_resizer NW)
      @:: return (transform_resizer NE)
      @:: return (transform_resizer SW)
      @:: return (transform_resizer SE)
      @:: nil ())
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
