type t =
  | N
  | E
  | S
  | W
  | NW
  | NE
  | SE
  | SW

let to_string = function
  | N -> "n"
  | E -> "e"
  | S -> "s"
  | W -> "w"
  | NW -> "nw"
  | NE -> "ne"
  | SE -> "se"
  | SW -> "sw"

let of_string (s : string) : t option =
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
