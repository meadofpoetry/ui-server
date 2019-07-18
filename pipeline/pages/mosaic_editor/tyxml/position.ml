type t = Pipeline_types.Wm.position

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

let direction_of_string (s : string) : direction option =
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

let (empty : t) =
  { x = 0.0
  ; y = 0.0
  ; w = 0.0
  ; h = 0.0
  }

let compare (a : t) (b : t) =
  let c = compare a.x b.x in
  if c <> 0 then c
  else (let c = compare a.y b.y in
        if c <> 0 then c
        else (let c = compare a.w b.w in
              if c <> 0 then c
              else compare a.h b.h))

let equal (a : t) (b : t) =
  compare a b = 0

let show ({ x; y; w; h } : t)=
  Printf.sprintf "x=%g, y=%g, w=%g, h=%g" x y w h

let to_normalized ~(parent_size : float * float) (pos : t) : t =
  let w, h =
    pos.w /. (fst parent_size),
    pos.h /. (snd parent_size) in
  let x = (pos.x *. w) /. pos.w in
  let y = (pos.y *. h) /. pos.h in
  { x; y; w; h }

let to_absolute ~(parent_size : float * float) (pos : t) : t =
  let w = pos.w *. (fst parent_size) /. 100. in
  let h = pos.h *. (snd parent_size) /. 100. in
  let x = pos.x *. w /. pos.w in
  let y = pos.y *. h /. pos.h in
  { x; y; w; h }
