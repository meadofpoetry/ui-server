type t = Uint16.t

include (Uint16 : module type of Uint16 with type t := t)

let counter : t ref = ref zero

let next () : t =
  let id = !counter in
  counter := succ id;
  id
