type t = { lock : bool; plps : int list } [@@deriving yojson, eq]

let to_string (x : t) =
  Printf.sprintf "lock: %b, PLP IDs: [%s]" x.lock
    (String.concat ", " @@ List.map string_of_int x.plps)
