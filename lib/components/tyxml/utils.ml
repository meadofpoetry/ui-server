let cons_if (case : bool) (x : 'a) (l : 'a list) : 'a list = if case then x :: l else l

let cons_if_lazy (case : bool) (f : unit -> 'a) (l : 'a list) : 'a list =
  if case then f () :: l else l

let cons_option (x : 'a option) (l : 'a list) : 'a list =
  match x with
  | None -> l
  | Some x -> x :: l

let map_cons_option (f : 'a -> 'b) (opt : 'a option) (l : 'b list) : 'b list =
  match opt with
  | None -> l
  | Some x -> f x :: l

let ( ^:: ) = cons_option
