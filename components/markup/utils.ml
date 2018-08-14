open Containers

let (^::) = List.cons_maybe
let cons_if case x l = if case then x :: l else l
let cons_option = List.cons_maybe
let map_cons_option f opt l = Option.map_or ~default:l (fun x -> (f x) :: l) opt
let map_cons_if ~f case x l = if case then (f x) :: l else l
let (<@>) l x = match x with Some x -> l @ x | None -> l
