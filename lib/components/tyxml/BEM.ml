(** CSS class constructors according to BEM methodology
    https://en.bem.info/
 *)

let add_element (block : string) (e : string) =
  Printf.sprintf "%s__%s" block e

let add_modifier (block : string) (m : string) =
  Printf.sprintf "%s--%s" block m
