(** CSS class constructors according to BEM methodology
    @see <https://en.bem.info/>
*)

let elt_delim = "__"

let mod_delim = "--"

let add_element (b : string) (e : string) = Printf.sprintf "%s%s%s" b elt_delim e

let add_modifier (b : string) (m : string) = Printf.sprintf "%s%s%s" b mod_delim m

let split (s : string) =
  let ( = ) = String.equal in
  let delims = Printf.sprintf "%s\\|%s" elt_delim mod_delim in
  match Str.full_split (Str.regexp delims) s with
  | [Text b] -> Ok (b, None, None)
  | [Text b; Delim d; Text e] when d = elt_delim -> Ok (b, Some e, None)
  | [Text b; Delim d; Text m] when d = mod_delim -> Ok (b, None, Some m)
  | [Text b; Delim d1; Text e; Delim d2; Text m] when d1 = elt_delim && d2 = mod_delim ->
      Ok (b, Some e, Some m)
  | _ -> Error (`Msg "invalid BEM string")

let get_block s =
  match split s with
  | Error _ -> None
  | Ok (b, _, _) -> Some b

let get_element s =
  match split s with
  | Ok (_, Some e, _) -> Some e
  | _ -> None

let get_modifier s =
  match split s with
  | Ok (_, _, Some m) -> Some m
  | _ -> None
