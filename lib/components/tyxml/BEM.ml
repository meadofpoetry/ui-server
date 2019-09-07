(** CSS class constructors according to BEM methodology
    @see <http://getbem.com/naming/>
*)

let elt_delim = "__"

let mod_delim = "--"

let add_element ?(element_delimiter = elt_delim) (b : string) (e : string) =
  Printf.sprintf "%s%s%s" b element_delimiter e

let add_modifier ?(modifier_delimiter = mod_delim) (b : string) (m : string) =
  Printf.sprintf "%s%s%s" b modifier_delimiter m

let split ?(element_delimiter = elt_delim) ?(modifier_delimiter = mod_delim) (s : string)
    =
  let ( = ) = String.equal in
  let delims = Printf.sprintf "%s\\|%s" element_delimiter modifier_delimiter in
  match Str.full_split (Str.regexp delims) s with
  | [Text b] -> Ok (b, None, None)
  | [Text b; Delim d; Text e] when d = elt_delim -> Ok (b, Some e, None)
  | [Text b; Delim d; Text m] when d = mod_delim -> Ok (b, None, Some m)
  | [Text b; Delim d1; Text e; Delim d2; Text m] when d1 = elt_delim && d2 = mod_delim ->
      Ok (b, Some e, Some m)
  | _ -> Error (`Msg "invalid BEM string")

let get_block ?element_delimiter ?modifier_delimiter s =
  match split ?element_delimiter ?modifier_delimiter s with
  | Error _ -> None
  | Ok (b, _, _) -> Some b

let get_element ?element_delimiter ?modifier_delimiter s =
  match split ?element_delimiter ?modifier_delimiter s with
  | Ok (_, Some e, _) -> Some e
  | _ -> None

let get_modifier ?element_delimiter ?modifier_delimiter s =
  match split ?element_delimiter ?modifier_delimiter s with
  | Ok (_, _, Some m) -> Some m
  | _ -> None
