open Js_of_ocaml
module Local = Browser_storage.Make_local (Application_types.User)

module K = struct
  let show_private_key_disclaimer = "show-private-key-disclaimer"
end

let user =
  let user = Js.to_string @@ Js.Unsafe.global##.username in
  match Application_types.User.of_string user with
  | Error e -> failwith e
  | Ok user -> user

let get_show_private_key_disclaimer () =
  match Local.get user K.show_private_key_disclaimer with
  | Some (`Bool x) -> x
  | _ -> true

let set_show_private_key_disclaimer (x : bool) =
  Local.put user K.show_private_key_disclaimer (`Bool x)
