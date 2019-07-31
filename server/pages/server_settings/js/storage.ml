open Ui_templates.Storage.Local_storage

module K = struct
  let show_private_key_disclaimer = "show-private-key-disclaimer"
end

let get_show_private_key_disclaimer () =
  match get K.show_private_key_disclaimer with
  | Some `Bool x -> x
  | _ -> true

let set_show_private_key_disclaimer (x : bool) =
  put K.show_private_key_disclaimer (`Bool x)
