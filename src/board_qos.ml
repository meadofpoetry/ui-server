open Common.Hardware
open Api_handler
open Interaction
open Board_types

module Make (V : VERSION) : BOARD = struct

  type t = { handlers : (module HANDLER) list }

  let handle _ _ id meth args _ _ _ =
    let open Redirect in
    let redirect_if_guest = redirect_if (User.eq id `Guest) in
    match meth, args with
    | `POST, ["settings"] ->
       redirect_if_guest not_found
    | `GET, (["section"; _; _; _; _; _] as l) ->
       respond_string (String.concat " " l) ()
    | `GET, ["table"; stream_id; table_id] ->
       respond_string (String.concat " " ["table"; stream_id; table_id]) ()
    | _ -> not_found ()

  let handlers id =
    [ (module struct
         let domain = get_api_path id
         let handle = handle () ()
       end : HANDLER) ]

  let create (b:topo_board) = { handlers = handlers b.control }

  let connect_db _ _ = ()

  let get_handlers (b:t) = b.handlers

end
