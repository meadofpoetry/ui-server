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
    | `POST, ["settings"] -> redirect_if_guest not_found
    | `GET, ["devinfo"]   -> not_found ()
    | `GET, ["plps"]      -> not_found ()
    | `GET, ["meas"]      -> not_found ()
    | `GET, ["params"]    -> not_found ()
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
