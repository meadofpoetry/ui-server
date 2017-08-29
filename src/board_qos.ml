open Common.Hardware
open Api_handler
open Interaction
open Board_meta

module V1 : BOARD = struct

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

  let create (b:topo_board) _ =
    { handlers       = handlers b.control 
    ; receiver       = (fun _ -> ())
    ; streams_signal = None
    ; is_converter   = true
    ; state          = object end
    }

  let connect_db b _ = b

end

let create = function
  | 1 -> (module V1 : BOARD)
  | v -> failwith ("qos board: unknown version " ^ (string_of_int v))
