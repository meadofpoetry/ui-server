open Common.Hardware
open Api_handler
open Interaction
open Board_meta

module V1 : BOARD = struct

  let handle send _ id meth args _ _ _ =
    let open Redirect in
    let redirect_if_guest = redirect_if (User.eq id `Guest) in
    match meth, args with
    | `POST, ["settings"] -> redirect_if_guest not_found
    | `POST, ["plp"]      -> redirect_if_guest not_found
    | `GET,  ["devinfo"]  -> respond_string "sent cmd devinfo!" ()
    | `GET,  ["params"]   -> not_found ()
    | `GET,  ["meas"]     -> not_found ()
    | `GET,  ["plps"]     -> respond_string "send cmd plp list" ()
    | _ -> not_found ()

  let handlers id send =
    [ (module struct
         let domain = get_api_path id
         let handle = handle send ()
       end : HANDLER) ]

  (* TODO Все, что выше, убрать *)

  module Protocol : Board_meta.PROTOCOL = Board_dvb_protocol

  type req = Protocol.req
  type resp = Protocol.resp

  module Messenger = Board_meta.Make(Protocol)
  module Board_api = Board_dvb_api.Make(Protocol)

  let create (b:topo_board) send =
    let e_msgs,  push = React.E.create () in
    let s_state, spush = React.S.create `No_response in
    let send_msg, step = Messenger.create send spush push in
    let handlers = Board_api.handlers b.control send_msg s_state e_msgs in
    let state = object method e_msgs = e_msgs end in
    { handlers       = handlers
    ; connection     = s_state
    ; streams_signal = None
    ; step           = step
    ; is_converter   = false
    ; state          = (state :> < >)
    }

  let connect_db b _ = b
end

let create = function
  | 1 -> (module V1 : BOARD)
  | v -> failwith ("dvb board: unknown version " ^ (string_of_int v))

                  (* React.E.fold (fun old buf ->
                    let msgs,res = parse ?old buf in
                    (parse_msgs msgs)
                    |> List.map (function
                                 | `Ok             -> "rsp ok"
                                 | `Devinfo x      -> "devinfo: "  ^ (devinfo_to_yojson x
                                                                      |> Yojson.Safe.to_string)
                                 | `Settings (_,x) -> "settings: " ^ (settings_resp_to_yojson x
                                                                      |> Yojson.Safe.to_string)
                                 | `Measure (_,x)  -> "measure: "  ^ (measure_to_yojson x
                                                                      |> Yojson.Safe.to_string)
                                 | `Plps (_,x)     -> "plps: "     ^ (plp_list_to_yojson x
                                                                      |> Yojson.Safe.to_string)
                                 | `Plp (_,x)      -> "plp: "      ^ (plp_set_to_yojson x
                                                                      |> Yojson.Safe.to_string)
                                 | `Corrupted      -> "corrupted"
                                 | _               -> "unknown")
                    |> List.map (Lwt_io.printf "%s\n")
                    |> ignore;
                    (Some res))
                             None
                             e_msgs *)
