open Common.Hardware
open Api_handler
open Interaction
open Board_meta

module V1 : BOARD = struct

  module Protocol = Board_qos_protocol

  type event = Protocol.event
  type response = Protocol.response
  type 'a request = 'a Protocol.request

  module Messenger = Board_meta.Make(Protocol)
  module Board_api = Board_qos_api.Make(Protocol)

  let create (b:topo_board) send =
    let e_msgs,  push = React.E.create () in
    let s_state, spush = React.S.create `No_response in
    let send_resp, send_inst, step = Messenger.create send spush push in
    let handlers = Board_api.handlers b.control send_resp send_inst s_state e_msgs in
    let e_probes = React.E.map (function
                                | Protocol.Status x -> Common.Board.Qos.status_to_yojson x
                                                       |> Yojson.Safe.to_string
                                                       |> Lwt_io.printf "%s\n"
                                                       |> ignore
                                | Protocol.Ts_errors x -> Common.Board.Qos.ts_errors_to_yojson x
                                                          |> Yojson.Safe.to_string
                                                          |> Lwt_io.printf "%s\n"
                                                          |> ignore
                                | _ -> Lwt_io.printf "smth\n"
                                       |> ignore) e_msgs in
    let state = object method e_msgs = e_msgs; method e_probes = e_probes  end in
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
  | v -> failwith ("qos board: unknown version " ^ (string_of_int v))