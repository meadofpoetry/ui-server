open Common.Hardware
open Api_handler
open Interaction
open Board_meta

module V1 : BOARD = struct

  module Protocol = Board_ip_protocol

  type event = Protocol.event
  type response = Protocol.response
  type 'a request = 'a Protocol.request
                  
  module Messenger = Board_meta.Make(Protocol)
  module Board_api = Board_ip_api.Make(Protocol)

  let create (b:topo_board) send =
    Lwt_io.printf "in ip create\n" |> ignore;
    let e_msgs,  push = React.E.create () in
    let s_state, spush = React.S.create `No_response in
    let send_resp, send_inst, step = Messenger.create send spush push in
    let handlers = Board_api.handlers b.control send_resp send_inst s_state e_msgs in
    let e_probes = React.E.map (fun _ -> ())
                               e_msgs in
    let state = object method e_msgs = e_msgs; method e_probes = e_probes  end in
    { handlers       = handlers
    ; control        = b.control
    ; connection     = s_state
    ; streams_signal = None
    ; step           = step
    ; is_converter   = false
    ; is_active      = (React.S.const true)
    ; ports_active   = (List.fold_left (fun m p -> Ports.add p.port (React.S.const true) m) Ports.empty b.ports)
    ; state          = (state :> < >)
    }

  let connect_db b _ = b

end

let create = function
  | 1 -> (module V1 : BOARD)
  | v -> failwith ("ip board: unknown version " ^ (string_of_int v))
