open Common.Hardware
open Api_handler
open Interaction
open Board_meta

module V1 : BOARD = struct

  type 'a request = 'a Board_ip_protocol.request

  let create_sm = Board_ip_protocol.SM.create

  let create (b:topo_board) send _ step =
    Lwt_io.printf "in ip create\n" |> ignore;
    let s_state, spush = React.S.create `No_response in
    let events, api, step = create_sm send spush step in
    let handlers = Board_ip_api.handlers b.control api events in
    let state = object end in
    { handlers       = handlers
    ; control        = b.control
    ; streams_signal = None
    ; step           = step
    ; is_converter   = false
    ; connection     = s_state
    ; ports_active   = (List.fold_left (fun m p -> Ports.add p.port (React.S.const true) m) Ports.empty b.ports)
    ; state          = (state :> < >)
    }

  let connect_db b _ = b

end

let create = function
  | 1 -> (module V1 : BOARD)
  | v -> failwith ("ip board: unknown version " ^ (string_of_int v))
