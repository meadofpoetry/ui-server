open Common.Hardware
open Api_handler
open Interaction
open Board_meta

module V1 : BOARD = struct

  type 'a request = 'a Board_ip_protocol.request

  let create_sm = Board_ip_protocol.SM.create

  let create (b:topo_board) send step =
    Lwt_io.printf "in ip create\n" |> ignore;
    let s_state, spush = React.S.create `No_response in
    let events, send, step = create_sm send spush step in
    let handlers = Board_ip_api.handlers b.control send [] s_state s_state in
    let e_probes = React.E.map (fun x -> Common.Board.Ip.board_status_to_yojson x
                                         |> Yojson.Safe.pretty_to_string
                                         |> Lwt_io.printf "%s\n"
                                         |> ignore)
                               events.status in
    let state = object method e_probes = e_probes end in
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
