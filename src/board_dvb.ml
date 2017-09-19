open Common.Hardware
open Api_handler
open Interaction
open Board_meta

open Lwt.Infix

module V1 : BOARD = struct

  type 'a request = 'a Board_dvb_protocol.request

  let create_sm = Board_dvb_protocol.SM.create
                   
  let create (b:topo_board) send step =
    Lwt_io.printf "in create\n" |> ignore;
    let s_state, spush = React.S.create `No_response in
    let events, api, step = create_sm send spush step in
    let handlers = Board_dvb_api.handlers b.control api events in (* XXX temporary *)
    let e_probes = React.E.map (fun (id,x) -> Common.Board.Dvb.rsp_measure_to_yojson x
                                              |> Yojson.Safe.to_string
                                              |> Lwt_io.printf "%d %s\n" id
                                              |> ignore)
                     events.measure in
    let state = object method e_probes = e_probes method s_state = s_state end in
    { handlers       = handlers
    ; control        = b.control
    ; connection     = s_state
    ; streams_signal = None
    ; step           = step
    ; is_converter   = false
    ; is_active      = (React.S.map (function `No_response -> false | `Fine -> true) s_state)
    ; ports_active   = (List.fold_left (fun m p -> Ports.add p.port (React.S.const true) m) Ports.empty b.ports)
    ; state          = (state :> < >)
    }

  let connect_db b _ = b
end

let create = function
  | 1 -> (module V1 : BOARD)
  | v -> failwith ("dvb board: unknown version " ^ (string_of_int v))
