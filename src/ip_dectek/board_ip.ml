open Common.Hardware
open Api_handler
open Interaction
open Board_meta

module V1 : BOARD = struct

  module Data = struct
    type t = Common.Board.Ip.config
    let default = Common.Board.Ip.config_default
    let dump = Common.Board.Ip.config_to_string
    let restore = Common.Board.Ip.config_of_string
  end

  module Config_storage = Config_storage.Make (Data)

  type 'a request = 'a Board_ip_protocol.request

  let create_sm = Board_ip_protocol.SM.create

  let create (b:topo_board) send _ base step =
    Lwt_io.printf "in ip create\n" |> ignore;
    let storage      = Config_storage.create base ["board"; (string_of_int b.control)] in
    let s_state, spush = React.S.create `No_response in
    let events, api, step = create_sm send storage spush step in
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
    
end

let create = function
  | 1 -> (module V1 : BOARD)
  | v -> failwith ("ip board: unknown version " ^ (string_of_int v))
