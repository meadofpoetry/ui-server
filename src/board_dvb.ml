open Common.Hardware
open Api_handler
open Interaction
open Board_meta

open Lwt.Infix

module V1 : BOARD = struct

  module Data = struct
    type t = Common.Board.Dvb.config
    let default = Common.Board.Dvb.config_default
    let dump = Common.Board.Dvb.config_to_string
    let restore = Common.Board.Dvb.config_of_string
  end
  
  module Config_storage = Config_storage.Make (Data)
  
  type 'a request = 'a Board_dvb_protocol.request

  let create_sm = Board_dvb_protocol.SM.create
                   
  let create (b:topo_board) send base step =
    let storage      = Config_storage.create base ["board"; (string_of_int b.control)] in
    let s_state, spush = React.S.create `No_response in
    let events, api, step = create_sm send storage spush step in
    let handlers = Board_dvb_api.handlers b.control api events in (* XXX temporary *)
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
  | v -> failwith ("dvb board: unknown version " ^ (string_of_int v))
