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

  module Storage : sig
    type _ req =
      | Store_measures : Common.Board.Dvb.measure -> unit Lwt.t req
    include (Database.STORAGE with type 'a req := 'a req)
  end = Board_dvb_storage
  
  type 'a request = 'a Board_dvb_protocol.request

  let create_sm = Board_dvb_protocol.SM.create
                   
  let create (b:topo_board) send db base step =
    let storage      = Config_storage.create base ["board"; (string_of_int b.control)] in
    let s_state, spush = React.S.create `No_response in
    let events, api, step = create_sm send storage spush step in
    let handlers = Board_dvb_api.handlers b.control api events in (* XXX temporary *)
    Lwt_main.run @@ Storage.init db;
    let _s = Lwt_react.E.map_p (fun m -> Storage.request db (Storage.Store_measures m)) events.measure in
    let state = object method _s = _s end in
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
  | v -> failwith ("dvb board: unknown version " ^ (string_of_int v))
