open Common.Hardware
open Api.Interaction
open Meta_board

module Api_handler = Api.Handler.Make (Common.User)
   
open Lwt.Infix

module Data = struct
  type t = Board_types.config
  let default = Board_types.config_default
  let dump = Board_types.config_to_string
  let restore = Board_types.config_of_string
end
            
module Config_storage = Storage.Options.Make (Data)

module Storage : sig
  type _ req =
    | Store_measures : Board_types.measure -> unit Lwt.t req
  include (Storage.Database.STORAGE with type 'a req := 'a req)
end = Db
    
type 'a request = 'a Board_protocol.request

let create_sm = Board_protocol.SM.create
              
let create (b:topo_board) send db base step =
  let storage      = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush = React.S.create `No_response in
  let events, api, step = create_sm send storage spush step in
  let handlers = Board_api.handlers b.control api events in (* XXX temporary *)
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
