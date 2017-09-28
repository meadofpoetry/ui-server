open Common.Hardware
open Api.Interaction
open Meta_board

module Api_handler = Api.Handler.Make(Common.User)

module Data = struct
  type t = Board_types.config
  let default = Board_types.config_default
  let dump = Board_types.config_to_string
  let restore = Board_types.config_of_string
end

module Config_storage = Storage.Options.Make (Data)

type 'a request = 'a Board_protocol.request

let create_sm = Board_protocol.SM.create

let create (b:topo_board) send _ base step =
  Lwt_io.printf "in ip create\n" |> ignore;
  let storage      = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush = React.S.create `No_response in
  let events, api, step = create_sm send storage spush step in
  let handlers = Board_api.handlers b.control api events in
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
