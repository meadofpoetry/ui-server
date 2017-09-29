open Common.Hardware
open Api.Interaction
open Meta_board
open Board_types

module Api_handler = Api.Handler.Make (Common.User)

open Lwt.Infix

module Data = struct

  type t = Board_types.config
  let default = Board_types.config_default
  let dump = Board_types.config_to_string
  let restore = Board_types.config_of_string

end

module Config_storage = Storage.Options.Make (Data)

type 'a request = 'a Board_protocol.request

let create_sm = Board_protocol.SM.create

let create (b:topo_board) send db base step =
  let storage         = Config_storage.create base ["board"; (string_of_int b.control)] in
  let s_state, spush  = React.S.create `No_response in
  let events,api,step = create_sm send storage spush step in
  let handlers        = Board_api.handlers b.control api events in
  let e_status        = React.E.map (fun x -> Board_types.user_status_to_yojson x
                                              |> Yojson.Safe.pretty_to_string
                                              |> Lwt_io.printf "%s\n"
                                              |> ignore)
                                    events.status in
  let e_sync          = React.E.map (fun (strm,sync) ->
                            Common.Stream.to_yojson strm
                            |> Yojson.Safe.to_string
                            |> (fun s -> if sync
                                         then Lwt_io.printf "Sync found on stream %s\n" s
                                         else Lwt_io.printf "Sync lost on stream %s\n" s)
                            |> ignore) events.ts_sync in
  let e_state         = React.S.map (function
                                     | `No_response -> Lwt_io.printf "QoS Board not responding\n"
                                     | `Fine        -> Lwt_io.printf "QoS Board is OK\n"
                                     | `Init        -> Lwt_io.printf "Qos Board is initializing\n") s_state in
  let e_jitter        = React.E.map (fun x -> Lwt_io.printf "Got jitter with ptr = %ld, nex_ptr = %ld, items = %d"
                                                            x.req_ptr x.next_ptr (CCList.length x.values)
                                              |> ignore)
                                    events.jitter in
  let state           = (object
                           method e_status = e_status;
                           method e_state = e_state;
                           method e_sync = e_sync;
                           method e_jitter = e_jitter;
                         end) in
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = None
  ; step           = step
  ; is_converter   = false
  ; connection     = s_state
  ; ports_active   = (List.fold_left (fun m p -> Ports.add p.port (React.S.const true) m) Ports.empty b.ports)
  ; state          = (state :> < >)
  }
