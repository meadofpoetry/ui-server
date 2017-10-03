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
                                              (* |> Lwt_io.printf "%s\n" *)
                                              |> ignore)
                                    events.status in
  let e_ts_found      = React.E.map (fun strm ->
                            Common.Stream.to_yojson strm
                            |> Yojson.Safe.to_string
                            |> Lwt_io.printf "Stream found: %s\n"
                            |> ignore) events.ts_found in
  let e_ts_lost       = React.E.map (fun strm ->
                            Common.Stream.to_yojson strm
                            |> Yojson.Safe.to_string
                            |> Lwt_io.printf "Stream lost: %s\n"
                            |> ignore) events.ts_lost in
  let e_state         = React.S.map (function
                                     | `No_response -> Lwt_io.printf "QoS Board not responding\n"
                                     | `Fine        -> Lwt_io.printf "QoS Board is OK\n"
                                     | `Init        -> Lwt_io.printf "Qos Board is initializing\n") s_state in
  (* let e_jitter        = React.E.map (fun x -> Lwt_io.printf "Got jitter with next_ptr = %ld, items = %d\n" *)
  (*                                                           x.next_ptr (CCList.length x.values) *)
  (*                                             |> ignore) *)
  (*                                   events.jitter in *)
  (* let e_struct        = React.E.map (fun _ -> Lwt_io.printf "Got structs\n") events.structs in *)
  (* let e_bitrate       = React.E.map (fun _ -> Lwt_io.printf "Got bitrate\n") events.bitrate in *)
  let state           = (object
                           method e_status = e_status;
                           method e_state = e_state;
                           method e_ts_found = e_ts_found;
                           method e_ts_lost = e_ts_lost;
                           (* method e_jitter = e_jitter; *)
                           (* method e_struct = e_struct; *)
                           (* method e_bitrate = e_bitrate; *)
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
