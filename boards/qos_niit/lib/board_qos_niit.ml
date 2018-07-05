open Common.Topology
open Boards.Board
open Board_types
open Containers

module Api_handler = Api.Handler.Make (Common.User)

module Data = struct

  type t      = config
  let default = config_default
  let dump    = config_to_string
  let restore = config_of_string

end

module Config_storage = Storage.Options.Make (Data)

let log_prefix control = Printf.sprintf "(Board QoS: %d) " control

let tick tm =
  let open Lwt.Infix in
  let e,push = Lwt_react.E.create () in
  let rec loop () =
    push (); Lwt_unix.sleep tm >>= loop
  in
  e, loop
                       
let create (b:topo_board) _ convert_streams send db_conf base step =
  let conv            = fun x -> convert_streams x b in
  let storage         = Config_storage.create base ["board"; (string_of_int b.control)] in
  let events,api,step = Board_protocol.SM.create (log_prefix b.control) send storage step conv in
  let db              = Result.get_exn @@ Db.Conn.create db_conf b.control in
  let handlers        = Board_api.handlers b.control db api events in
  let tick, tick_loop = tick 5. in
  Lwt_react.E.keep @@
    Lwt_react.E.map_p (fun e -> Db.Device.bump db e)
    @@ Lwt_react.S.sample (fun () e -> e) tick events.device.state;
  Lwt_react.E.keep @@
    Lwt_react.E.map_p (fun e -> Db.Errors.insert_errors ~is_ts:true db e) events.errors.ts_errors;
  Lwt_react.E.keep @@
    Lwt_react.E.map_p (fun e -> Db.Errors.insert_errors ~is_ts:false db e) events.errors.t2mi_errors;
  let state = (object val db = db val _tick = tick_loop () method finalize () = () end) in (* TODO fix finalize *)
  { handlers       = handlers
  ; control        = b.control
  ; streams_signal = events.streams.streams
  ; step           = step
  ; connection     = events.device.state
  ; ports_active   =
      List.fold_left (fun acc p ->
          (match p.port with
           | 0 -> React.S.map (function SPI -> true | _ -> false) events.device.input
           | 1 -> React.S.map (function ASI -> true | _ -> false) events.device.input
           | x -> raise (Invalid_port ("Board_qos_niit: invalid port " ^ (string_of_int x))))
          |> fun x -> Ports.add p.port x acc) Ports.empty b.ports
  ; settings_page  = ("QOS", React.S.const (Tyxml.Html.div []))
  ; widgets_page   = [("QOS", React.S.const (Tyxml.Html.div []))]
  ; stream_handler = None
  ; state          = (state :> < finalize : unit -> unit >)
  }
