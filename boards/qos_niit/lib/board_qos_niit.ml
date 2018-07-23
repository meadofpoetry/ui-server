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

let appeared_streams ~(past:Common.Stream.t list) ~(pres:Common.Stream.t list) =
  let open Common.Stream in
  let rec not_in_or_diff s = function
    | [] -> true
    | so::_ when equal so s -> false
    | _::tl -> not_in_or_diff s tl
  in
  let appeared = List.fold_left (fun acc pres ->
                     if not_in_or_diff pres past
                     then pres::acc else acc) [] pres in
  appeared
                       
let create (b:topo_board) _ convert_streams send db_conf base step =
  let conv            = fun x -> convert_streams x b in
  let storage         = Config_storage.create base ["board"; (string_of_int b.control)] in
  let events,api,step = Board_protocol.SM.create (log_prefix b.control) send storage step conv in
  let db              = Result.get_exn @@ Db.Conn.create db_conf b.control in
  let handlers        = Board_api.handlers b.control db api events in
  let tick, tick_loop = tick 5. in
  (* State *)
  Lwt.ignore_result @@ Db.Device.init db;
  Lwt_react.E.keep
  @@ Lwt_react.E.map_p (fun e -> Db.Device.bump db e)
  @@ Lwt_react.E.select [ Lwt_react.S.changes events.device.state
                        ; Lwt_react.S.sample (fun _ e -> e) tick events.device.state ];
  (* Streams *)
  let streams_ev   =
    Lwt_react.S.sample (fun () sl -> `Active sl) tick events.streams.streams
  in
  let streams_diff =
    Lwt_react.S.diff (fun pres past -> `New (appeared_streams ~past ~pres)) events.streams.streams
  in
  Lwt_react.E.keep
  @@ Lwt_react.E.map_s (function
         | `Active streams -> Db.Streams.bump_streams db streams
         | `New streams -> Db.Streams.insert_streams db streams)
  @@ Lwt_react.E.select [streams_ev; streams_diff];
  (* @@ Lwt_react.S.map (fun s -> Lwt.ignore_result @@ Db.Streams.insert_streams db s) events.streams.streams; *)
  (* Structs ts *)
  Lwt_react.E.keep
  @@ Lwt_react.E.map_p (fun s -> Lwt.(catch (fun () -> Db.Streams.insert_structs_ts db s)
                                        (fun e -> Logs_lwt.err (fun m -> m "ERROR %s" @@ Printexc.to_string e))))
       events.streams.ts_structures;
  (* Structs t2 *)
  Lwt_react.E.keep
  @@ Lwt_react.E.map_p (fun s -> Db.Streams.insert_structs_t2 db s) events.streams.t2mi_structures;
  (* TS bitrates *)
(*  Lwt_react.E.keep
  @@ Lwt_react.E.map_p (fun b -> Db.Streams.insert_bitrate db b) events.streams.ts_bitrates; *)
  (* Errors *)
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
