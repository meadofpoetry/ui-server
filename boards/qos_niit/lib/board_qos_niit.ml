open Boards
open Board_qos_types
open Containers
open Lwt.Infix
open Common

let ( % ) = Fun.( % )

module Api_handler = Api.Handler.Make (User)

module Data = struct

  type t = config
  let default =
    { input = ASI
    ; t2mi_mode = None
    ; jitter_mode = None
    }
  let dump c = Yojson.Safe.to_string @@ config_to_yojson c
  let restore s = config_of_yojson @@ Yojson.Safe.from_string s

end

module Config_storage = Storage.Options.Make (Data)

let tick tm =
  let e, push = React.E.create () in
  let rec loop () =
    push (); Lwt_unix.sleep tm >>= loop
  in
  e, loop

module DB_handler = struct

  module Coll = struct

    type 'a coll = 'a list timestamped
    type 'a flat = 'a timespan list
    type 'a t =
      { lost : (Stream.ID.t * 'a flat) list
      ; found : (Stream.ID.t * 'a flat) list
      ; upd : (Stream.ID.t * 'a flat) list
      }

    module I = struct

      type 'a t =
        { lost : 'a flat
        ; found : 'a flat
        ; upd : 'a flat
        }

      let flatten ?till (c : 'a coll) : 'a flat =
        let till = match till with
          | Some x -> x
          | None -> c.timestamp in
        List.map (make_timespan ~from:c.timestamp ~till) c.data

      let split ~(eq : 'a -> 'a -> bool)
            (past : (Stream.ID.t * 'a flat) list)
            (id, (data : 'a coll)) =
        match List.Assoc.get ~eq:Stream.ID.equal id past with
        | None -> id, { lost = []; upd = []; found = flatten data }
        | Some past_data ->
           let (lost : 'a flat) =
             List.filter_map (fun (x : 'a timespan) ->
                 if List.mem ~eq x.data data.data then None else
                   Some { x with till = data.timestamp }) past_data in
           let (found : 'a flat), (upd : 'a flat) =
             List.fold_left (fun (acc_new, acc_upd) (x : 'a) ->
                 match List.find_opt (fun (i : 'a timespan) -> eq x i.data)
                         past_data with
                 | Some p ->
                    let i = { p with till = data.timestamp } in
                    (acc_new, i :: acc_upd)
                 | None ->
                    let till = data.timestamp in
                    let from = data.timestamp in
                    let i = make_timespan ~from ~till x in
                    (i :: acc_new, acc_upd)) ([], []) data.data in
           (id, { lost; found; upd })

    end

    let empty =
      { lost = []; found = []; upd = [] }

    let flatten ?till x =
      List.map (fun (id, x) ->
          id, I.flatten ?till x) x

    let split ~(eq : 'a -> 'a -> bool)
          (pres : (Stream.ID.t * 'a coll) list)
          (past : (Stream.ID.t * 'a flat) list) : 'a t =
      let (l : (Stream.ID.t * 'a I.t) list) =
        List.map (I.split ~eq past) pres in
      let lost, upd, found =
        List.fold_left (fun (lost_acc, found_acc, upd_acc)
                            (id, ({ lost; found; upd } : 'a I.t)) ->
            let lost = match lost with
              | [] -> lost_acc
              | x -> (id, x) :: lost_acc in
            let found = match found with
              | [] -> found_acc
              | x -> (id, x) :: found_acc in
            let upd = match upd with
              | [] -> upd_acc
              | x -> (id, x) :: upd_acc in
            (lost, upd, found)) ([], [], []) l
      in
      { lost; found; upd }

    let handle ~eq ~insert ~bump db tick s =
      let open React in
      let open E in
      S.changes s
      |> fold (fun { upd; found; _ } n ->
             split ~eq n (upd @ found)) empty
      |> (fun e ->
        select [e; S.sample (fun () e ->
                       let upd = flatten ~till:(Ptime_clock.now ()) e in
                       { upd; lost = []; found = [] }) tick s])
      |> map_s (fun { lost; upd; found } ->
             let t = if List.is_empty lost then Lwt.return_unit
                     else bump db lost in
             t
             >>= (fun () ->
               if List.is_empty upd then Lwt.return_unit
               else bump db upd)
             >>= (fun () ->
               if List.is_empty found then Lwt.return_unit
               else insert db found))
      |> keep

  end

  module Single = struct

    type 'a t =
      { lost : (Stream.ID.t * 'a timespan) list
      ; found : (Stream.ID.t * 'a timespan) list
      ; upd : (Stream.ID.t * 'a timespan) list
      }

    let empty =
      { lost = []; found = []; upd = [] }

    let split ~(eq : 'a -> 'a -> bool)
          (pres : (Stream.ID.t * 'a timestamped) list)
          (past : (Stream.ID.t * 'a timespan) list) : 'a t =
      let _ = eq in
      ignore pres; ignore past;
      empty

    let handle ~eq ~insert ~bump db tick s =
      let _ = tick in
      let _ = eq in
      let open React in
      let open E in
      S.changes s
      |> fold (fun { upd; found; _ } n ->
             split ~eq:Ts_info.equal n (upd @ found)) empty
      |> (fun e -> select [e; e])
      |> map_s (fun { lost; upd; found } ->
             let t = if List.is_empty lost then Lwt.return_unit
                     else bump db lost in
             t
             >>= (fun () ->
               if List.is_empty upd then Lwt.return_unit
               else bump db upd)
             >>= (fun () ->
               if List.is_empty found then Lwt.return_unit
               else insert db found))
      |> keep

  end

end

let appeared_streams
      sources
      ~(past : Stream.t list)
      ~(pres : Stream.t list) =
  let open Common.Stream in
  let rec not_in_or_diff s = function
    | [] -> true
    | so :: _ when equal so s -> false
    | _ :: tl -> not_in_or_diff s tl
  in
  let appeared =
    List.fold_left (fun acc pres ->
        if not_in_or_diff pres past
        then (Board_protocol.is_incoming sources pres, pres) :: acc
        else acc) [] pres in
  appeared

let invalid_port prefix x =
  let s = prefix ^ ": invalid port " ^ (string_of_int x) in
  raise (Board.Invalid_port s)

let get_ports_sync prefix streams input ports =
  let open React in
  let eq = Equal.bool in
  List.fold_left (fun acc (p : Topology.topo_port) ->
      begin match p.port with
      | 0 -> S.l2 ~eq (fun i s ->
                 match i, s with
                 | ASI, _ | SPI, [] -> false
                 | _ -> true) input streams
      | 1 -> S.l2 ~eq (fun i s ->
                 match i, s with
                 | SPI, _ | ASI, [] -> false
                 | _ -> true) input streams
      | x -> invalid_port prefix x
      end
      |> fun x -> Board.Ports.add p.port x acc)
    Board.Ports.empty ports

let get_ports_active prefix input ports =
  let open React in
  let eq = Equal.bool in
  List.fold_left (fun acc (p : Topology.topo_port) ->
      begin match p.port with
      | 0 -> S.with_finaliser
               (fun () -> Logs.info (fun m -> m "!!! finalized !!!"))
               @@ S.map ~eq (function SPI -> Logs.info (fun m -> m "spi true"); true | ASI -> false) input
      | 1 -> S.map ~eq (function ASI -> Logs.info (fun m -> m "asi true"); true | SPI -> false) input
      | x -> invalid_port prefix x
      end
      |> fun x -> Board.Ports.add p.port x acc)
    Board.Ports.empty ports

let log_of_errors event source
    : Stream.Log_message.t list React.event =
  let open Stream.Log_message in
  let filter l =
    List.filter (fun (id, _) ->
        match source with
        | `All -> true
        | `Id ids -> List.mem ~eq:Stream.ID.equal id ids) l in
  let map = fun ((id : Stream.ID.t), (errors : Error.t_ext list)) ->
    List.map (fun (error : Error.t_ext) ->
        let num, name = Ts_error.Kind.of_error error in
        let service = Option.flat_map (fun (x : Pid.info) -> x.service_name)
                        (snd error.pid) in
        let pid =
          { typ = Option.map (fun (x : Pid.info) ->
                      Pid.typ_to_string x.typ)
                    (snd error.pid)
          ; id = fst error.pid
          } in
        make ?service
          ~time:error.time
          ~level:Warn
          ~message:(num ^ " " ^ name)
          ~info:(Ts_error.Info.of_error error)
          ~stream:id
          ~pid
          ()) errors in
  React.E.map (List.concat % List.map map % filter) event

let log_of_device (control : int) (events : device_events) =
  let open Stream.Log_message in
  let make = make ~node:(Board control) in
  let state =
    React.E.fmap (function
        | `Fine ->
           make ~time:(Ptime_clock.now ())
             ~level:Info
             ~message:"Восстановление после внутреннего сбоя"
             ~info:""
             ()
           |> List.return
           |> Option.return
        | _ -> None)
    @@ React.S.changes events.state in
  let errors =
    React.E.map (fun (x : Board_error.t list) ->
        List.map (fun (e : Board_error.t) ->
            let prefix = match e.source with
              | Hardware -> "Hardware error"
              | Protocol -> "Protocol error" in
            let info = Printf.sprintf "%s: code = %d, count = %d"
                         prefix e.code e.count in
            let info = match e.param with

              | None -> info
              | Some p -> Printf.sprintf "%s, param = %d" info p in
            make ~time:(Ptime_clock.now ())
              ~level:Fatal
              ~message:"Внутренний сбой"
              ~info
              ()) x) events.errors in
  React.E.merge (@) [] [state; errors]

let log_of_streams (control : int) event source =
  let s = match source with
    | `All -> event
    | `Id ids ->
       React.S.fmap ~eq:(Equal.list Stream.equal) (fun streams ->
           List.filter (fun (s : Stream.t) ->
               List.mem ~eq:Stream.ID.equal s.id ids) streams
           |> function [] -> None | l -> Some l)
         [] event in
  React.S.diff (fun cur old ->
      let open Stream.Log_message in
      let eq = Stream.equal in
      let found = List.filter (fun s -> not @@ List.mem ~eq s old) cur in
      let lost = List.filter (fun s -> not @@ List.mem ~eq s cur) old in
      let time = Ptime_clock.now () in (* FIXME should be time from status *)
      let make ~message ~level (s : Stream.t) =
        make ~time
          ~message
          ~level
          ~info:(Stream.Source.to_string s.source.info)
          ~stream:s.id
          ?input:(Stream.get_input s)
          ~node:(Board control)
          () in
      let found' =
        List.map (fun (s : Stream.t) ->
            make ~level:Info ~message:"Обнаружен поток" s) found in
      let lost' =
        List.map (fun (s : Stream.t) ->
            make ~level:Err ~message:"Пропадание потока" s) lost in
      found' @ lost') s

let make_log_event (control : int) (events : events) source
    : Stream.Log_message.t list React.event =
  let errors = log_of_errors events.ts.errors source in
  let device = log_of_device control events.device in
  let streams = log_of_streams control events.streams source in
  React.E.merge (@) [] [errors; device; streams]

let create (b : Topology.topo_board) _ convert_streams send
      db_conf base step : Board.t =
  let open DB_handler in
  let log_name = Boards.Board.log_name b in
  let log_src = Logs.Src.create log_name in
  Option.iter (fun x -> Logs.Src.set_level log_src @@ Some x) b.logs;
  let (module Logs : Logs.LOG) = Logs.src_log log_src in
  let module SM = Board_protocol.Make(Logs) in
  let sources = match b.sources with
    | None ->
       let s = log_name ^ ": no sources provided!" in
       raise (Board.Invalid_sources s)
    | Some x ->
       begin match init_of_yojson x with
       | Ok init -> init
       | Error s -> raise (Board.Invalid_sources s)
       end in
  let conv = fun x -> convert_streams x b in
  let storage =
    Config_storage.create base
      ["board"; (string_of_int b.control)] in
  let ({ ts; t2mi; _ } as events), api, step =
    SM.create sources send storage step conv in
  let db = Result.get_exn @@ Db.Conn.create db_conf b.control in
  let handlers = Board_api.handlers b.control db sources api events in
  let tick, tick_loop = tick 5. in
  let open React in
  (* State *)
  Lwt.ignore_result @@ Db.Device.init db;
  E.keep
  @@ E.map_p (fun e -> Db.Device.bump db e)
  @@ E.select [ S.changes events.device.state
              ; S.sample (fun _ e -> e) tick events.device.state ];
  (* Streams *)
  let streams_ev =
    S.sample (fun () sl -> `Active sl) tick events.streams in
  let streams_diff =
    S.diff (fun pres past -> `New (appeared_streams sources ~past ~pres))
      events.streams in
  E.(keep
     @@ map_s (function
            | `Active x -> Db.Streams.bump_streams db x
            | `New x -> Db.Streams.insert_streams db x)
     @@ select [streams_ev; streams_diff]);
  Db.Ts_info.(Single.handle ~eq:Ts_info.equal ~insert ~bump db tick ts.info);
  Db.Pids.(Coll.handle ~eq:Pid.equal ~insert ~bump db tick ts.pids);
  Db.Services.(Coll.handle ~eq:Service.equal ~insert ~bump db tick ts.services);
  Db.T2mi_info.(Coll.handle ~eq:T2mi_info.equal ~insert ~bump db tick t2mi.structures);
  E.(keep @@ map_s (Db.Bitrate.insert db) ts.bitrates);
  E.(keep @@ map_s (Db.Bitrate.insert_pids db) ts.bitrates);
  (* E.(keep @@ map_p (Db.Errors.insert ~is_ts:true  db) events.ts.errors); *)
  E.(keep @@ map_p (Db.Errors.insert ~is_ts:false db) events.t2mi.errors);
  let state = (object
                 val db    = db
                 val _tick = tick_loop ()
                 method finalize () = ()
               end) in (* TODO fix finalize *)
  { handlers = handlers
  ; control = b.control
  ; streams_signal = events.streams
  ; log_source = make_log_event b.control events
  ; step
  ; connection = events.device.state
  ; ports_sync =
      get_ports_sync log_name
        events.streams
        events.device.input
        b.ports
  ; ports_active =
      get_ports_active log_name
        events.device.input
        b.ports
  ; stream_handler = None
  ; state = (state :> < finalize : unit -> unit >)
  ; templates = None
  }
