open Common.Topology
open Boards.Board
open Board_qos_types
open Containers
open Common

module Api_handler = Api.Handler.Make (Common.User)

module Data = struct

  type t      = config
  let default = config_default
  let dump    = config_to_string
  let restore = config_of_string

end

module Config_storage = Storage.Options.Make (Data)

let tick tm =
  let open Lwt.Infix in
  let e, push = Lwt_react.E.create () in
  let rec loop () =
    push (); Lwt_unix.sleep tm >>= loop
  in
  e, loop

module Split = struct

  type 'a coll = 'a list timestamped
  type 'a flat = 'a timestamped list

  let flatten_item (c : 'a coll) : 'a flat =
    List.map (fun x -> make_timestamped c.timestamp x) c.data

  let flatten (c : (Stream.ID.t * 'a coll) list) =
    List.map (Pair.map2 flatten_item) c

  let split
        ~(eq : 'a -> 'a -> bool)
        (past : (Stream.ID.t * 'a coll) list)
        (pres : (Stream.ID.t * 'a coll) list) =
    let l =
      List.map (fun (id, ({ timestamp; data } as inner)) ->
          match List.Assoc.get ~eq:Stream.ID.equal id past with
          | None -> id, [], flatten_item inner
          | Some { timestamp = past_time; data = past_data } ->
             let (lost : 'a flat) =
               List.filter_map (fun x ->
                   if List.mem ~eq x data then None else
                     Some (make_timestamped timestamp x)) past_data in
             let (upd : 'a flat) =
               List.map (fun x ->
                   if List.mem ~eq x past_data
                   then make_timestamped past_time x
                   else make_timestamped timestamp x) data in
             id, lost, upd) pres in
    List.fold_left (fun (lost, upd) (id, l, u) ->
        let lost = match l with
          | [] -> lost
          | x -> (id, x) :: lost in
        (lost, (id, u) :: upd)) ([], []) l

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
  raise (Invalid_port s)

let get_ports_sync prefix streams input ports =
  let open React in
  List.fold_left (fun acc p ->
      begin match p.port with
      | 0 -> S.l2 (fun i s -> match i, s with
                              | SPI, _ :: _ -> true
                              | _ -> false) input streams
      | 1 -> S.l2 (fun i s -> match i, s with
                              | ASI, _ :: _ -> true
                              | _ -> false) input streams
      | x -> invalid_port prefix x
      end
      |> fun x -> Ports.add p.port x acc) Ports.empty ports

let get_ports_active prefix input ports =
  let open React in
  List.fold_left (fun acc p ->
      begin match p.port with
      | 0 -> S.map (function SPI -> true | _ -> false) input
      | 1 -> S.map (function ASI -> true | _ -> false) input
      | x -> invalid_port prefix x
      end
      |> fun x -> Ports.add p.port x acc) Ports.empty ports

let make_templates (b : topo_board) =
  let open Api.Template in
  let open Common.Uri in
  let template =
    { title = Some ""
    ; pre_scripts = [ Src "/js/moment.min.js"
                    ; Src "/js/Chart.min.js"
                    ; Src "/js/Chart.PieceLabel.min.js"
                    ]
    ; post_scripts = [ Src "/js/board_qos_stream.js" ]
    ; stylesheets = []
    ; content = []
    } in
  let node =
    let pre = Printf.sprintf "board/%d/stream" b.control in
    Pure { path = Path.Format.(pre @/ Stream.ID.fmt ^/ empty)
         ; template } in
  let rval = [ `Index 1, node ]
  in
  User.({ root = rval
        ; operator = rval
        ; guest = rval })

let create (b : topo_board) _ convert_streams send db_conf base step =
  let open Split in
  let log_name = Boards.Board.log_name b in
  let log_src = Logs.Src.create log_name in
  Option.iter (fun x -> Logs.Src.set_level log_src @@ Some x) b.logs;
  let (module Logs : Logs.LOG) = Logs.src_log log_src in
  let module SM = Board_protocol.Make(Logs) in
  let sources = match b.sources with
    | None ->
       let s = log_name ^ ": no sources provided!" in
       raise (Invalid_sources s)
    | Some x ->
       begin match init_of_yojson x with
       | Ok init -> init
       | Error s -> raise (Invalid_sources s)
       end in
  let conv = fun x -> convert_streams x b in
  let storage =
    Config_storage.create base
      ["board"; (string_of_int b.control)] in
  let ({ ts; _ } as events), api, step =
    SM.create sources send storage step conv in
  let db = Result.get_exn @@ Db.Conn.create db_conf b.control in
  let handlers = Board_api.handlers b.control db sources api events in
  let tick, tick_loop = tick 5. in
  let open Lwt_react in
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
  E.(keep @@ map_p (Db.Ts_info.insert db) @@ S.changes events.ts.info);
  E.(keep @@ map_s (function
                 | `New (lost, upd) ->
                    Logs.err (fun m -> m "lost: %d" (List.length lost));
                    let open Lwt.Infix in
                    Db.Pids.bump ~now:false db lost
                    >>= fun () -> Db.Pids.insert db upd
                 | `Bump x -> Db.Pids.bump ~now:true db x)
     @@ select [ map (fun x -> `New x) @@ S.diff (split ~eq:Pid.equal) ts.pids
               ; S.sample (fun () e -> `Bump (flatten e)) tick ts.pids ]);
  (* E.(keep @@ map_s (function
   *                | `Tick x -> Db.Pids.bump db x
   *                | `New (prev, pres) ->
   *                   let open Lwt.Infix in
   *                   Db.Pids.bump db prev
   *                   >>= fun () -> Db.Pids.insert db pres)
   *    @@ select [ S.diff (fun pres prev -> `New (prev, pres)) events.ts.pids
   *              ; S.sample (fun _ e -> `Tick e) tick events.ts.pids]); *)
  (* E.(keep @@ map_p (Db.Streams.insert_services db) events.ts.services);
   * E.(keep @@ map_p (Db.Streams.insert_tables db) events.ts.tables);
   * E.(keep @@ map_p (Db.Pids.insert db) events.ts.pids); *)
  (* Structs t2 *)
  (* E.(keep @@ map_p (Db.Streams.insert_t2mi_info db) events.t2mi.structures); *)
  (* TS bitrates *)
  (* Errors *)
  (* E.(keep @@ map_p (Db.Errors.insert ~is_ts:true  db) events.ts.errors);
   * E.(keep @@ map_p (Db.Errors.insert ~is_ts:false db) events.t2mi.errors); *)
  let state = (object
                 val db    = db
                 val _tick = tick_loop ()
                 method finalize () = ()
               end) in (* TODO fix finalize *)
  { handlers = handlers
  ; control = b.control
  ; streams_signal = events.streams
  ; step
  ; connection = events.device.state
  ; ports_sync = get_ports_sync log_name
                   events.streams
                   events.device.input
                   b.ports
  ; ports_active = get_ports_active log_name
                     events.device.input
                     b.ports
  ; stream_handler = None
  ; state = (state :> < finalize : unit -> unit >)
  ; templates = Some (make_templates b)
  }
