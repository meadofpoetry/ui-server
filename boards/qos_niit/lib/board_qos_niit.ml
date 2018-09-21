open Common.Topology
open Boards.Board
open Board_qos_types
open Containers
open Common
open Lwt.Infix

module Api_handler = Api.Handler.Make (Common.User)

module Data = struct

  type t      = config
  let default = config_default
  let dump    = config_to_string
  let restore = config_of_string

end

module Config_storage = Storage.Options.Make (Data)

let tick tm =
  let e, push = Lwt_react.E.create () in
  let rec loop () =
    push (); Lwt_unix.sleep tm >>= loop
  in
  e, loop

module DB_handler = struct

  type 'a coll = 'a list timestamped
  type 'a flat = 'a timespan list

  module Split_item = struct

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

  type 'a t =
    { lost : (Stream.ID.t * 'a flat) list
    ; found : (Stream.ID.t * 'a flat) list
    ; upd : (Stream.ID.t * 'a flat) list
    }

  let flatten ?till x =
    List.map (fun (id, x) ->
        id, Split_item.flatten ?till x) x

  let split
        ~(eq : 'a -> 'a -> bool)
        (pres : (Stream.ID.t * 'a coll) list)
        (past : (Stream.ID.t * 'a flat) list) : 'a t =
    let (l : (Stream.ID.t * 'a Split_item.t) list) =
      List.map (Split_item.split ~eq past) pres in
    let lost, upd, found =
      List.fold_left (fun (lost_acc, found_acc, upd_acc)
                          (id, ({ lost; found; upd } : 'a Split_item.t)) ->
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

  let handle_list db tick s =
    let open Lwt_react in
    let open E in
    S.changes s
    |> fold (fun { upd; found; _ } n ->
           split ~eq:Pid.equal n (upd @ found))
         { lost = []; upd = []; found = [] }
    |> (fun e ->
      select [e; S.sample (fun () e ->
                     let upd = flatten ~till:(Time.Clock.now_s ()) e in
                     { upd; lost = []; found = [] }) tick s])
    |> map_s (fun { lost; upd; found } ->
           let t = if List.is_empty lost then Lwt.return_unit
                   else Db.Pids.bump db lost in
           t
           >>= (fun () ->
             if List.is_empty upd then Lwt.return_unit
             else Db.Pids.bump db upd)
           >>= (fun () ->
             if List.is_empty found then Lwt.return_unit
             else Db.Pids.insert db found))
    |> keep

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
  (* PIDs *)
  DB_handler.handle_list db tick ts.pids;
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
