open Application_types

module Api_http = Api_cohttp.Make(User)(Body)
module Api_events = Api_websocket.Make
                      (User)
                      (Body)
                      (Body_ws)
module Api_template = Api_cohttp_template.Make(User)

(* TODO remove in 4.08 *)
module Int = struct
  type t = int

  let compare = compare
end

let ( ^:: ) l x = match x with
  | None -> l
  | Some x -> x :: l

module Ports = Map.Make(Int)

exception Invalid_port of string

exception Invalid_sources of string

(* TODO better error types *)
type error =
  [ Kv_v.error
  | Db.conn_error
  | `Unknown_log_level of string
  | `Board_error of string
  ]

let pp_error ppf : error -> unit = function
  | `Board_error e -> Fmt.fmt "Board error %s" ppf e
  | `Unknown_log_level s -> Fmt.fmt "Unrecognized log level (%s)" ppf s
  | (`Db_connection_error _) as e -> Db.pp_conn_error ppf e
  | (`No_value _) as e -> Kv_v.pp_error ppf e

type constraints =
  { range : Netlib.Ipaddr.V4.range list
  ; state : Stream.Table.source_state React.signal
  }

type stream_handler =
  < streams : Stream.Table.stream list React.signal
  ; set : Stream.Table.setting list ->
      (unit, Stream.Table.set_error) Lwt_result.t
  ; constraints : constraints
  >

type t =
  { http : Api_http.t list
  ; ws : Api_events.t list
  ; templates : Api_template.topmost Api_template.item list
  ; control : int
  ; streams_signal : Stream.t list React.signal
  ; log_source : Stream.Log_message.source
  ; loop : unit -> unit Lwt.t
  (* TODO return Lwt here *)
  ; push_data : Cstruct.t -> unit
  ; connection : Topology.state React.signal
  ; ports_active : bool React.signal Ports.t
  ; ports_sync : bool React.signal Ports.t
  ; stream_handler : stream_handler option
  ; state : < finalize : unit -> unit Lwt.t >
  }

module type BOARD = sig
  open React
  val create :
    Topology.topo_board ->
    Stream.t list signal ->
    (Topology.topo_board ->
     Stream.Raw.t list signal ->
     Stream.t list signal) ->
    (Cstruct.t -> unit Lwt.t) ->
    Db.t ->
    Kv.RW.t -> (t, [> error]) Lwt_result.t
end

let log_name (b : Topology.topo_board) =
  Printf.sprintf "%s %s (%d)" b.manufacturer b.model b.control

let create_log_src (b : Topology.topo_board) =
  let log_name = log_name b in
  let log_src = Logs.Src.create log_name in
  match b.logs with
  | None -> Ok log_src
  | Some x ->
    match Logs.level_of_string x with
    | Ok x ->
      Logs.Src.set_level log_src x;
      Ok log_src
    | Error _ -> Error (`Unknown_log_level x)

let invalid_port (src : Logs.src) port =
  let s = (Logs.Src.name src) ^ ": invalid port " ^ (string_of_int port) in
  raise (Invalid_port s)

let await_no_response (state : Topology.state React.signal) =
  match React.S.value state with
  | `No_response | `Init | `Detect -> Lwt.return ()
  | `Fine ->
    Util_react.(
      E.next
      @@ E.fmap (function
          | `Init | `No_response | `Detect -> Some ()
          | `Fine -> None)
      @@ S.changes state)

module Map = Map.Make(Int)

let get_streams (boards : t Map.t) (topo : Topology.topo_board) :
  Stream.t list React.signal =
  let rec get_streams' acc = function
    | [] -> acc
    | (h : Topology.topo_port) :: tl ->
      begin match h.child with
        | Topology.Input _ -> get_streams' acc tl
        | Topology.Board b ->
          try
            let b = Map.find b.control boards in
            let eq = Util_equal.List.equal Stream.equal in
            get_streams' (React.S.l2 ~eq (@) b.streams_signal acc) tl
          with _ -> get_streams' acc tl
      end in
  get_streams' (React.S.const []) topo.ports

(* XXX what to do if this fails? *)
let merge_streams (boards : t Map.t)
    (topo : Topology.topo_board)
    (raw_streams : Stream.Raw.t list React.signal)
  : Stream.t list React.signal =
  let open Topology in
  let open Stream in
  let ports =
    List.fold_left (fun m port ->
        match port.child with
        | Input i -> Map.add port.port (`Input i) m
        | Board b -> try let b = Map.find b.control boards in
            Map.add port.port (`Streams b.streams_signal) m
          with _ -> m)
      Map.empty topo.ports in
  (* When a board itself generated the stream *)
  let create_board_stream (s : Raw.t) =
    let source = { node = Entry (Board topo)
                 ; info = s.source.info } in
    let stream = { source
                 ; typ = s.typ
                 ; orig_id = s.id
                 ; id = make_id source } in
    `Done (React.S.const (Some stream)) in
  (* When board is connected directly to input *)
  let create_in_stream (s : Raw.t) (i : topo_input) =
    let source = { node = Entry (Input i)
                 ; info = s.source.info } in
    let stream = { source
                 ; typ = s.typ
                 ; orig_id = s.id
                 ; id = make_id source } in
    `Done (React.S.const (Some stream)) in
  (* When a board is connected to another board *)
  let find_cor_stream (s : Raw.t) (lst : Stream.t list React.signal) = (* use S.fmap `None*)
    let map prev = match prev.orig_id, s.id with
      | TS_raw, (TS_multi _ as id) ->
        Some { prev with orig_id = id; typ = s.typ }
      | id, sid ->
        if equal_container_id id sid
        then Some { prev with typ = s.typ }
        else None in
    let eq = Util_equal.Option.equal Stream.equal in
    let rec find_map = function
      | [] -> None
      | x :: l' ->
        match map x with
        | Some _ as res -> res
        | None -> find_map l' in
    `Done (React.S.map ~eq find_map lst) in
  (* When source of raw stream is another stream *)
  let compose_hier (s : Raw.t) (parent : container_id) sms =
    List.find_opt (fun ((stream : Raw.t), _) ->
        equal_container_id stream.id parent) sms
    |> function
    | None -> `Await s
    | Some (_, v) ->
      let eq = Util_equal.Option.equal Stream.equal in
      React.S.map ~eq (function
          | None -> None
          | Some p ->
            let source =
              { node = Stream p
              ; info = s.source.info
              } in
            let stream =
              { source
              ; id = make_id source
              ; typ = s.typ
              ; orig_id = s.id
              } in
            Some stream) v
      |> fun x -> `Done x in
  let transform acc (s : Raw.t) =
    match s.source.node with
    | Port i ->
      (try
         (match Map.find i ports with
          | `Input i -> create_in_stream s i
          | `Streams lst -> find_cor_stream s lst)
       with _ ->
         `Error (Printf.sprintf "merge_streams: port %d is not connected" i))
    | Stream id -> compose_hier s id acc
    | Board     -> create_board_stream s in
  let rec lookup acc await = function
    | [] -> cleanup acc await
    | x :: tl ->
      (match transform acc x with
       | `Done s -> lookup ((x, s) :: acc) await tl
       | `Await s -> lookup acc (s :: await) tl
       | `None -> lookup acc await tl
       | `Error e -> print_endline e; failwith e)
  and cleanup acc = function
    | [] -> acc
    | x :: tl ->
      (match transform acc x with
       | `Done s -> cleanup ((x, s) :: acc) tl
       | `None -> cleanup acc tl
       | `Error e -> print_endline e; failwith e
       | `Await s ->
         (* XXX What is this case for? *)
         try List.find (fun (p : Raw.t) ->
             match s.source.node with
             | Stream s -> equal_container_id p.id s
             | _ -> false)
             tl |> ignore; (* parent exists TODO: check it more thoroughly *)
           cleanup acc (tl @ [s])
         with _ -> cleanup acc tl) in
  let eq_lst = Util_equal.List.equal Stream.equal in
  let eq_opt = Util_equal.Option.equal Stream.equal in
  let eq =
    React.S.equal ~eq:eq_opt
    |> Util_equal.Pair.equal Raw.equal
    |> Util_equal.List.equal in
  React.(
    raw_streams
    |> S.map ~eq (lookup [] [])
    |> S.map ~eq:(Util_equal.List.equal @@ S.equal ~eq:eq_opt) (List.map snd)
    |> S.map ~eq:(S.equal ~eq:eq_lst) (S.merge ~eq:eq_lst (^::) [])
    |> S.switch ~eq:eq_lst)
