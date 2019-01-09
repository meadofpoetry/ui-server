open Containers
open Storage.Options
open Api.Template
open Common

module Api_handler = Api.Handler.Make(User)

type 'a cc = [`Continue of 'a]

module Ports = Map.Make(Int)

exception Invalid_port of string

exception Invalid_sources of string

type constraints =
  { range : (Stream.Table.url * Stream.Table.url) list
  ; state : Stream.Table.source_state React.signal
  }

type stream_handler =
  < streams : Stream.Table.stream list React.signal
  ; set : Stream.Table.setting list ->
          (unit, Stream.Table.set_error) Lwt_result.t
  ; constraints : constraints
  >

type t =
  { handlers : (module Api_handler.HANDLER) list
  ; control : int
  ; streams_signal : Stream.t list React.signal
  ; log_source : Stream.Log_message.source
  ; step : (Cstruct.t list -> 'c cc as 'c) cc
  ; connection : Topology.state React.signal
  ; ports_active : bool React.signal Ports.t
  ; ports_sync : bool React.signal Ports.t
  ; stream_handler : stream_handler option
  ; templates : upper ordered_item list User.user_table option
  ; state : < finalize : unit -> unit >
  }

module type BOARD = sig
  open React
  val create :
    Topology.topo_board ->
    Stream.t list signal ->
    (Stream.Raw.t list signal -> Topology.topo_board ->
     Stream.t list signal) ->
    (Cstruct.t -> unit Lwt.t) ->
    Storage.Database.t ->
    path ->
    float -> t
end

let log_name (b : Topology.topo_board) =
  Printf.sprintf "board.%s_%s.%d"
  b.manufacturer b.model b.control

let concat_acc acc recvd = match acc with
  | Some acc -> Cstruct.append acc (Cstruct.concat (List.rev recvd))
  | None -> Cstruct.concat (List.rev recvd)

let apply = function `Continue step -> step

module Map = Map.Make(Int)

let get_streams (boards : t Map.t)
                (topo : Topology.topo_board)
    : Stream.t list React.signal =
  let rec get_streams' acc = function
    | [] -> acc
    | (h : Topology.topo_port) :: tl ->
       begin match h.child with
       | Topology.Input _ -> get_streams' acc tl
       | Topology.Board b ->
          begin match Map.get b.control boards with
          | Some b ->
             let eq = Equal.list Stream.equal in
             get_streams' (React.S.l2 ~eq (@) b.streams_signal acc) tl
          | None -> get_streams' acc tl
          end
       end
  in
  get_streams' (React.S.const []) topo.ports

let merge_streams (boards : t Map.t)
      (raw_streams : Stream.Raw.t list React.signal)
      (topo : Topology.topo_board)
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
  let create_in_stream (s : Raw.t) (i:topo_input) =
    let source = { node = Entry (Input i)
                 ; info = s.source.info } in
    let stream = { source
                 ; typ = s.typ
                 ; orig_id = s.id
                 ; id = make_id source } in
    `Done (React.S.const (Some stream)) in
  (* When a board is connected to another board *)
  let find_cor_stream (s : Raw.t) (lst : Stream.t list React.signal) = (* use S.fmap `None*)
    let map (s : Raw.t) prev = match prev.orig_id, s.id with
      | TS_raw, (TS_multi _ as id) ->
         Some { prev with orig_id = id; typ = s.typ }
      | id, sid ->
         if equal_container_id id sid
         then Some { prev with typ = s.typ }
         else None in
    let eq = Equal.option Stream.equal in
    `Done (React.S.map ~eq (List.find_map (map s)) lst) in
  (* When source of raw stream is another stream *)
  let compose_hier (s : Raw.t) (parent : container_id) sms =
    List.find_pred (fun ((stream : Raw.t), _) ->
        equal_container_id stream.id parent) sms
    |> function
      | None -> `Await s
      | Some (_, v) ->
         let eq = Equal.option Stream.equal in
         React.S.map ~eq (function
             | None -> None
             | Some p ->
                let source = { node = Stream p
                             ; info = s.source.info } in
                let stream = { source
                             ; id = make_id source
                             ; typ = s.typ
                             ; orig_id = s.id } in
                Some stream) v
         |> fun x -> `Done x in
  let transform acc (s : Raw.t) =
    match s.source.node with
    | Port i ->
       (Map.get i ports
        |> function
          | None -> `Error (Printf.sprintf "merge_streams: \
                                            port %d is not connected" i)
          | Some (`Input i) -> create_in_stream s i
          | Some (`Streams lst) -> find_cor_stream s lst)
    | Stream id -> compose_hier s id acc
    | Board     -> create_board_stream s in
  let rec lookup acc await = function
    | [] -> cleanup acc await
    | x :: tl ->
       (match transform acc x with
        | `Done s -> lookup ((x, s) :: acc) await tl
        | `Await s -> lookup acc (s :: await) tl
        | `None -> lookup acc await tl
        | `Error e -> failwith e)
  and cleanup acc = function
    | []      -> acc
    | x :: tl ->
       (match transform acc x with
        | `Done s -> cleanup ((x, s) :: acc) tl
        | `None -> cleanup acc tl
        | `Error e -> failwith e
        | `Await s ->
           (* XXX What is this case for? *)
           try List.find (fun (p : Raw.t) ->
                   match s.source.node with
                   | Stream s -> equal_container_id p.id s
                   | _ -> false)
                 tl |> ignore; (* parent exists TODO: check it more thoroughly *)
               cleanup acc (tl @ [s])
           with _ -> cleanup acc tl)
  in
  let ( ^:: ) = Fun.flip List.cons_maybe in
  let eq_lst = Equal.list Stream.equal in
  let eq_opt = Equal.option Stream.equal in
  let eq =
    React.S.equal ~eq:eq_opt
    |> Equal.pair Raw.equal
    |> Equal.list in
  React.(
    raw_streams
    |> S.map ~eq (lookup [] [])
    |> S.map ~eq:(Equal.list @@ S.equal ~eq:eq_opt) (List.map snd)
    |> S.map ~eq:(S.equal ~eq:eq_lst) (S.merge ~eq:eq_lst (^::) [])
    |> S.switch ~eq:eq_lst)
