open Containers
open Common
open Common.Topology
open Storage.Options
open Lwt.Infix
open Api.Template
open Common.User

module Api_handler = Api.Handler.Make(User)

type 'a cc = [`Continue of 'a]

module Ports = Map.Make(Int)

exception Invalid_port of string

exception Invalid_sources of string

type url = Url.t
type set_state =
  [ `Forbidden
  | `Limited   of int
  | `Unlimited
  ]
type set_error =
  [ `Not_in_range
  | `Limit_exceeded of (int * int)
  | `Forbidden
  | `Internal_error of string
  ] [@@deriving yojson]
type constraints =
  { range : (url * url) list
  ; state : set_state React.signal
  }

type stream_handler =
  < streams     : (url option * Stream.t) list React.signal
  ; set         : (url * Stream.t) list -> (unit,set_error) Lwt_result.t
  ; constraints : constraints
                  >

type t =
  { handlers        : (module Api_handler.HANDLER) list
  ; control         : int
  ; streams_signal  : Common.Stream.t list React.signal
  ; step            : (Cstruct.t list -> 'c cc as 'c) cc
  ; connection      : state React.signal
  ; ports_active    : bool React.signal Ports.t
  ; ports_sync      : bool React.signal Ports.t
  ; stream_handler  : stream_handler option
  ; templates       : upper ordered_item list user_table option
  ; state           : < finalize : unit -> unit >
  }

module type BOARD = sig
  open React
  val create :
    topo_board ->
    Stream.t list signal ->
    (Stream.Raw.t list signal -> topo_board -> Stream.t list signal) ->
    (Cstruct.t -> unit Lwt.t) ->
    Storage.Database.t ->
    path ->
    float -> t
end

let log_name (b:topo_board) =
  Printf.sprintf "board.%s_%s.%d"
  b.manufacturer b.model b.control

let concat_acc acc recvd = match acc with
  | Some acc -> Cstruct.append acc (Cstruct.concat (List.rev recvd))
  | None     -> Cstruct.concat (List.rev recvd)

let apply = function `Continue step -> step

module Map  = Map.Make(Int)

let get_streams (boards : t Map.t)
                (topo : topo_board)
    : Stream.t list React.signal =
  let rec get_streams' acc = function
    | []    -> acc
    | h::tl -> match h.child with
               | Input i -> get_streams' acc tl
               | Board b -> match Map.get b.control boards with
                            | Some b -> get_streams' (React.S.l2 (@) b.streams_signal acc) tl
                            | None   -> get_streams' acc tl
  in
  get_streams' (React.S.const []) topo.ports

let merge_streams (boards : t Map.t)
                  (raw_streams : Stream.Raw.t list React.signal)
                  (topo : topo_board)
    : Stream.t list React.signal =
  let open React in
  let open Stream in
  let open Option in
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
    `Done (S.const (Some { source
                         ; typ     = s.typ
                         ; orig_id = s.id
                         ; id      = make_id source })) in
  (* When board is connected directly to input *)
  let create_in_stream (s : Raw.t) (i:topo_input) =
    let source = { node = Entry (Input i)
                 ; info = s.source.info } in
    `Done (S.const (Some { source
                         ; typ     = s.typ
                         ; orig_id = s.id
                         ; id      = make_id source })) in
  (* When a board is connected to another board *)
  let find_cor_stream (s : Raw.t) (lst : Stream.t list signal) = (* use S.fmap `None*)
    let map (s : Raw.t) prev = match prev.orig_id, s.id with
      | TS_raw, (TS_multi _ as id) ->
         Some { prev with orig_id = id
                        ; typ = s.typ }
      | id, sid ->
         if equal_container_id id sid
         then Some { prev with typ = s.typ }
         else None in
    `Done (S.map (List.find_map (map s)) lst) in
  (* When source of raw stream is another stream *)
  let compose_hier (s : Raw.t) (parent : container_id) sms =
    List.find_pred (fun ((stream : Raw.t), _) ->
        equal_container_id stream.id parent) sms
    |> function
      | None        -> `Await s
      | Some (_, v) ->
         S.map (function
             | None   -> None
             | Some p ->
                let source = { node = Stream p
                             ; info = s.source.info } in
                Some ({ source
                      ; id      = make_id source
                      ; typ     = s.typ
                      ; orig_id = s.id })) v
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
    | []      -> cleanup acc await
    | x :: tl ->
       (match transform acc x with
        | `Done s  -> lookup ((x, s) :: acc) await tl
        | `Await s -> lookup acc (s :: await) tl
        | `None    -> lookup acc await tl
        | `Error e -> failwith e)
  and cleanup acc = function
    | []      -> acc
    | x :: tl ->
       (match transform acc x with
        | `Done s  -> cleanup ((x, s) :: acc) tl
        | `None    -> cleanup acc tl
        | `Error e -> failwith e
        | `Await s ->
           (* XXX What is this case for? *)
           try List.find (fun (p : Raw.t) ->
                   match s.source.node with
                   | Stream s -> equal_container_id p.id s
                   | _        -> false)
                 tl |> ignore; (* parent exists TODO: check it more thoroughly *)
               cleanup acc (tl @ [s])
           with _ -> cleanup acc tl)
  in
  let eq = Equal.physical in
  raw_streams
  |> S.map ~eq (lookup [] [])
  |> S.map ~eq (List.map snd)
  |> S.map ~eq (S.merge ~eq (fun acc -> function
                    | None   -> acc
                    | Some x -> x :: acc) [])
  |> S.switch ~eq
