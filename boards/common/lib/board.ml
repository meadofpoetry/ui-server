open Containers
open Common.Topology
open Storage.Options
open Lwt.Infix

module Api_handler = Api.Handler.Make(Common.User)

type 'a cc = [`Continue of 'a]

module Ports = Map.Make(Int)

exception Invalid_port of string

type url = Common.Url.t
type set_state = [ `Forbidden
                 | `Limited   of int
                 | `Unlimited
                 ]
type set_error = [ `Not_in_range
                 | `Limit_exceeded of (int * int)
                 | `Forbidden
                 | `Internal_error of string
                 ] [@@deriving yojson]
type constraints =
  { range : (url * url) list
  ; state : set_state React.signal
  }

type stream_handler = < streams     : (url option * Common.Stream.t) list React.signal
                      ; set         : (url * Common.Stream.t) list -> (unit,set_error) Lwt_result.t
                      ; constraints : constraints
                      >

type t = { handlers        : (module Api_handler.HANDLER) list
         ; control         : int
         ; streams_signal  : Common.Stream.t list React.signal
         ; step            : (Cbuffer.t list -> 'c cc as 'c) cc
         ; connection      : state React.signal
         ; ports_active    : bool React.signal Ports.t
         ; settings_page   : (string * [`Div] Tyxml.Html.elt React.signal)
         ; widgets_page    : (string * [`Div] Tyxml.Html.elt React.signal) list
         ; stream_handler  : stream_handler option
         ; state           : < finalize : unit -> unit >
         }

module type BOARD = sig
  val create : topo_board ->
               Common.Stream.t list React.signal ->
               (Common.Stream.stream list React.signal -> topo_board -> Common.Stream.t list React.signal) ->
               (Cbuffer.t -> unit Lwt.t) ->
               Storage.Database.t ->
               path ->
               float -> t
end

let concat_acc acc recvd = match acc with
  | Some acc -> Cbuffer.append acc (Cbuffer.concat (List.rev recvd))
  | None     -> Cbuffer.concat (List.rev recvd)

let apply = function `Continue step -> step

module Map  = Map.Make(Int)

let get_streams (boards : t Map.t)
                (topo : topo_board)
    : Common.Stream.t list React.signal =
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
                  (raw_streams : Common.Stream.stream list React.signal)
                  (topo : topo_board)
    : Common.Stream.t list React.signal =
  let open React in
  let open Common.Stream in
  let open Option in
  let ports =
    List.fold_left (fun m port ->
        match port.child with
        | Input i -> Map.add port.port (`Input i) m
        | Board b -> try let b = Map.find b.control boards in
                         Map.add port.port (`Streams b.streams_signal) m
                     with _ -> m)
      Map.empty topo.ports
  in
  let create_in_stream (s : stream) i =
    `Done (S.const (Some { source = (Input i)
                         ; id     = s.id
                         ; description = s.description }))
  in
  (* let g = S.fmap (function None -> None | Some x -> Some (string_of_int x)) "" a;; *)
  let find_cor_stream (s : stream) lst = (* use S.fmap `None*)
    let initial = S.map (List.find_pred (fun n -> equal_stream_id n.id s.id)) lst in
    `Done (S.fmap (function None -> None | Some x -> Some (Some x)) None initial)
  in
  let compose_hier (s : stream) id sms =
    List.find_pred (fun ((stream : stream), _) -> equal_stream_id stream.id (`Ts id)) sms
    |> function None        -> `Await s
              | Some (_ ,v) ->
                 `Done (S.map (function
                              Some p -> Some ({ source = (Parent p)
                                              ; id     = s.id
                                              ; description = s.description })
                            | None -> None)
                          v)
  in
  let transform acc (s : stream) =
    match s.source with
    | Port i -> (Map.get i ports
                 |> function
                   | None                -> `Error (Printf.sprintf "merge_streams: port %d is not connected" i) 
                   | Some (`Input i)     -> create_in_stream s i
                   | Some (`Streams lst) -> find_cor_stream s lst)
    | Stream id -> compose_hier s id acc
  in
  let rec lookup acc await = function
    | []    -> cleanup acc await
    | x::tl -> 
       (match transform acc x with
        | `Done s  -> lookup ((x,s)::acc) await tl
        | `Await s -> lookup acc (s::await) tl
        | `None    -> lookup acc await tl
        | `Error e -> failwith e)
  and cleanup acc = function
    | []    -> acc
    | x::tl ->
       (match transform acc x with
        | `Done s  -> cleanup ((x,s)::acc) tl
        | `None    -> cleanup acc tl
        | `Error e -> failwith e
        | `Await s -> try List.find (fun (p : stream) ->
                              match s.source with
                              | Stream id -> equal_stream_id (`Ts id) p.id
                              | _ -> false)
                            tl |> ignore; (* parent exists TODO: check it more thoroughly *)
                          cleanup acc (List.append tl [s])
                      with _ -> cleanup acc tl)
  in
  raw_streams
  |> S.map ~eq:(Equal.physical) (lookup [] [])
  |> S.map ~eq:(Equal.physical) (List.map snd)
  |> S.map ~eq:(Equal.physical) (fun l -> S.merge ~eq:(Equal.physical) (fun acc x -> match x with None -> acc | Some x -> x::acc) [] l)
  |> S.switch ~eq:(Equal.physical)
