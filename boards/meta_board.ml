open Containers
open Common.Topology
open Storage.Options
open Lwt.Infix

(* TODO remove *)
let (=) = Pervasives.(=)
   
module Api_handler = Api.Handler.Make(Common.User)

type 'a cc = [`Continue of 'a]

module Ports = Map.Make(Int)

exception Invalid_port of string

type url = string
                        
type stream_handler = < streams : (url option * Common.Stream.t) list React.signal
                      ; set     : (url * Common.Stream.t) list -> (unit,string) Lwt_result.t
                      >
                        
type board = { handlers        : (module Api_handler.HANDLER) list
             ; control         : int
             ; streams_signal  : Common.Stream.t list React.signal
             ; step            : (Cbuffer.t list -> 'c cc as 'c) cc
             ; connection      : state React.signal
             ; ports_active    : bool React.signal Ports.t
             ; settings_page   : (string * [`Div] Tyxml.Html.elt React.signal)
             ; widgets_page    : (string * [`Div] Tyxml.Html.elt React.signal) list
             ; stream_handler  : stream_handler option
             ; state           : < >
             }

module type BOARD = sig
  val create       : topo_board ->
                     Common.Stream.t list React.signal ->
                     (Common.Stream.stream list React.signal -> topo_board -> Common.Stream.t list React.signal) ->
                     (Cbuffer.t -> unit Lwt.t) ->
                     Storage.Database.t ->
                     path ->
                     float -> board
end

module Msg = struct

  exception Timeout

  type ('a, 'b) msg = { send    : (unit -> unit Lwt.t)
                      ; pred    : ('a -> 'b option)
                      ; timeout : int
                      ; exn     : exn option
                      }

  module Pool = struct
    type ('a,'b) t = { timer   : int
                     ; point   : int
                     ; reqs    : ('a,'b) msg array
                     }
    let create lst    = { timer = 0; point = 0; reqs = Array.of_list lst }

    let append t msgs = { t with reqs = Array.append t.reqs msgs }

    let empty t       = Array.length t.reqs = 0

    let current t     = t.reqs.(t.point)

    let responsed t   = List.find_map (current t).pred

    let send t        = (current t).send

    let step t        =
      let tmr = succ t.timer in
      if tmr >= (current t).timeout
      then raise_notrace @@ Option.get_or ~default:Timeout (current t).exn
      else { t with timer = tmr }

    let next t        = { t with point = ((succ t.point) mod (Array.length t.reqs)); timer = 0 }

    let last t        = Int.equal t.point (Array.length t.reqs - 1)

    let map t f       = Array.map f t.reqs

    let iter t f      = Array.iter f t.reqs
  end

  module Queue = struct
    type ('a,'b) t = { timer   : int
                     ; reqs    : ('a,'b) msg CCFQueue.t
                     }
    let create lst    = { timer = 0; reqs = CCFQueue.of_list lst }

    let append t msg  = { t with reqs = CCFQueue.snoc t.reqs msg }

    let empty t       = CCFQueue.size t.reqs = 0

    let responsed t m = Option.(CCFQueue.first t.reqs >>= fun head -> List.find_map head.pred m)

    let send t ()     = try (CCFQueue.first_exn t.reqs).send () with _ -> Lwt.return_unit

    let step t        = (match CCFQueue.first t.reqs with
                         | Some head -> let tmr = succ t.timer in
                                        if tmr >= head.timeout
                                        then raise_notrace @@ Option.get_or ~default:Timeout head.exn
                                        else { t with timer = tmr }
                         | None      -> t)

    let next t        = { timer = 0; reqs = CCFQueue.tail t.reqs }

    let map t f       = CCFQueue.map f t.reqs

    let iter t f      = CCFQueue.iter f t.reqs
  end

  module Await_queue = struct
    type ('a, 'b) t    = { reqs    : ('a,'b) msg CCFQueue.t
                         ; pending : (int * ('a,'b) msg) list
                         }
    let create lst     = { reqs = CCFQueue.of_list lst; pending = [] }

    let append t msg   = { t with reqs = CCFQueue.snoc t.reqs msg }

    let empty t        = CCFQueue.size t.reqs = 0

    let has_pending t  = not @@ List.is_empty t.pending

    let responsed t m  =
      let open Option.Infix in
      let pending, responses =
        List.partition_map (fun (timer, req) ->
            List.find_map req.pred m
            |> function
              (* No response -> retain request *)
              | None -> `Left (timer, req)
              (* Responded -> drop request and return response *)
              | Some resp -> `Right resp)
          t.pending
      in { t with pending }, responses

    let send t () =
      try
        let msg, reqs = CCFQueue.take_front_exn t.reqs in
        { reqs; pending = (0 , msg) :: t.pending }, msg.send ()
      with _ -> t, Lwt.return_unit

    let step t =
      let tout, pending = List.partition_map
                            (fun (timer, msg) ->
                              if timer > msg.timeout
                              then `Left msg
                              else `Right (succ timer, msg))
                            t.pending
      in { t with pending }, tout

    let iter t f = List.iter f t.pending

    let map t f = { t with pending = List.map f t.pending }
         
  end

end

let concat_acc acc recvd = match acc with
  | Some acc -> Cbuffer.append acc (Cbuffer.concat (List.rev recvd))
  | None     -> Cbuffer.concat (List.rev recvd)

let apply = function `Continue step -> step

module Map  = Map.Make(Int)

let get_streams (boards : board Map.t)
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

let merge_streams (boards : board Map.t)
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
    let initial = S.map (List.find_pred (fun n -> n.id = s.id)) lst in
    `Done (S.fmap (function None -> None | Some x -> Some (Some x)) None initial)
  in
  let compose_hier (s : stream) id sms =
    List.find_pred (fun ((stream : stream), _) -> stream.id = `Ts id) sms
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
                              | Stream id -> (`Ts id) = p.id
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
