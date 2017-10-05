open Common.Hardware
open Storage.Options
open Lwt.Infix

module Api_handler = Api.Handler.Make(Common.User)

type 'a cc = [`Continue of 'a]

type _ request
                                        
module Streams = CCMap.Make(CCInt)
module Ports = CCMap.Make(CCInt)

type board = { handlers        : (module Api_handler.HANDLER) list
             ; control         : int
             ; streams_signal  : string Streams.t React.signal option
             ; step            : (Cbuffer.t list -> 'c cc as 'c) cc
             ; is_converter    : bool
             ; connection      : state React.signal
             ; ports_active    : bool React.signal Ports.t
             ; state           : < >
             } 
           
module type BOARD = sig
  type _ request
  val create       : topo_board -> (Cbuffer.t -> unit Lwt.t) -> Storage.Database.t -> path -> float -> board
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
    let create lst    = { timer = 0; point = 0; reqs = CCArray.of_list lst }
                      
    let append t msgs = { t with reqs = CCArray.append t.reqs msgs }
                      
    let empty t       = CCArray.length t.reqs = 0
                      
    let current t     = t.reqs.(t.point)
                      
    let responsed t   = CCList.find_map (current t).pred
                      
    let send t        = (current t).send
                      
    let step t        =
      let tmr = succ t.timer in
      if tmr >= (current t).timeout
      then raise_notrace @@ CCOpt.get_or ~default:Timeout (current t).exn
      else { t with timer = tmr }
      
    let next t        = { t with point = ((succ t.point) mod (Array.length t.reqs)); timer = 0 }
                      
    let last t        = CCInt.equal t.point (CCArray.length t.reqs - 1)

    let map t f       = CCArray.map f t.reqs
                      
    let iter t f      = CCArray.iter f t.reqs
  end

  module Queue = struct
    type ('a,'b) t = { timer   : int
                     ; reqs    : ('a,'b) msg CCFQueue.t
                     }
    let create lst    = { timer = 0; reqs = CCFQueue.of_list lst }
                      
    let append t msg  = { t with reqs = CCFQueue.snoc t.reqs msg }
                      
    let empty t       = CCFQueue.size t.reqs = 0
                      
    let responsed t m = CCOpt.(CCFQueue.first t.reqs >>= fun head -> CCList.find_map head.pred m)
                      
    let send t ()     = try (CCFQueue.first_exn t.reqs).send () with _ -> Lwt.return_unit
                                                                        
    let step t        = (match CCFQueue.first t.reqs with
                         | Some head -> let tmr = succ t.timer in
                                        if tmr >= head.timeout
                                        then raise_notrace @@ CCOpt.get_or ~default:Timeout head.exn
                                        else { t with timer = tmr }
                         | None      -> t)
                      
    let next t        = { timer = 0; reqs = CCFQueue.tail t.reqs }
                      
    let map t f       = CCFQueue.map f t.reqs
                      
    let iter t f      = CCFQueue.iter f t.reqs
  end

  module Await_queue = struct
    type ('a, 'b) t = { reqs    : ('a,'b) msg CCFQueue.t
                      ; pending : (int * ('a,'b) msg) list
                      }
    let create lst     = { reqs = CCFQueue.of_list lst; pending = [] }
                       
    let append t msg   = { t with reqs = CCFQueue.snoc t.reqs msg }

    let empty t        = CCFQueue.size t.reqs = 0
                       
    let responsed t m  =
      let open CCOpt.Infix in
      let pending, responses =
        CCList.partition_map (fun (timer, req) ->
            CCList.find_map req.pred m
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
      let tout, pending = CCList.partition_map
                            (fun (timer, msg) ->
                              if timer > msg.timeout
                              then `Left msg
                              else `Right (succ timer, msg))
                            t.pending
      in { t with pending }, tout

    let iter t f = CCList.iter f t.pending

    let map t f = { t with pending = CCList.map f t.pending }
         
  end

end

let concat_acc acc recvd = match acc with
  | Some acc -> Cbuffer.append acc (Cbuffer.concat (List.rev recvd))
  | None     -> Cbuffer.concat (List.rev recvd)

let apply = function `Continue step -> step
