open Containers

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

  let init t        = { t with point = 0; timer = 0 }

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
