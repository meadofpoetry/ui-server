open Board_types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Meta_board
open Meta_board.Msg

include Board_parser

(* Board protocol implementation *)

let to_period x step_duration = x * int_of_float (1. /. step_duration)

module SM = struct

  type events = { status : status React.event }

  type push_events = { status : status -> unit }

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_board_info -> Get_board_info.to_cbuffer ())
    |> sender

  let send_instant (type a) sender (msg : a instant_request) : unit Lwt.t =
    (match msg with
     | Set_factory_mode x -> Set_factory_mode.to_cbuffer x
     | Set_board_mode x   -> Set_board_mode.to_cbuffer x)
    |> sender

  let enqueue_instant (type a) msgs sender (msg : a instant_request) : unit Lwt.t =
    let t,w = Lwt.wait () in
    let send = fun () -> (send_instant sender msg) >>= (fun x -> Lwt.return @@ Lwt.wakeup w x) in
    let pred = fun _  -> None in
    msgs := Queue.append !msgs { send; pred; timeout = 0; exn = None };
    t

  let step msgs imsgs sender (storage : config storage) step_duration push_state push_events =
    let period = to_period 5 step_duration in

    let wakeup_timeout (_,t) = t.pred `Timeout |> ignore in
    let events_push info = function
      | `Status x -> let status = (* match info.packers_num,x.data with
                        * | Some n,General g -> { x with data = General (CCList.take n g) }
                        * | None, General g  -> { x with data = General [] }
                        * | _                -> x *) x
                     in
                     push_events.status status in

    let rec first_step () =
      Await_queue.iter !msgs wakeup_timeout;
      msgs := Await_queue.create [];
      imsgs := Queue.create [];
      push_state `No_response;
      send_msg sender Get_board_info |> ignore;
      `Continue (step_detect period None)

    and step_detect p acc recvd =
      let _,rsps,acc = deserialize (Meta_board.concat_acc acc recvd) in
      match CCList.find_map (is_response Get_board_info) rsps with
      | Some r -> push_state `Init;
                  let config = storage#get in
                  send_instant sender (Set_factory_mode config.factory_mode) |> ignore;
                  send_instant sender (Set_board_mode config.board_mode) |> ignore;
                  `Continue (step_normal_idle r period None)
      | None -> if p < 0 then first_step ()
                else `Continue (step_detect (pred p) acc)

    and step_normal_idle info p acc recvd =
      let events,rsps,acc = deserialize (Meta_board.concat_acc acc recvd) in
      if CCOpt.is_none @@ CCList.find_map (is_response Get_board_info) rsps
      then (Queue.send !imsgs () |> ignore;
            imsgs := Queue.next !imsgs;
            match events with
            | [] -> if p < 0 then first_step ()
                    else `Continue (step_normal_idle info (pred p) acc)
            | l  -> push_state `Fine;
                    List.iter (events_push info) l;
                    `Continue (step_normal_idle info period acc))
      else first_step ()

    in first_step ()

  let create sender (storage : config storage) push_state step_duration =
    let msgs  = ref (Await_queue.create []) in
    let imsgs = ref (Queue.create []) in
    let status,status_push = React.E.create () in
    let (events : events) = { status = status } in
    let push_events = { status = status_push } in
    let api = { set_mode = (fun m -> enqueue_instant imsgs sender (Set_board_mode m))} in
    events,
    api,
    (step msgs imsgs sender storage step_duration push_state push_events)

end
