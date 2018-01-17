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

  type push_events = { status : status -> unit }

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_board_info -> Get_board_info.to_cbuffer ())
    |> sender

  let send_instant (type a) sender (msg : a instant_request) : unit Lwt.t =
    (match msg with
     | Set_factory_mode x    -> Set_factory_mode.to_cbuffer x
     | Set_board_mode (nw,p) -> Set_board_mode.to_cbuffer (nw,p))
    |> sender

  let enqueue_instant (type a) msgs sender storage (msg : a instant_request) : unit Lwt.t =
    let t,w = Lwt.wait () in
    let send = fun () -> (send_instant sender msg) >>= (fun x -> Lwt.return @@ Lwt.wakeup w x) in
    let pred = fun _  -> None in
    let conf = storage#get in
    let _    = match msg with
      | Set_board_mode (nw,streams) -> storage#store ({ conf with nw_mode = nw; streams }:config)
      | _                           -> ()
    in
    msgs := Queue.append !msgs { send; pred; timeout = 0; exn = None };
    t

  let step msgs imsgs sender (storage : config storage) step_duration push_state push_events push_info =
    let period = to_period 5 step_duration in

    let wakeup_timeout (_,t) = t.pred `Timeout |> ignore in
    let events_push info = function
      | `Status x -> let status = match info.packers_num,x.data with
                       | Some n,General g -> { x with data = General (CCList.take n g) }
                       | None, General g  -> { x with data = General [] }
                       | _                -> x
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
                  push_info @@ Some r;
                  let config = storage#get in
                  send_instant sender (Set_factory_mode config.factory_mode) |> ignore;
                  send_instant sender (Set_board_mode (config.nw_mode,config.streams)) |> ignore;
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

  let stream_settings_to_packer_settings s_info convert (streams:stream_setting list) =
    match CCOpt.(React.S.value s_info >>= (fun x -> x.packers_num)) with
    | None   -> Error "Undetermined number of available packers"
    | Some n ->
       let streams = CCList.filter (fun (s:'a) -> match s.stream.id with `Ts _ -> true | _ -> false) streams in
       let len = CCList.length streams in
       if len > n
       then Error (Printf.sprintf "Can't set so many packers. Available: %d, got: %d" n len)
       else
         let rec pack acc = function
           | []    -> acc
           | h::tl ->
              match convert h with
              | Some pkr -> pack (pkr :: acc) tl
              | None     -> pack acc tl
         in
         Ok (pack [] streams)

  let streams_to_stream_settings (streams:Common.Stream.t list) =
    let succ_mcast addr = Int32.add (Ipaddr.V4.to_int32 addr) 1l |> Ipaddr.V4.of_int32 in
    let rec pack addr port acc = function
      | []         -> acc
      | stream::tl -> let dst_ip   = succ_mcast addr in
                      let dst_port = succ port in
                      let s        = { dst_ip; dst_port; enabled = true; stream } in
                      pack dst_ip dst_port (s :: acc) tl
    in
    pack (Ipaddr.V4.make 224 1 2 2) 1234 [] streams

  let create sender (storage : config storage) push_state step_duration convert =
    let s_devinfo,s_devinfo_push = React.S.create None in
    let s_config, s_config_push  = React.S.create storage#get in
    let msgs  = ref (Await_queue.create []) in
    let imsgs = ref (Queue.create []) in
    let status,status_push = React.E.create () in
    let (events : events) = { status = status
                            ; config = React.S.changes @@ React.S.map config_to_config_response s_config
                            } in
    let push_events = { status = status_push } in
    let api = { devinfo            = (fun () -> Lwt.return @@ React.S.value s_devinfo)
              ; set_factory_mode   = (fun (x : factory_settings) ->
                enqueue_instant imsgs sender storage (Set_factory_mode x))
              ; set_mode           = (fun (x:nw_settings) ->
                let ss = storage#get.streams in
                enqueue_instant imsgs sender storage (Set_board_mode (x,ss))
                >>= (fun _ -> s_config_push storage#get; Lwt.return_unit))
              ; set_streams_simple = (fun (x:Common.Stream.t list) ->
                let ss = streams_to_stream_settings x in
                match stream_settings_to_packer_settings s_devinfo convert ss with
                | Ok x    -> let nw = storage#get.nw_mode in
                             enqueue_instant imsgs sender storage (Set_board_mode (nw,x))
                             >>= (fun _ -> s_config_push storage#get; Lwt.return_ok ())
                | Error e -> Lwt.return_error e)
              ; set_streams_full   = (fun (x:stream_setting list) ->
                match stream_settings_to_packer_settings s_devinfo convert x with
                | Ok x    -> let nw = storage#get.nw_mode in
                             enqueue_instant imsgs sender storage (Set_board_mode (nw,x))
                             >>= (fun _ -> s_config_push storage#get; Lwt.return_ok ())
                | Error e -> Lwt.return_error e)
              ; config = (fun () -> Lwt.return @@ config_to_config_response storage#get)
              }
    in
    events,
    api,
    (step msgs imsgs sender storage step_duration push_state push_events s_devinfo_push)

end
