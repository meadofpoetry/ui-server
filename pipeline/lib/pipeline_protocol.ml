open Containers
open Lwt_zmq
open Lwt_react
open Lwt.Infix
open Message
open Notif

let (%) = Fun.(%)
(* TODO make 'active type label *)        
type state = { ctx         : ZMQ.Context.t
             ; msg         : [ `Req] ZMQ.Socket.t
             ; ev          : [ `Sub] ZMQ.Socket.t
             ; sock_events : unit event
                                  (*; _e : unit event*)
             }

type channels =
  { wm        : Wm.t channel
  ; streams   : Structure.t list channel
  }
  
type api = { streams   : Structure.t list signal
           ; settings  : Settings.t option signal
           ; graph     : Graph.t event
           ; wm        : Wm.t option signal
           ; vdata     : Video_data.t event (* TODO to be split by purpose later *)
           ; adata     : Audio_data.t event
           ; requests  : channels
           }

module Wm_notif = Notif.Make(Wm)
module Streams_notif = Notif.Make(Streams)
module Settings_notif = Notif.Make(Settings)
module Graph_notif = Notif.Make(Graph)
module Video_data_notif = Notif.Make(Video_data)
module Audio_data_notif = Notif.Make(Audio_data)

let notif_events typ =
  let _ = Lwt_io.printf "split_events\n" |> ignore in 
  let events, epush     = E.create () in
  let strm, strm_push   = S.create [] in
  let sets, sets_push   = S.create None in
  let grap, grap_push   = E.create () in
  let wm  , wm_push     = S.create None in
  let vdata, vdata_push = E.create () in
  let adata, adata_push = E.create () in  
  let (<$>) f result =
    match result with
    | Ok r -> (f r : unit)
    | Error e -> Lwt_io.printf "parse error %s\n" e |> ignore in

  let table = Hashtbl.create 10 in
  List.iter (fun (n,f) -> Hashtbl.add table n f)
    [ Wm_notif.create `Json ((<$>) (fun x -> wm_push (Some x)))
    ; Streams_notif.create `Json ((<$>) strm_push)
    ; Settings_notif.create `Json ((<$>) (fun x -> sets_push (Some x)))
    ; Graph_notif.create `Json ((<$>) grap_push)
    ; Video_data_notif.create `Json ((<$>) vdata_push)
    ; Audio_data_notif.create `Json ((<$>) adata_push)
    ];

  let dispatch = dispatch_js table in
  let events = E.map dispatch events in
  events, epush,
  strm, sets, grap, wm, vdata, adata,
  strm_push, wm_push

module Wm_msg = Message.Make(Wm)
module Structure_msg = Message.Make(struct
                           type t = Structure.structure_list
                           let name = "streams"
                           let to_yojson = Structure.structure_list_to_yojson
                           let of_yojson = Structure.structure_list_of_yojson
                         end)

let create_channels
      typ
      send
      (trans : Structure.structure list -> Structure.t list)
      (s_push : Structure.structure list -> unit)
      (wm_push : Wm.t option -> unit) =
  let wm_get, wm_set = Wm_msg.create send `Json in
  let s_get, s_set = Structure_msg.create send `Json in
  let wm_get_upd () =
    wm_get () >|= function 
    | Error _ as r -> r
    | Ok v    as r -> wm_push (Some v); r
  in
  let s_get_upd () =
    s_get () >|= function
    | Error _ as r -> r
    | Ok v  -> s_push v; Ok(trans v)
  in
  let s_set_upd s =
    Structure.unwrap s
    |> s_set
  in
  { wm = { get = wm_get_upd; set = wm_set }
  ; streams = { get = s_get_upd; set = s_set_upd }
  }
  
  
let create sock_in sock_out hardware_streams =
  let ctx = ZMQ.Context.create () in
  let msg = ZMQ.Socket.create ctx ZMQ.Socket.req in
  let ev  = ZMQ.Socket.create ctx ZMQ.Socket.sub in

  ZMQ.Socket.connect msg sock_in;
  ZMQ.Socket.connect ev sock_out;
  ZMQ.Socket.subscribe ev "";

  let msg_sock = Socket.of_socket msg in
  let ev_sock  = Socket.of_socket ev in
  
  let sock_events, epush,
      streams', settings,
      graph, wm,
      vdata, adata,
      strms_push, wm_push = notif_events ()
  in
  let streams = S.l2
                  (Structure_conv.match_streams Common.Topology.(Some { input = TSOIP; id = 42 }))
                  hardware_streams streams'
  in
  let send_js = fun x ->
    Socket.send msg_sock (Yojson.Safe.to_string x) >>= fun () ->
    Socket.recv msg_sock >|= fun js ->
    Yojson.Safe.from_string js
  in
  let merge = fun s -> (Structure_conv.match_streams Common.Topology.(Some { input = TSOIP; id = 42 })) (S.value hardware_streams) s in
  let requests = create_channels `Json send_js merge strms_push wm_push in
  (*let set = set msg_sock converter in
  let get = fun x -> get msg_sock converter structure wm settings x in *)
  let api = { streams
            ; settings; graph; wm; vdata; adata
            ; requests
            } in
  (*set (Set_settings Settings.default) |> ignore;*)
  (*let _e  = Lwt_react.E.map_p (fun x ->
                Yojson.Safe.pretty_to_string x
                |> Lwt_io.printf "Video_data: %s\n") msgs in*)
  let state = { ctx; msg; ev; sock_events;} in
  let recv () =
    Socket.recv ev_sock
    >>= fun msg ->
    Lwt.return @@ epush (Yojson.Safe.from_string msg)
  in
  api, state, recv

  
let finalize state =
  ZMQ.Socket.unsubscribe state.ev "";
  ZMQ.Socket.close       state.ev;
  ZMQ.Socket.close       state.msg;
  ZMQ.Context.terminate  state.ctx


                         (*
    let get (type a) sock
      (conv : Msg_conv.converter)
      (s : Structure.t list signal)
      (wm : Wm.t option signal)
      (st : Settings.t option signal)
      (req : (get, a) req) : a Lwt.t =
  let find s kv =
    List.find_map (fun (k,v) -> if String.equal k s then Some v else None) kv
    |> function None -> Error "key not found" | Some v -> Ok v
  in
  let send s  = Socket.send sock (conv.to_string (`Assoc ["get", `List [`String s]])) in
  let get' s decode =
    send s >>= fun () ->
    Socket.recv sock
    >>= fun js ->
    match conv.of_string js with
    | `Assoc ["ok", `Assoc kvs] ->
       (* Lwt_io.printf  "Pipeline: request decode: %s\n" (Yojson.Safe.pretty_to_string (`Assoc kvs)) |> ignore;*)
       Result.(find s kvs >>= decode)
       |> (function Ok v -> Lwt.return v
                  | Error e -> Lwt_io.printf  "Pipeline: request decode failure: %s\n" e >>= fun () -> Lwt.fail_with e)
    | `Assoc ["error", `String msg] ->
       Lwt_io.printf "Pipeline: request failure: %s\n" msg >>= fun () ->
       Lwt.fail_with msg
    | _ ->
       Lwt_io.printf "Pipeline: unknown failure: %s\n" js >>= fun () ->
       Lwt.fail_with ("unknown resp: " ^ js)
  in
  let unpack_opt = function
    | None -> Lwt.fail_with "no data"
    | Some v -> Lwt.return v
  in
  match req with
  | Get_structures ->
     Lwt.return (React.S.value s) (* TODO fixlater *)
  | Get_settings   ->
     Lwt.catch
       (fun () -> get' "settings" Settings.of_yojson)
       (fun _  -> unpack_opt (React.S.value st))
  | Get_graph      -> get' "graph" Graph.of_yojson
  | Get_wm         ->
     Lwt.catch
       (fun () -> get' "wm" Wm.of_yojson)
       (fun _  -> unpack_opt (React.S.value wm))
    
let set (type a) sock (conv : Msg_conv.converter) (req : (set, a) req) : unit Lwt.t =
  let set' s encode v =
    Socket.send sock (conv.to_string (`Assoc ["set", `Assoc [s, (encode v)]]))
    >>= fun () -> Socket.recv sock
    >>= Lwt_io.printf "Send result: %s\n"
  in
  match req with
  | Set_structures x -> set' "streams" Structure.structure_list_to_yojson
                          (List.map Structure.(fun s -> s.structure) x)
  | Set_settings   x -> set' "settings" Settings.to_yojson x
  | Set_graph      x -> set' "graph" Graph.to_yojson x
  | Set_wm         x -> set' "wm" Wm.to_yojson x
                          *)
