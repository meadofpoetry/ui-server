open Lwt_zmq
open Lwt_react
open Lwt.Infix
open Containers

let (%) = CCFun.(%)
(* TODO make 'active type label *)        
type state = { ctx         : ZMQ.Context.t
             ; msg         : [ `Req] ZMQ.Socket.t
             ; ev          : [ `Sub] ZMQ.Socket.t
             ; sock_events : unit event
             }

type get = Get_not_used
type set = Set_not_used
type (_,_) req =
  | Get_structures : (get, Structure.t list) req
  | Get_settings   : (get, Settings.t) req
  | Get_graph      : (get, Graph.t) req
  | Get_wm         : (get, Wm.t) req
  | Set_structures : Structure.t list -> (set, unit) req
  | Set_settings   : Settings.t -> (set, unit) req
  | Set_graph      : Graph.t -> (set, unit) req
  | Set_wm         : Wm.t -> (set, unit) req
                   
type api = { structure : Structure.t list signal
           ; settings  : Settings.t event
           ; graph     : Graph.t event
           ; wm        : Wm.t event
           ; vdata     : Video_data.t event (* TODO to be split by purpose later *)
           ; get       : 'a.(get, 'a) req -> 'a Lwt.t
           ; set       : (set, unit) req -> unit Lwt.t
           }

let split_events () =
  let _ = Lwt_io.printf "split_events\n" |> ignore in 
  let events, epush     = E.create () in
  let strm, strm_push   = S.create [] in
  let sets, sets_push   = E.create () in
  let grap, grap_push   = E.create () in
  let wm  , wm_push     = E.create () in
  let vdata, vdata_push = E.create () in
  let (<$>) f result =
    match result with
    | Ok r -> (f r : unit)
    | Error e -> failwith @@ Printf.sprintf "parse error %s\n" e in
  let split = function
    | `Assoc [("streams", tl)]    -> strm_push  <$> Structure.structure_list_of_yojson tl
    | `Assoc [("settings", tl)]   -> sets_push  <$> Settings.of_yojson tl
    | `Assoc [("graph", tl)]      -> grap_push  <$> Graph.of_yojson tl
    | `Assoc [("wm", tl)]         -> wm_push    <$> Wm.of_yojson tl
    | `Assoc [("video_data", tl)] -> vdata_push <$> Video_data.of_yojson tl
    | s -> Yojson.Safe.pretty_to_string s
           |> prerr_endline
  in
  let events = E.map split events in
  events, epush, strm, sets, grap, wm, vdata

let get (type a) sock (conv : Msg_conv.converter) (s : Structure.t list signal) (req : (get, a) req) : a Lwt.t =
  let find s kv =
    List.find_map (fun (k,v) -> if k = s then Some v else None) kv
    |> function None -> Error "key not found" | Some v -> Ok v
  in
  let send s  = Socket.send sock (conv.to_string (`Assoc ["get", `List [`String s]])) in
  let get' s decode =
    send s >>= fun () ->
    Socket.recv sock >>= fun js ->
    match conv.of_string js with
    | `Assoc ["ok", `Assoc kvs] ->
       Result.(find s kvs >>= decode)
       |> (function Ok v ->  Lwt.return v | Error e -> Lwt.fail_with e)
    | `Assoc ["error", `String msg] -> Lwt.fail_with msg
    | _ -> Lwt.fail_with ("unknown resp: " ^ js)
  in
  match req with
  | Get_structures -> Lwt.return (React.S.value s) (* TODO fixlater *)
  | Get_settings   -> get' "settings" Settings.of_yojson
  | Get_graph      -> get' "graph" Graph.of_yojson
  | Get_wm         -> get' "wm" Wm.of_yojson
  
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
       
let create sock_in sock_out converter hardware_streams =
  let ctx = ZMQ.Context.create () in
  let msg = ZMQ.Socket.create ctx ZMQ.Socket.req in
  let ev  = ZMQ.Socket.create ctx ZMQ.Socket.sub in

  ZMQ.Socket.connect msg sock_in;
  ZMQ.Socket.connect ev sock_out;
  ZMQ.Socket.subscribe ev "";

  let msg_sock = Socket.of_socket msg in
  let ev_sock  = Socket.of_socket ev in

  let sock_events, epush,
      structure', settings,
      graph, wm,
      vdata = split_events ()
  in
  let structure = S.l2 Structure_conv.match_streams hardware_streams structure' in
  let set = set msg_sock converter in
  let get = fun x -> get msg_sock converter structure x in
  let api = {set; get; structure;
             settings; graph;
             wm; vdata;} in
  let state = { ctx; msg; ev; sock_events } in
  let recv () =
    Socket.recv ev_sock
    >>= fun msg ->
    Lwt.return @@ epush (converter.of_string msg)
  in
  api, state, recv

let finalize state =
  ZMQ.Socket.unsubscribe state.ev "";
  ZMQ.Socket.close       state.ev;
  ZMQ.Socket.close       state.msg;
  ZMQ.Context.terminate  state.ctx
