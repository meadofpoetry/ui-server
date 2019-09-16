open Js_of_ocaml
open Js_of_ocaml.WebSockets
open Js_of_ocaml_lwt

let ( >>= ) = Lwt.bind

type 'a t =
  { socket : WebSockets.webSocket Js.t
  ; subscribers : (int, int ref * 'a React.event * ('a -> unit)) Hashtbl.t
  ; control : (int * 'a Api.ws_message) Lwt_stream.t }

include Websocket_intf

let close_socket ?code ?reason (t : 'a t) =
  match code, reason with
  | None, None -> (t.socket)##close
  | Some code, None -> (t.socket)##close_withCode code
  | None, Some reason -> (t.socket)##close_withCodeAndReason 1000 (Js.string reason)
  | Some code, Some reason -> (t.socket)##close_withCodeAndReason code (Js.string reason)

module Make (Body : Api.BODY) (Msg : Api.WS_BODY with type t := Body.t) = struct
  open Netlib

  type nonrec t = Body.t t

  type 'a req =
    | Subscribe : int * Uri.t -> int req
    | Unsubscribe : int * int -> unit req

  let request (type a) ?(timeout = 2.) socket (req : a req) =
    let id, msg =
      match req with
      | Subscribe (id, uri) -> id, `Subscribe (Uri.to_string uri)
      | Unsubscribe (id, subid) -> id, `Unsubscribe subid
    in
    (socket.socket)##send (Js.string @@ Body.to_string @@ Msg.compose id msg);
    let rec loop () =
      Lwt_stream.next socket.control
      >>= fun resp ->
      match req, resp with
      | Subscribe _, (id', `Subscribed subid) when id = id' ->
          (Lwt.return_ok subid : (a, _) result Lwt.t)
      | Unsubscribe _, (id', `Unsubscribed) when id = id' -> Lwt.return_ok ()
      | _, (id', `Error e) when id = id' -> Lwt.return_error (`Msg e)
      | _ -> loop ()
    in
    Lwt.pick
      [ loop ()
      ; (Lwt_js.sleep timeout >>= fun () -> Lwt.return_error (`Msg "Request timed out"))
      ]

  let make_uri ?(insert_defaults = true) ?secure ?host ?port ~f ~path ~query =
    let host =
      match host, insert_defaults with
      | None, false -> None
      | None, true -> Some Url.Current.host
      | (Some _ as x), _ -> x
    in
    let port =
      match port, insert_defaults with
      | None, false -> None
      | None, true -> Url.Current.port
      | (Some _ as x), _ -> x
    in
    let scheme =
      match secure, insert_defaults with
      | Some false, _ -> Some "ws"
      | Some true, _ -> Some "wss"
      | None, false -> None
      | None, true -> (
        match Url.Current.protocol with
        | "https:" -> Some "wss"
        | "http:" -> Some "ws"
        | _ -> Some "ws")
    in
    Uri.kconstruct ?scheme ?host ?port ~path ~query ~f

  let unsubscribe ?(reqid = Random.bits ()) t subid =
    let ( >>= ) = Lwt_result.( >>= ) in
    let req = Unsubscribe (reqid, subid) in
    request t req
    >>= fun () ->
    (match Hashtbl.find_opt t.subscribers subid with
    | None -> ()
    | Some (n, e, _) -> (
      match !n with
      | 0 ->
          Hashtbl.remove t.subscribers subid;
          React.E.stop ~strong:true e
      | _ -> decr n));
    Lwt.return_ok ()

  let subscribe ?(reqid = Random.bits ()) ~path ~query =
    let ( >>= ) = Lwt_result.( >>= ) in
    let f (uri : Uri.t) _of (t : t) =
      let req = Subscribe (reqid, uri) in
      request t req
      >>= fun subid ->
      let e =
        match Hashtbl.find_opt t.subscribers subid with
        | Some (n, e, _) ->
            incr n;
            e
        | None ->
            let e, push = React.E.create () in
            Hashtbl.add t.subscribers subid (ref 0, e, push ?step:None);
            e
      in
      let e =
        React.E.fmap
          (fun x ->
            match _of x with
            | Error e ->
                print_endline e;
                None
            | Ok x -> Some x)
          e
      in
      Lwt.return_ok (subid, e)
    in
    make_uri ?secure:None ?host:None ?port:None ~insert_defaults:false ~f ~path ~query

  let close_socket = close_socket

  let open_socket :
         ?secure:bool
      -> ?host:string
      -> ?port:int
      -> path:('a, unit -> (t, error) Lwt_result.t) Uri.Path.Format.t
      -> 'a =
   fun ?secure ?host ?port ~path ->
    let f uri () : (t, error) Lwt_result.t =
      let control, push_ctrl = Lwt_stream.create () in
      let subscribers = Hashtbl.create 100 in
      let socket = new%js webSocket (Js.string uri) in
      let message_handler (evt : webSocket messageEvent Js.t) =
        (match Body.of_string @@ Js.to_string evt##.data with
        | Error (`Msg e) -> print_endline e
        | Ok body -> (
          match Msg.parse body with
          | None -> ()
          | Some (subid, `Event data) -> (
            match Hashtbl.find_opt subscribers subid with
            | None -> ()
            | Some (_, _, push) -> push data)
          | Some x -> push_ctrl @@ Some x));
        Js._true
      in
      Lwt.pick
        [ Lwt_js_events.make_event (Dom_events.Typ.make "open") socket
        ; Lwt_js_events.make_event (Dom_events.Typ.make "error") socket ]
      >>= fun event ->
      match Js.to_string event##._type with
      | "open" ->
          socket##.onmessage := Dom.handler message_handler;
          Lwt.return_ok {socket; subscribers; control}
      | "error" ->
          ignore @@ Js.Unsafe.global##.console##log event;
          Lwt.return_error (`Msg "socket error")
      | _ -> assert false
    in
    make_uri
      ?secure
      ?host
      ?port
      ~path
      ~insert_defaults:true
      ~query:Uri.Query.empty
      ~f:(fun uri -> f @@ Uri.pct_decode @@ Uri.to_string uri)
end

module JSON = Make (Application_types.Body) (Application_types.Body_ws)
