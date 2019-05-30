open Js_of_ocaml
open Js_of_ocaml.WebSockets

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.( >>= )

let return_ok x = Ok x

module type BODY = sig
  include Api.BODY
  val of_event : webSocket messageEvent Js.t -> (t, [ `Conv_error of string]) result
end

module type CONTROL_MSG = sig
  type t
  val parse : t -> (int * string) option
  val compose : int -> t -> t
end

type t = webSocket Js.t

module Make (Body : BODY) (Msg : CONTROL_MSG with type t = Body.t) = struct

  open Netlib

  type state = Body.t -> unit
     
  type t = Uri.t -> (Body.t React.event * state)

  let make_uri ?secure ?host ?port ~f ~path =
    let host = match host with
      | None -> Url.Current.host
      | Some x -> x in
    let port = match port with
      | None -> Url.Current.port
      | Some x -> Some x in
    let scheme = match secure with
      | Some false -> "ws"
      | Some true -> "wss"
      | None -> match Url.Current.protocol with
                | "https" -> "wss"
                | "http" -> "ws"
                | _ -> "ws" in
    Uri.kconstruct ~scheme ~host ?port ~path ~query:Uri.Query.empty
      ~f:(f % Uri.pct_decode % Uri.to_string)
  
  let open_socket : ?secure:bool
                    -> ?host:string
                    -> ?port:int
                    -> path:('a, unit -> ((t, string) Lwt_result.t)) Uri.Path.Format.t
                    -> 'a =
    fun ?sequre ?host ?port ~path ->
    let f uri () : (t, string) Lwt_result.t =
      let next_id = ref 0 in
      let handlers = Weak.create 100 in
      let socket = new%js webSocket (Js.string uri) in
      let message_handler (evt : webSocket messageEvent Js.t) =
        (match Body.of_event evt with
         | Error _ -> failwith "TODO"
         | Ok v ->
            match Msg.parse v with
            | None -> ()
            | Some (id, data) ->
               match Weak.get handlers id with
               | None -> ()
               | Some f -> f data)
        Js._true
      in
      let event_generator path =
        let msg = Msg.compose !next_id (Uri.to_string path) in
        let event, push = React.E.create () in
        socket##send (Js.string @@ Body.to_string msg);
        Weak.set handlers !next_id push;
        incr next_id;
        event, push
      in
      Lwt.pick [ Event.make "open" socket
               ; Event.make "error" socket ]
      >>= fun (evt : webSocket Dom.event Js.t) ->
      match Js.to_string evt##._type with
      | "open" ->
         socket##.onmessage := Dom.handler message_handler;
         Lwt.return 
      | "error" -> Lwt.return_error "socket error"
      | _ -> assert false in
    make_uri ?secure ?host ?port ~path ~f
    

end
       (*
module Event = struct

  let make event_type (socket : t) =
    let el = ref Js.null in
    let t, w = Lwt.task () in
    let cancel () = Js.Opt.iter !el Dom.removeEventListener in
    Lwt.on_cancel t cancel;
    el :=
      Js.some
        (Dom.addEventListener
           socket
           (Dom.Event.make event_type)
           (Dom.handler (fun (ev : 'a #Dom.event Js.t) ->
                cancel ();
                Lwt.wakeup w ev;
                Js._true))
           Js._false);
    t

  let close (socket : t) =
    match socket##.readyState with
    | CLOSED -> Lwt.fail_with "already closed"
    | _ -> make "close" socket

  let error (socket : t) =
    match socket##.readyState with
    | CLOSED -> Lwt.fail_with "already closed"
    | _ -> make "error" socket

end

let url (t : t) : string =
  Js.to_string t##.url

let state (t : t) : readyState =
  t##.readyState

let close ?code ?reason (t : t) =
  match code, reason with
  | None, None -> t##close
  | Some code, None -> t##close_withCode code
  | Some code, Some reason ->
    t##close_withCodeAndReason code (Js.string reason)
  | None, Some reason ->
    let default = 1005 in
    t##close_withCodeAndReason default (Js.string reason)

module Make(Body : BODY) : sig

  open Netlib

  type t = webSocket Js.t

  val create : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, 'b) Uri.Path.Format.t
    -> query:('b, unit -> ((t, string) Lwt_result.t)) Uri.Query.format
    -> 'a

  val subscribe : t -> ((Body.t, string) result -> unit) -> unit

  val subscribe_map : t
    -> (Body.t -> ('a, string) result)
    -> (('a, string) result -> unit)
    -> unit

end = struct

  open Netlib

  type t = webSocket Js.t

  let make_uri ?secure ?host ?port ~f ~path ~query =
    let host = match host with
      | None -> Url.Current.host
      | Some x -> x in
    let port = match port with
      | None -> Url.Current.port
      | Some x -> Some x in
    let scheme = match secure with
      | Some false -> "ws"
      | Some true -> "wss"
      | None -> match Url.Current.protocol with
        | "https" -> "wss"
        | "http" -> "ws"
        | _ -> "ws" in
    Uri.kconstruct ~scheme ~host ?port ~path ~query
      ~f:(f % Uri.pct_decode % Uri.to_string)

  let create ?secure ?host ?port ~path ~query =
    let f uri () : (t, string) Lwt_result.t =
      let socket = new%js webSocket (Js.string uri) in
      Lwt.pick [ Event.make "open" socket
               ; Event.make "error" socket ]
      >>= fun (evt : webSocket Dom.event Js.t) ->
      match Js.to_string evt##._type with
      | "open" -> Lwt.return_ok socket
      | "error" -> Lwt.return_error "socket error"
      | _ -> assert false in
    make_uri ?secure ?host ?port ~path ~query ~f

  let subscribe (socket : t) (f : (Body.t, string) result -> unit) : unit =
    let message_handler = fun (evt : webSocket messageEvent Js.t) ->
      (match Body.of_event evt with
       | Ok x -> f @@ Ok x
       | Error `Conv_error s -> f @@ Error s);
      Js._true in
    socket##.onmessage := Dom.handler message_handler

  let subscribe_map socket _of f =
    let f = function
      | Error e -> f @@ Error e
      | Ok json -> f @@ _of json in
    subscribe socket f

end
        *)
