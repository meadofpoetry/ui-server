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
  val parse : t -> (int * t) option
  val compose : int -> string -> t
end

module Json_msg : CONTROL_MSG with type t = Yojson.Safe.json = struct
  type t = Yojson.Safe.json

  let parse : t -> (int * t) option = function
    | `Assoc [ "id", `Int id
             ; "path", path] -> Some (id, path)
    | _ -> None

  let compose id data : t =
    `Assoc [ "id", `Int id
           ; "data", `String data ]
end

module Make (Body : BODY) (Msg : CONTROL_MSG with type t = Body.t) : sig

  open Netlib

  type t

  val subscribe : path:('b, 'c) Uri.Path.Format.t
    -> query:('c, (Body.t -> ('a, string) result) -> t -> 'a React.event) Uri.Query.format
    -> 'b

  val open_socket : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, unit -> ((t, string) Lwt_result.t)) Uri.Path.Format.t
    -> 'a

end = struct

  open Netlib

  type t = Uri.t -> Body.t React.event

  let make_uri ?secure ?host ?port ~f ~path ~query=
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
    Uri.kconstruct ~scheme ~host ?port ~path ~query ~f

  let get_event x = fst x

  let make_event event_type (socket : webSocket Js.t) =
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

  let subscribe : path:('b, 'c) Uri.Path.Format.t
    -> query:('c, (Body.t -> ('a, string) result)
              -> t
              -> 'a React.event) Uri.Query.format
    -> 'b = fun ~path ~query ->
    let f (uri : Uri.t) _of (t : t) =
      let event = t uri in
      React.E.fmap (fun x -> match _of x with
          | Error e -> prerr_endline e; None
          | Ok x -> Some x) event in
    make_uri ?secure:None ?host:None ?port:None ~f ~path ~query

  let open_socket : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, unit -> ((t, string) Lwt_result.t)) Uri.Path.Format.t
    -> 'a =
    fun ?secure ?host ?port ~path ->
    let f uri () : (t, string) Lwt_result.t =
      let next_id = ref 0 in
      let handlers = Array.make 100 None in
      let socket = new%js webSocket (Js.string uri) in
      let message_handler (evt : webSocket messageEvent Js.t) =
        (match Body.of_event evt with
         | Error _ -> failwith "TODO"
         | Ok v ->
           match Msg.parse v with
           | None -> ()
           | Some (id, data) ->
             match Array.get handlers id with
             | None -> ()
             | Some f -> f data);
        Js._true
      in
      let event_generator path =
        let msg = Msg.compose !next_id (Uri.to_string path) in
        let event, push = React.E.create () in
        let push = push ?step:None in
        socket##send (Js.string @@ Body.to_string msg);
        Array.set handlers !next_id (Some push);
        incr next_id;
        event
      in
      Lwt.pick [ make_event "open" socket
               ; make_event "error" socket ]
      >>= fun (evt : webSocket Dom.event Js.t) ->
      match Js.to_string evt##._type with
      | "open" ->
        socket##.onmessage := Dom.handler message_handler;
        Lwt.return_ok event_generator
      | "error" -> Lwt.return_error "socket error"
      | _ -> assert false in
    make_uri ?secure ?host ?port ~path ~query:Uri.Query.empty ~f:(fun uri ->
      f @@ Uri.pct_decode @@ Uri.to_string uri)

end
