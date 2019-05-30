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

  let open_socket : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, unit -> ((t, string) Lwt_result.t)) Uri.Path.Format.t
    -> 'a =
    fun ?secure ?host ?port ~path ->
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
             | Some f -> f data);
        Js._true
      in
      let event_generator path =
        let msg = Msg.compose !next_id (Uri.to_string path) in
        let event, push = React.E.create () in
        let push = push ?step:None in
        socket##send (Js.string @@ Body.to_string msg);
        Weak.set handlers !next_id (Some push);
        incr next_id;
        event, push
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
    make_uri ?secure ?host ?port ~path ~f

end
