open Js_of_ocaml
open Js_of_ocaml.WebSockets
open Js_of_ocaml_lwt

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.( >>= )

let return_ok x = Ok x

include Websocket_intf

module type BODY = sig
  include Api.BODY
  val of_event : webSocket messageEvent Js.t -> (t, string) result
  val of_msg : Wamp.Element.t -> t
  val to_msg : t -> Wamp.Element.t
end

module Json_body : BODY with type t = Yojson.Safe.json = struct
  open StdLabels
  include Api.Json_body
  let of_event evt =
    match of_string @@ Js.to_string evt##.data with
    | Ok _ as x -> x
    | Error `Conv_error e -> Error e
  type repr = Yojson.Safe.json

  let rec to_msg = function
    | `Bool b -> Wamp.Element.Bool b
    | `Int i -> Int i
    | `String s -> String s
    | `List l -> List (List.map l ~f:to_msg)
    | `Assoc a -> Dict (List.map a ~f:(fun (s, v) -> s, to_msg v))
    | _ -> invalid_arg "of_repr"

  let rec of_msg = function
    | Wamp.Element.Int i -> `Int i
    | String s -> `String s
    | Bool b -> `Bool b
    | Dict d -> `Assoc (List.map d ~f:(fun (k, v) -> k, of_msg v))
    | List l -> `List (List.map l ~f:of_msg)
end

type 'a t =
  { socket : WebSockets.webSocket Js.t
  ; subscribers : (int, 'a React.event * ('a -> unit)) Hashtbl.t
  ; control : Wamp.t React.event
  }

let close_socket (t : 'a t) =
  t.socket##close

module Make (Body : BODY) = struct

  open Netlib

  type nonrec t = Body.t t

  module Wamp_body = Wamp.Make(Body)

  type timeout_error =
    [ `Timeout of float ]

  type error =
    [ `Error of Uri.t * string option
    ]

  type subscribe_error =
    [ error
    | timeout_error
    ]

  let make_uri ?(insert_defaults = true) ?secure ?host ?port ~f ~path ~query=
    let host = match host, insert_defaults with
      | None, false -> None
      | None, true -> Some Url.Current.host
      | Some _ as x, _ -> x in
    let port = match port, insert_defaults with
      | None, false -> None
      | None, true -> Url.Current.port
      | Some _ as x, _ -> x in
    let scheme = match secure, insert_defaults with
      | Some false, _ -> Some "ws"
      | Some true, _ -> Some "wss"
      | None, false -> None
      | None, true -> match Url.Current.protocol with
        | "https" -> Some "wss"
        | "http" -> Some "ws"
        | _ -> Some "ws" in
    Uri.kconstruct ?scheme ?host ?port ~path ~query ~f

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

  let wait_message
      ?(timeout = 2.)
      ~pred
      (socket : t) =
    let timer = Lwt_js.sleep timeout >>= fun () -> Lwt.return `Tm in
    let return x = Lwt.cancel timer; Lwt.return x in
    let rec aux () =
      Lwt.choose
        [ (Lwt_react.E.next socket.control >>= fun x -> Lwt.return @@ `Ev x)
        ; timer ]
      >>= function
      | `Tm -> Lwt.return_error (`Timeout timeout)
      | `Ev msg ->
        match pred msg with
        | `Stop x -> return x
        | `Resume -> aux () in
    aux ()

  let open_session (t : t) =
    let realm = Uri.of_string "ui-server" in
    let msg =
      Body.to_string
      @@ Wamp_body.serialize
      @@ Wamp.hello_roles ~realm ~roles:[Wamp.Subscriber] in
    t.socket##send (Js.string msg);
    wait_message
      ~pred:(function
          | Welcome x -> `Stop (Ok x.id)
          | Abort x ->
            let message = match x.details with
              | ["message", String s] -> Some s
              | _ -> None in
            `Stop (Error (`Abort (x.reason, message)))
          | x -> `Stop (Error (`Unexpected_msg (Wamp.msg_type x))))
      t

  let close_session
      ?(reason = Uri.of_string "wamp.close_realm")
      ?message
      (t : t) =
    let msg =
      Wamp.goodbye
        ~reason
        ~details:(match message with
            | None -> []
            | Some s -> ["message", String s]) in
    t.socket##send (Js.string @@ Body.to_string @@ Wamp_body.serialize msg);
    wait_message ~pred:(function
        | Goodbye { reason; details }->
          let message = match details with
            | ["message", String s] -> Some s
            | _ -> None in
          `Stop (Ok (reason, message))
        | _ -> `Resume) t

  let subscribe ~path ~query =
    let f (uri : Uri.t) _of (t : t) =
      let req_id, msg = Wamp.subscribe uri in
      t.socket##send (Js.string @@ Body.to_string @@ Wamp_body.serialize msg);
      wait_message ~pred:(function
          | Error { reqid; reqtype; details; error; _ }
            when req_id = reqid && reqtype = Wamp.msg_type_to_enum SUBSCRIBE ->
            let message = match details with
              | ["message", String s] -> Some s
              | _ -> None in
            `Stop (Error (`Error (error, message)))
          | Subscribed { reqid; id } when req_id = reqid ->
            let e = match Hashtbl.find_opt t.subscribers id with
              | Some (e, _) -> e
              | None ->
                let e, push = React.E.create () in
                Hashtbl.add t.subscribers id (e, push ?step:None);
                e in
            let e = React.E.fmap (fun x -> match _of x with
                | Error e -> print_endline e; None
                | Ok x -> Some x) e in
            `Stop (Ok (id, e))
          | _ -> `Resume) t in
    make_uri ?secure:None ?host:None ?port:None
      ~insert_defaults:false
      ~f ~path ~query

  let unsubscribe (t : t) id =
    let req_id = Random.bits () in
    let msg = Wamp.unsubscribe ~reqid:req_id ~id in
    t.socket##send (Js.string @@ Body.to_string @@ Wamp_body.serialize msg);
    wait_message ~pred:(function
        | Error { reqid; reqtype; details; error; _ }
          when req_id = reqid && reqtype = Wamp.msg_type_to_enum UNSUBSCRIBE ->
          let message = match details with
            | ["message", String s] -> Some s
            | _ -> None in
          `Stop (Error (`Error (error, message)))
        | Unsubscribed reqid when req_id = reqid ->
          (match Hashtbl.find_opt t.subscribers id with
           | None -> ()
           | Some (e, _) -> React.E.stop ~strong:true e);
          Hashtbl.remove t.subscribers id;
          `Stop (Ok ())
        | _ -> `Resume) t

  let close_socket = close_socket

  let open_socket : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, unit -> ((t, string) Lwt_result.t)) Uri.Path.Format.t
    -> 'a =
    fun ?secure ?host ?port ~path ->
    let ( >>=? ) x f = match x with Error _ as e -> e | Ok x -> f x in
    let f uri () : (t, string) Lwt_result.t =
      let e, push = React.E.create () in
      let subscribers = Hashtbl.create 100 in
      let socket = new%js webSocket (Js.string uri) in
      let message_handler (evt : webSocket messageEvent Js.t) =
        (match Body.of_event evt >>=? Wamp_body.parse % Body.to_msg with
         | Error _ -> failwith "TODO"
         | Ok Event { subid; kw_args = ["data", data]; _ } ->
           (match Hashtbl.find_opt subscribers subid with
            | None -> ()
            | Some (_, push) -> push @@ Body.of_msg data)
         | Ok Event _ -> ()
         | Ok m -> push m);
        Js._true
      in
      Lwt.pick [ make_event "open" socket
               ; make_event "error" socket ]
      >>= fun (evt : webSocket Dom.event Js.t) ->
      match Js.to_string evt##._type with
      | "open" ->
        socket##.onmessage := Dom.handler message_handler;
        Lwt.return_ok { socket; subscribers; control = e }
      | "error" -> Lwt.return_error "socket error"
      | _ -> assert false in
    make_uri ?secure ?host ?port ~path
      ~insert_defaults:true
      ~query:Uri.Query.empty
      ~f:(fun uri -> f @@ Uri.pct_decode @@ Uri.to_string uri)

end

module JSON = Make(Json_body)
