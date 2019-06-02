open Js_of_ocaml
open Js_of_ocaml.WebSockets
open Js_of_ocaml_lwt
open Application_types

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.( >>= )

let return_ok x = Ok x

include Websocket_intf

type 'a t =
  { socket : WebSockets.webSocket Js.t
  ; subscribers : (int, 'a React.event * ('a -> unit)) Hashtbl.t
  ; control : (int * Api.WS_BODY) React.event
  }

let close_socket (t : 'a t) =
  t.socket##close

module Make
    (Boby : Api.BODY)
    (Msg : Api.WS_BODY with type t := Body.t) = struct

  open Netlib

  type nonrec t = Body.t t

  type 'a req =
    | Subscribe : int * Uri.t -> int req
    | Unsubscribe : int * int -> unit req

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

  let request (type a) ?(timeout = 2.) (socket : t) (req : a req) =
    let timer = Lwt_js.sleep timeout >>= fun () -> Lwt.return `Tm in
    let rec aux () =
      Lwt.choose
        [ (Lwt_react.E.next socket.control >>= fun x -> Lwt.return @@ `Ev x)
        ; timer ]
      >>= function
      | `Tm -> Lwt.return_error (`Timeout timeout)
      | `Ev msg -> Lwt.return_error `TODO in
    aux ()

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

  (* let unsubscribe ?(reqid = Random.bits ()) id (t : t) =
   *   let msg = Msg.Unsubscribe { reqid; id} in
   *   t.socket##send (Js.string @@ Body.to_string @@ Msg.compose msg);
   *   wait_message ~pred:(function
   *       | Err { reqid = reqid'; error } when reqid' = reqid ->
   *         `Stop (Error (`Error error))
   *       | Unsubscribed reqid' when reqid' = reqid ->
   *         `Stop (Ok ())
   *       | _ -> `Resume) t *)

  (* let subscribe ?(reqid = Random.bits ()) ~path ~query =
   *   let f (uri : Uri.t) _of (t : t) =
   *     let msg = Msg.Subscribe { reqid; uri = Uri.to_string uri } in
   *     t.socket##send (Js.string @@ Body.to_string @@ Msg.compose msg);
   *     wait_message ~pred:(function
   *         | Err { reqid = reqid'; error } when reqid' = reqid  ->
   *           `Stop (Error (`Error error))
   *         | Subscribed { reqid = reqid'; id } when reqid' = reqid ->
   *           let e = match Hashtbl.find_opt t.subscribers id with
   *             | Some (e, _) -> e
   *             | None ->
   *               let e, push = React.E.create () in
   *               Hashtbl.add t.subscribers id (e, push ?step:None);
   *               e in
   *           let e = React.E.fmap (fun x -> match _of x with
   *               | Error e -> print_endline e; None
   *               | Ok x -> Some x) e in
   *           `Stop (Ok (id, e))
   *         | _ -> `Resume) t in
   *   make_uri ?secure:None ?host:None ?port:None
   *     ~insert_defaults:false
   *     ~f ~path ~query *)

  let close_socket = close_socket

  let open_socket : ?secure:bool
    -> ?host:string
    -> ?port:int
    -> path:('a, unit -> ((t, string) Lwt_result.t)) Uri.Path.Format.t
    -> 'a =
    fun ?secure ?host ?port ~path ->
    let f uri () : (t, string) Lwt_result.t =
      let e, push = React.E.create () in
      let subscribers = Hashtbl.create 100 in
      let socket = new%js webSocket (Js.string uri) in
      let message_handler (evt : webSocket messageEvent Js.t) =
        (match Body.of_string @@ Js.to_string evt##.data with
         | Error _ -> failwith "TODO"
         | Ok body ->
           match Msg.parse body with
           | None -> ()
           | Some (subid, Event data) ->
             (match Hashtbl.find_opt subscribers subid with
              | None -> ()
              | Some (_, push) -> push data)
           | Some x -> push x);
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

module JSON = Make(Application_types.Body)(Application_types.Body_ws)
