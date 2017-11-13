open Api.Interaction
open Api.Redirect
open Pipeline
open Websocket_cohttp_lwt
open Frame
open Containers
   
open Lwt.Infix

module Api_handler = Api.Handler.Make(Common.User)
   
let ( % ) = CCFun.(%)

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)
       
let socket_table = Hashtbl.create 1000

let get_page () =
  respond_html_elt
    Tyxml.Html.(div
                  [ script ~a:[a_src "/js/pipeline.js"] ( pcdata "" );
                    h2 [ pcdata "Pipeline page" ];
                    p  [ pcdata "Some text" ];
                    div ~a:[ a_id "pipeline_container" ] [  ] ] )
    ()

let set body conv apply =
  yojson_of_body body >>= fun js ->
  match conv js with
  | Error e -> respond_error e ()
  | Ok x    -> apply x
               >>= fun () -> respond_ok ()

let get_sock sock_data body conv event =
  let id = rand_int () in
  Cohttp_lwt_body.drain_body body
  >>= fun () ->
  Websocket_cohttp_lwt.upgrade_connection
    (fst sock_data)
    (snd sock_data)
    (fun f -> match f.opcode with
              | Opcode.Close -> Hashtbl.remove socket_table id
              | _ -> ())
  >>= fun (resp, body, frames_out_fn) ->
  let send x =
    let msg = Api.Msg_conv.to_string @@ conv x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.E.map send event in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt_body.t))

let set_streams pipe body () =
  set body Streams.of_yojson
      (fun strm -> pipe.set [Streams strm])

let get_streams pipe () =
  pipe.get [`Streams]
  >>= function
  | [Streams s] -> Streams.to_yojson s
                   |> fun js -> respond_js js ()
  | _ -> respond_error "Unknown error" ()

let get_streams_sock sock_data body pipe () =
  get_sock sock_data body Streams.to_yojson pipe.streams_events

let set_settings pipe body () =
  set body Settings.of_yojson
      (fun sets -> pipe.set [Settings sets])

let get_settings pipe () =
  pipe.get [`Settings]
  >>= function
  | [Settings s] -> Settings.to_yojson s
                   |> fun js -> respond_js js ()
  | _ -> respond_error "Unknown error" ()

let get_settings_sock sock_data body pipe () =
  get_sock sock_data body Settings.to_yojson pipe.settings_events

let pipeline_handle pipe id meth args sock_data _ body =
  let is_guest = Common.User.eq id `Guest in
  match meth, args with
  | `GET,  []                -> get_page ()
  | `POST, ["streams"]       -> redirect_if is_guest @@ set_streams pipe body
  | `GET,  ["streams"]       -> get_streams pipe ()
  | _ ,    ["streams_sock"]  -> get_streams_sock sock_data body pipe ()
  | `POST, ["settings"]      -> redirect_if is_guest @@ set_settings pipe body
  | `GET,  ["settings"]      -> get_settings pipe ()
  | _ ,    ["settings_sock"] -> get_settings_sock sock_data body pipe ()                   
  | _                        -> not_found ()
                     
let handlers pipe =
  [ (module struct
       let domain = "pipeline"
       let handle = pipeline_handle pipe
     end : Api_handler.HANDLER) ]

let handlers_not_implemented () =
  []
  
    (*
let test _ _ body =
  Cohttp_lwt_body.to_string body >>= fun body ->
  let jss = String.split_on_char '=' body |> fun l -> List.nth l 1 in
  let js  = Uri.pct_decode jss |> Yojson.Safe.from_string in
  Lwt_io.printf "Got: %s\n" (Yojson.Safe.to_string js) >>= fun _ ->
  let s =
    Common.State.of_yojson js
    |> function
      | Error _ -> "Sorry, something is wrong with your json"
      | Ok root -> Lwt_io.printf "Msgpck: %s\n" (Msg_conv.to_msg_string @@ Common.State.to_yojson root) |> ignore;
                   let prefix = "Thank you, master! " in
                   begin match (CCOpt.get_exn root.graph).state with
                   | Some Null  -> prefix ^ "You want me to stop the graph?"
                   | Some Stop  -> prefix ^ "Halting graph"
                   | Some Play  -> prefix ^ "Starting"
                   | Some Pause -> prefix ^ "Graph is going to be paused"
                   | None       -> prefix ^ "I don't understand"
                   end
  in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:s ()
        *)
