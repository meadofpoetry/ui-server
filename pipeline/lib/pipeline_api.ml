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
    let msg = Msg_conv.to_string @@ conv x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.E.map send event in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt_body.t))

let set_structure api body () =
  set body Structure.t_list_of_yojson
      Pipeline_protocol.(fun x -> api.set (Set_structures x))

let get_structure api () =
  let open Pipeline_protocol in
  api.get Get_structures
  >|= Structure.t_list_to_yojson
  >>= fun js -> respond_js js ()

let get_structure_sock sock_data body api () =
  let open Pipeline_protocol in
  get_sock sock_data body Structure.t_list_to_yojson (React.S.changes api.structure)

let set_settings api body () =
  set body Settings.of_yojson
      Pipeline_protocol.(fun x -> api.set (Set_settings x))

let get_settings api () =
  let open Pipeline_protocol in
  api.get Get_settings
  >|= Settings.to_yojson
  >>= fun js -> respond_js js ()

let get_settings_sock sock_data body api () =
  let open Pipeline_protocol in
  get_sock sock_data body Settings.to_yojson api.settings

let pipeline_handle api id meth args sock_data _ body =
  let is_guest = Common.User.eq id `Guest in
  match meth, args with
  | `GET,  []                 -> get_page ()
  | `POST, ["strucutre"]      -> redirect_if is_guest @@ set_structure api body
  | `GET,  ["structure"]      -> get_structure api ()
  | `GET,  ["structure_sock"] -> get_structure_sock sock_data body api ()
  | `POST, ["settings"]       -> redirect_if is_guest @@ set_settings api body
  | `GET,  ["settings"]       -> get_settings api ()
  | `GET,  ["settings_sock"]  -> get_settings_sock sock_data body api ()                   
  | _                         -> not_found ()
                     
let handlers pipe =
  [ (module struct
       let domain = "pipeline"
       let handle = pipeline_handle pipe.api
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
