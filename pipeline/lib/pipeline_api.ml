open Containers
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect
open Websocket_cohttp_lwt
open Frame
open Qoe_errors
open Common

open Lwt.Infix

module Api_handler = Api.Handler.Make(Common.User)

let ( %> ) = Fun.( %> )

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let socket_table = Hashtbl.create 1000
                 
let get_page id headers body =
  respond_html_elt
    Tyxml.Html.(div
                  [ h2 [ pcdata "Pipeline page" ];
                    p  [ pcdata "Some text" ];
                    div ~a:[ a_id "pipeline_container" ] [  ] ] )
    ()

let set body conv apply =
  of_body body >>= fun js ->
  match conv js with
  | Error e -> respond_error e ()
  | Ok x    -> apply x >>= function Ok () -> respond_result_unit (Ok ())

let get_sock sock_data body conv event =
  let id = rand_int () in
  Cohttp_lwt.Body.drain_body body
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
  Lwt.return (resp, (body :> Cohttp_lwt.Body.t))

let set_structure api headers body () =
  Lwt_io.printf "set structure\n" |> ignore;
  set body Structure.Streams.of_yojson
    Pipeline_protocol.(fun x -> api.requests.streams.set x)

let get_structure api headers body () =
  let open Pipeline_protocol in
  api.requests.streams.get ()
  >>= (function
       | Error e -> Lwt.fail_with e
       | Ok v -> Lwt.return v)
  >|= (Structure.Streams.to_yojson %> Result.return)
  >>= respond_result

let get_structure_sock api _ body sock_data () =
  let open Pipeline_protocol in
  get_sock sock_data body Structure.Streams.to_yojson (React.S.changes api.notifs.streams)

let set_settings api headers body () =
  set body Settings.of_yojson
      Pipeline_protocol.(fun x -> api.requests.settings.set x)

let get_settings api headers body () =
  let open Pipeline_protocol in
  api.requests.settings.get ()
  >>= (function
       | Error e -> Lwt.fail_with e
       | Ok v    -> Lwt.return v)
  >|= (Settings.to_yojson %> Result.return)
  >>= respond_result

let get_settings_sock api _ body sock_data () =
  let open Pipeline_protocol in
  get_sock sock_data body Settings.to_yojson (React.S.changes api.notifs.settings)

let set_wm api _ body () =
  set body Wm.of_yojson
    Pipeline_protocol.(fun x -> api.requests.wm.set x)

let get_wm api _ body () =
  let open Pipeline_protocol in
  api.requests.wm.get ()
  >>= (function
       | Error e -> Lwt.fail_with e
       | Ok v -> Lwt.return v)
  >|= (Wm.to_yojson %> Result.return)
  >>= respond_result

let get_wm_sock api _ body sock_data () =
  let open Pipeline_protocol in
  get_sock sock_data body Wm.to_yojson (React.S.changes api.notifs.wm)

let get_status api _ body () =
  let open Pipeline_protocol in
  Lwt_react.S.value api.notifs.status
  |> Qoe_status.status_list_to_yojson
  |> fun r -> respond_result (Ok r)
  
let get_status_sock api _ body sock_data () =
  let open Pipeline_protocol in
  get_sock sock_data body Qoe_status.status_list_to_yojson (React.S.changes api.notifs.status)

let get_vdata_sock api stream channel pid _ body sock_data () =
  let open Pipeline_protocol in
  match stream, channel, pid with
  | Some s, Some c, Some p ->
     let pred (x : Video_data.t) = x.pid = p && x.channel = c && x.stream = s in
     get_sock sock_data body Video_data.to_yojson (React.E.filter pred api.notifs.vdata)
  | Some s, Some c, _ ->
     let pred (x : Video_data.t) = x.channel = c && x.stream = s in
     get_sock sock_data body Video_data.to_yojson (React.E.filter pred api.notifs.vdata)
  | Some s, _, _ ->
     let pred (x : Video_data.t) = x.stream = s in
     get_sock sock_data body Video_data.to_yojson (React.E.filter pred api.notifs.vdata)
  | _ -> get_sock sock_data body Video_data.to_yojson api.notifs.vdata

module Archive = struct

  type res = (Stream.t * Time.t * Time.t) list [@@deriving yojson]

  type res_struct = (Structure.structure * Time.t) list [@@deriving yojson]
  
  let get_streams db limit from till duration _ _ () =
    match Time.make_interval ?from ?till ?duration () with
    | Ok `Range (from,till) ->
       Db.Streams.select_streams db ?limit ~from ~till
       |> Lwt_result.map (Api.Api_types.rows_to_yojson res_to_yojson (fun () -> `Null))
       |> Lwt_result.map_err (fun e -> `String e)
       >>= respond_result
    | _ -> respond_error ~status:`Not_implemented "FIXME" ()

  let get_structures db uris limit from till duration _ _ () =
    let uris = List.map Stream.tsoip_id_of_url uris in
    match Time.make_interval ?from ?till ?duration () with
    | Ok `Range (from,till) ->
       Db.Structure.select_structures db ?limit ~uris ~from ~till
       |> Lwt_result.map (Api.Api_types.rows_to_yojson res_struct_to_yojson (fun () -> `Null))
       |> Lwt_result.map_err (fun e -> `String e)
       >>= respond_result
    | _ -> respond_error ~status:`Not_implemented "FIXME" ()  
    
end
       
let handlers (api : Pipeline_protocol.api) =
  let open Common.Uri in
  let open Api_handler in
  create_dispatcher
    "pipeline"
    [ create_ws_handler ~docstring:"Structure websocket"
        ~path:Path.Format.("structure" @/ empty)
        ~query: Query.empty
        (get_structure_sock api)
    ; create_ws_handler ~docstring:"Settings socket"
        ~path:Path.Format.("settings" @/ empty)
        ~query:Query.empty
        (get_settings_sock api)
    ; create_ws_handler ~docstring:"WM socket"
        ~path:Path.Format.("wm" @/ empty)
        ~query:Query.empty
        (get_wm_sock api)
    ; create_ws_handler ~docstring:"Stream status socket"
        ~path:Path.Format.("status" @/ empty)
        ~query:Query.empty
        (get_status_sock api)
    ; create_ws_handler ~docstring:"Video data socket"
        ~path:Path.Format.("vdata" @/ empty)
        ~query:Query.[ "stream",  (module Option(Int))
                     ; "channel", (module Option(Int))
                     ; "pid",     (module Option(Int)) ]
        (get_vdata_sock api)
    ]
    [ `GET,  [ create_handler ~docstring:"Pipeline page"
                 ~path:Path.Format.empty
                 ~query:Query.empty
                 get_page
             ; create_handler ~docstring:"Structure"
                 ~path:Path.Format.("structure" @/ empty)
                 ~query:Query.empty
                 (get_structure api)
             ; create_handler ~docstring:"Settings"
                 ~path:Path.Format.("settings" @/ empty)
                 ~query:Query.empty
                 (get_settings api)
             ; create_handler ~docstring:"Wm"
                 ~path:Path.Format.("wm" @/ empty)
                 ~query:Query.empty
                 (get_wm api)
             ; create_handler ~docstring:"Status"
                 ~path:Path.Format.("status" @/ empty)
                 ~query:Query.empty
                 (get_status api)
             (* Archive *)
             ; create_handler ~docstring:"Streams archive"
                 ~path:Path.Format.("streams/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 (Archive.get_streams api.model.db)
             ; create_handler ~docstring:"Structure archive"
                 ~path:Path.Format.("structure/archive" @/ empty)
                 ~query:Query.[ "uris",     (module List(Common.Url.Q))
                              ; "limit",    (module Option(Int))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 (Archive.get_structures api.model.db)
             ]
    ; `POST, [ create_handler ~docstring:"Post structure"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("structure" @/ empty)
                 ~query:Query.empty
                 (set_structure api)
             ; create_handler ~docstring:"Post settings"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("settings" @/ empty)
                 ~query:Query.empty
                 (set_settings api)
             ; create_handler ~docstring:"Post wm"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("wm" @/ empty)
                 ~query:Query.empty
                 (set_wm api)
             ]
    ]
