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
  get_sock sock_data body Structure.Streams.to_yojson (React.S.changes api.streams)

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
  get_sock sock_data body Settings.to_yojson (React.S.changes api.settings)

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
  get_sock sock_data body Wm.to_yojson (React.S.changes api.wm)

let get_vdata_sock api stream channel pid _ body sock_data () =
  let open Pipeline_protocol in
  match stream, channel, pid with
  | Some s, Some c, Some p ->
     let pred (x : Video_data.t) = x.pid = p && x.channel = c && x.stream = s in
     get_sock sock_data body Video_data.to_yojson (React.E.filter pred api.vdata)
  | Some s, Some c, _ ->
     let pred (x : Video_data.t) = x.channel = c && x.stream = s in
     get_sock sock_data body Video_data.to_yojson (React.E.filter pred api.vdata)
  | Some s, _, _ ->
     let pred (x : Video_data.t) = x.stream = s in
     get_sock sock_data body Video_data.to_yojson (React.E.filter pred api.vdata)
  | _ -> get_sock sock_data body Video_data.to_yojson api.vdata

let handlers api =
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

  
(* [ (module struct
 *      let domain = "pipeline"
 *      let handle = pipeline_handle api
 *    end : Api_handler.HANDLER)
 * ]
 *)

(* TODO fix dat *)

(* let of_seconds s =
 *   int_of_string s
 *   |> (fun s -> Ptime.add_span Ptime.epoch (Ptime.Span.of_int_s s))
 *   |> function Some t -> Ptime.to_rfc3339 t | None -> failwith "of_seconds: failure"
 *                                  
 * let get_structures api input id () =
 *   let open Pipeline_protocol in
 *   let input, id = (Result.get_exn @@ Common.Topology.input_of_string input), int_of_string id in
 *   let input = Common.Topology.{ input; id } in
 *   Lwt.catch (fun () ->
 *       api.model.struct_api.get_input input
 *       >>= function None -> respond_error "No data" () (\* TODO fix *\)
 *                   | Some (str, date) ->
 *                      let s = Structure.Streams.to_yojson str in
 *                      (`Tuple [s; Time.Period.Seconds.to_yojson @@ Time.to_span date])
 *                      |> Result.return
 *                      |> Json.respond_result)
 *     (function Failure e -> respond_error e ())
 * 
 * let get_structures_between api input id from to' () =
 *   let open Pipeline_protocol in
 *   let input, id = (Result.get_exn @@ Common.Topology.input_of_string input), int_of_string id in
 *   let input = Common.Topology.{ input; id } in
 *   Lwt.catch (fun () ->
 *       api.model.struct_api.get_input_between input
 *         (Option.get_exn @@ Time.of_span @@ Time.Period.Seconds.of_string from) (\* TODO consider time instead of span *\)
 *         (Option.get_exn @@ Time.of_span @@ Time.Period.Seconds.of_string to')
 *       >>= fun l ->
 *       let l = List.map (fun (str, date) -> `Tuple [ Structure.Streams.to_yojson str
 *                                                   ; Time.Period.Seconds.to_yojson @@ Time.to_span date]) l in
 *       Json.respond_result @@ Result.return (`List l))
 *     (function Failure e -> respond_error e ())
 *                                  
 * let archive_handle api id meth uri_sep sock_data _ body =
 *   (\* TODO match string + query *\)
 *   let path_list = Common.Uri.(split @@  Path.to_string uri_sep.path) in
 *   match meth, path_list with
 *   | `GET,  ["structures";i;num]   -> get_structures api i num ()
 *   | `GET,  ["structures_between";i;num;from;to'] -> get_structures_between api i num from to' ()
 *   | _                             -> not_found ()                                        
 *   ; (module struct
 *        let domain = "pipeline_archive"
 *        let handle = archive_handle api
 *      end : Api_handler.HANDLER) *)
