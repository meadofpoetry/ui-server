open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect

(** API
    POST /receiver/mode

    GET  /receiver/mode
    GET  /receiver/lock
    GET  /receiver/measures
    GET  /receiver/parameters
    GET  /receiver/plp-list
 *)


let parse_id = Uri.Query.(parse_query ["id", (module Option(Int))] (fun x -> x))

module WS = struct

  let mode sock_data (events:events) body (query:Uri.Query.t) () =
    match parse_id query with
    | Ok (Some id) ->
       let e = React.E.filter (fun (m:mode) -> m.id = id) events.mode in
       sock_handler sock_data e mode_to_yojson body
    | Ok None -> sock_handler sock_data events.mode mode_to_yojson body
    | Error e -> respond_error "bad query" ()

  let lock sock_data (events:events) body (query:Uri.Query.t) () =
    match parse_id query with
    | Ok (Some id) ->
       let e = React.E.filter (fun (l:lock) -> l.id = id) events.lock in
       sock_handler sock_data e lock_to_yojson body
    | Ok None -> sock_handler sock_data events.lock lock_to_yojson body
    | Error e -> respond_error "bad query" ()

  let measures sock_data (events:events) body (query:Uri.Query.t) () =
    match parse_id query with
    | Ok (Some id) ->
       let e = React.E.filter (fun (m:measures) -> m.id = id) events.measures in
       sock_handler sock_data e measures_to_yojson body
    | Ok None -> sock_handler sock_data events.measures measures_to_yojson body
    | Error e -> respond_error "bad query" ()

  let parameters sock_data (events:events) body (query:Uri.Query.t) () =
    match parse_id query with
    | Ok (Some id) ->
       let e = React.E.filter (fun (p:params) -> p.id = id) events.params in
       sock_handler sock_data e params_to_yojson body
    | Ok None -> sock_handler sock_data events.params params_to_yojson body
    | Error e -> respond_error "bad query" ()

  let plp_list sock_data (events:events) body (query:Uri.Query.t) () =
    match parse_id query with
    | Ok (Some id) ->
       let e = React.E.filter (fun (x:plp_list) -> x.id = id) events.plp_list in
       sock_handler sock_data e plp_list_to_yojson body
    | Ok None -> sock_handler sock_data events.plp_list plp_list_to_yojson body
    | Error e -> respond_error "bad query" ()

end

module HTTP = struct

  let post_mode (api:api) body () =
    of_body body >>= fun mode ->
    (match mode_of_yojson mode with
     | Error e -> Lwt_result.fail @@ Json.of_error_string e
     | Ok mode -> api.set_mode mode >|= (mode_rsp_to_yojson %> Result.return))
    >>= Json.respond_result

  let mode_last (api:api) (query:Uri.Query.t) () =
    let channel (x:config_item) = match x.standard with
      | T2 -> x.t2 | T -> x.t | C -> x.c
    in
    let config = api.get_config () in
    match parse_id query with
    | Ok (Some id) ->
       (match List.find_opt (fun (id',_) -> id = id') config with
        | Some (_,({standard;_} as x)) ->
           let mode = { id; standard; channel = channel x } in
           respond_result (Ok (mode_to_yojson mode))
        | None -> (`String "no receiver found") |> Result.fail |> respond_result)
    | Ok None ->
       List.map (fun (id,({standard;_} as x)) -> { id; standard; channel = channel x }) config
       |> modes_to_yojson
       |> Result.return
       |> respond_result
    | Error e -> respond_error "bad query" ()

  let get_last (get:unit -> 'a list)
               (get_id:'a -> int)
               (one_to_yojson:'a -> Yojson.Safe.json)
               (all_to_yojson:'a list -> Yojson.Safe.json)
               (query:Uri.Query.t) () =
    let coll = get () in
    match parse_id query with
    | Ok (Some id) ->
       (match List.find_opt (fun (x:'a) -> (get_id x) = id) coll with
        | Some x -> one_to_yojson x     |> Result.return |> respond_result
        | None   -> `String "not found" |> Result.fail   |> respond_result)
    | Ok None -> all_to_yojson coll |> Result.return |> respond_result
    | Error e -> respond_error "bad query" ()

  let lock_last (api:api) (query:Uri.Query.t) () =
    get_last api.get_lock (fun x -> x.id) lock_to_yojson lock_all_to_yojson query ()

  let measures_last (api:api) (query:Uri.Query.t) () =
    get_last api.get_measures (fun x -> x.id) measures_to_yojson measures_all_to_yojson query ()

  let parameters_last (api:api) (query:Uri.Query.t) () =
    get_last api.get_params (fun x -> x.id) params_to_yojson params_all_to_yojson query ()

  let plp_list_last (api:api) (query:Uri.Query.t) () =
    get_last api.get_plp_list (fun x -> x.id) plp_list_to_yojson plp_list_all_to_yojson query ()

  module Archive = struct

    let mode time (query:Uri.Query.t) () =
      not_found ()

    let lock time (query:Uri.Query.t) () =
      not_found ()

    let measures time (query:Uri.Query.t) () =
      not_found ()

    let parameters time (query:Uri.Query.t) () =
      not_found ()

  end

  let mode (api:api) (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (Some time,query) -> Archive.mode time query ()
    | Ok (None,query)      -> mode_last api query ()
    | Error e              -> respond_error "bad query" ()

  let lock (api:api) (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (Some time,query) -> Archive.lock time query ()
    | Ok (None,query)      -> lock_last api query ()
    | Error e              -> respond_error "bad query" ()

  let measures (api:api) (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (Some time,query) -> Archive.measures time query ()
    | Ok (None,query)      -> measures_last api query ()
    | Error e              -> respond_error "bad query" ()

  let parameters (api:api) (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (Some time,query) -> Archive.parameters time query ()
    | Ok (None,query)      -> parameters_last api query ()
    | Error e              -> respond_error "bad query" ()

  let plp_list (api:api) (query:Uri.Query.t) () =
    match Api.Query.Time.get' query with
    | Ok (Some time,query) -> respond_error ~status:`Not_implemented "not implemented" ()
    | Ok (None,query)      -> plp_list_last api query ()
    | Error e              -> respond_error "bad query" ()

end

let handler api events id meth ({path;query;_}:Uri.sep) sock_data headers body =
  let is_guest = Common.User.eq id `Guest in
  match Api.Headers.is_ws headers,meth,path with
  (* WS*)
  | true, `GET, ["mode"]       -> WS.mode       sock_data events body query ()
  | true, `GET, ["lock"]       -> WS.lock       sock_data events body query ()
  | true, `GET, ["measures"]   -> WS.measures   sock_data events body query ()
  | true, `GET, ["parameters"] -> WS.parameters sock_data events body query ()
  | true, `GET, ["plp-list"]   -> WS.plp_list   sock_data events body query ()
  (* HTTP *)
  | false,`POST,["mode"]       -> redirect_if is_guest @@ HTTP.post_mode api body
  | false,`GET, ["mode"]       -> HTTP.mode       api query ()
  | false,`GET, ["lock"]       -> HTTP.lock       api query ()
  | false,`GET, ["measures"]   -> HTTP.measures   api query ()
  | false,`GET, ["parameters"] -> HTTP.parameters api query ()
  | false,`GET, ["plp_list"]   -> HTTP.plp_list   api query ()
  | _ -> not_found ()
