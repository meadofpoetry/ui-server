open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect

module WS = struct

  let mode (events:events) id _ body sock_data () = match id with
    | Some id ->
       let e = React.E.filter (fun (m:mode) -> m.id = id) events.mode in
       sock_handler sock_data e mode_to_yojson body
    | None -> sock_handler sock_data events.mode mode_to_yojson body

  let lock (events:events) id _ body sock_data () = match id with
    | Some id ->
       let e = React.E.filter (fun (l:lock) -> l.id = id) events.lock in
       sock_handler sock_data e lock_to_yojson body
    | None -> sock_handler sock_data events.lock lock_to_yojson body

  let measures (events:events) id _ body sock_data () = match id with
    | Some id ->
       let e = React.E.filter (fun (m:measures) -> m.id = id) events.measures in
       sock_handler sock_data e measures_to_yojson body
    | None -> sock_handler sock_data events.measures measures_to_yojson body

  let parameters (events:events) id _ body sock_data () = match id with
    | Some id ->
       let e = React.E.filter (fun (p:params) -> p.id = id) events.params in
       sock_handler sock_data e params_to_yojson body
    | None -> sock_handler sock_data events.params params_to_yojson body

  let plp_list (events:events) id _ body sock_data () = match id with
    | Some id ->
       let e = React.E.filter (fun (x:plp_list) -> x.id = id) events.plp_list in
       sock_handler sock_data e plp_list_to_yojson body
    | None -> sock_handler sock_data events.plp_list plp_list_to_yojson body

end

module HTTP = struct

  let post_mode (api:api) _ body () =
    of_body body >>= fun mode ->
    (match mode_of_yojson mode with
     | Error e -> Lwt_result.fail @@ Json.of_error_string e
     | Ok mode -> api.set_mode mode >|= (mode_rsp_to_yojson %> Result.return))
    >>= Json.respond_result

  let mode (api:api) id _ _ () =
    let channel (x:config_item) = match x.standard with
      | T2 -> x.t2 | T -> x.t | C -> x.c
    in
    let config = api.get_config () in
    match id with
    | Some id -> (match List.find_opt (fun (id',_) -> id = id') config with
                  | Some (_,({standard;_} as x)) ->
                     let mode = { id; standard; channel = channel x } in
                     respond_result (Ok (mode_to_yojson mode))
                  | None -> (`String "no receiver found") |> Result.fail |> respond_result)
    | None -> List.map (fun (id,({standard;_} as x)) -> { id; standard; channel = channel x }) config
              |> modes_to_yojson |> Result.return |> respond_result
  let mode_one (api:api) id hdrs body () =
    mode api (Some id) hdrs body ()
  let mode_all (api:api) hdrs body () =
    mode api None hdrs body ()

  let get_last ?id (get:unit -> 'a list) (get_id:'a -> int)
               (one_to_yojson:'a -> Yojson.Safe.json) (all_to_yojson:'a list -> Yojson.Safe.json)
               () = match id with
    | None    -> all_to_yojson @@ get () |> Result.return |> respond_result
    | Some id -> (match List.find_opt (fun (x:'a) -> (get_id x) = id) @@ get () with
                  | Some x -> one_to_yojson x     |> Result.return |> respond_result
                  | None   -> `String "not found" |> Result.fail   |> respond_result)

  let lock ?id (api:api) _ _ () =
    get_last ?id api.get_lock (fun x -> x.id) lock_to_yojson lock_all_to_yojson ()
  let lock_one (api:api) id hdrs body = lock ~id api hdrs body
  let lock_all (api:api) hdrs body    = lock api hdrs body

  let measures ?id (api:api) _ _ () =
    get_last ?id api.get_measures (fun x -> x.id) measures_to_yojson measures_all_to_yojson ()
  let measures_one (api:api) id hdrs body = measures ~id api hdrs body
  let measures_all (api:api) hdrs body    = measures api hdrs body

  let parameters ?id (api:api) _ _ () =
    get_last ?id api.get_params (fun x -> x.id) params_to_yojson params_all_to_yojson ()
  let parameters_one (api:api) id hdrs body = parameters ~id api hdrs body
  let parameters_all (api:api) hdrs body    = parameters api hdrs body

  let plp_list ?id (api:api) _ _ () =
    get_last ?id api.get_plp_list (fun x -> x.id) plp_list_to_yojson plp_list_all_to_yojson ()
  let plp_list_one (api:api) id hdrs body = plp_list ~id api hdrs body
  let plp_list_all (api:api) hdrs body    = plp_list api hdrs body

  module Archive = struct

    let mode id from till duration _ _ () =
      not_found ()

    let lock id from till duration _ _ () =
      not_found ()

    let measures id from till duration _ _ () =
      not_found ()

    let parameters id from till duration _ _ () =
      not_found ()

  end

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "receiver"
    [ create_ws_handler ~docstring:"Returns receiver mode"
                        ~path:Path.Format.("mode" @/ empty)
                        ~query:Query.["id", (module Option(Int))]
                        (WS.mode events)
    ; create_ws_handler ~docstring:"Returns receiver lock state"
                        ~path:Path.Format.("lock" @/ empty)
                        ~query:Query.["id", (module Option(Int))]
                        (WS.lock events)
    ; create_ws_handler ~docstring:"Returns receiver measures"
                        ~path:Path.Format.("measures" @/ empty)
                        ~query:Query.["id", (module Option(Int))]
                        (WS.measures events)
    ; create_ws_handler ~docstring:"Returns DVB-T2 signal parameters"
                        ~path:Path.Format.("parameters" @/ empty)
                        ~query:Query.["id", (module Option(Int))]
                        (WS.parameters events)
    ; create_ws_handler ~docstring:"Returns available PLPs"
                        ~path:Path.Format.("plp-list" @/ empty)
                        ~query:Query.["id", (module Option(Int))]
                        (WS.plp_list events)
    ]
    [ `POST, [ create_handler ~docstring:"Sets receiver mode"
                              ~path:Path.Format.("mode" @/ empty)
                              ~query:Query.empty
                              (HTTP.post_mode api)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns current mode for the selected receiver"
                              ~path:Path.Format.("mode" @/ Int ^/ empty)
                              ~query:Query.empty
                              (HTTP.mode_one api)
             ; create_handler ~docstring:"Returns current lock state for the selected receiver"
                              ~path:Path.Format.("lock" @/ Int ^/ empty)
                              ~query:Query.empty
                              (HTTP.lock_one api)
             ; create_handler ~docstring:"Returns measures from the selected receiver"
                              ~path:Path.Format.("measures" @/ Int ^/ empty)
                              ~query:Query.empty
                              (HTTP.measures_one api)
             ; create_handler ~docstring:"Returns parameters of DVB-T2 signal for the selected receiver"
                              ~path:Path.Format.("parameters" @/ Int ^/ empty)
                              ~query:Query.empty
                              (HTTP.parameters_one api)
             ; create_handler ~docstring:"Returns available PLPs for the selected receiver"
                              ~path:Path.Format.("plp-list" @/ Int ^/ empty)
                              ~query:Query.empty
                              (HTTP.plp_list_one api)
             (* Group of receivers *)
             ; create_handler ~docstring:"Returns current mode for all receivers"
                              ~path:Path.Format.("mode" @/ empty)
                              ~query:Query.empty
                              (HTTP.mode_all api)
             ; create_handler ~docstring:"Returns current lock state for all receivers"
                              ~path:Path.Format.("lock" @/ empty)
                              ~query:Query.empty
                              (HTTP.lock_all api)
             ; create_handler ~docstring:"Returns measures from all receivers"
                              ~path:Path.Format.("measures" @/ empty)
                              ~query:Query.empty
                              (HTTP.measures_all api)
             ; create_handler ~docstring:"Returns parameters of DVB-T2 signal for all receivers"
                              ~path:Path.Format.("parameters" @/ empty)
                              ~query:Query.empty
                              (HTTP.parameters_all api)
             ; create_handler ~docstring:"Returns available PLPs for all receivers"
                              ~path:Path.Format.("plp-list" @/ empty)
                              ~query:Query.empty
                              (HTTP.plp_list_all api)
             (* Archive *)
             ; create_handler ~docstring:"Returns receiver mode for the requested period"
                              ~path:Path.Format.("mode/archive" @/ empty)
                              ~query:Query.[ "id",      (module Option(Int))
                                           ; "from",    (module Option(Time.Show))
                                           ; "to",      (module Option(Time.Show))
                                           ; "duration",(module Option(Time.Relative)) ]
                              HTTP.Archive.mode
             ; create_handler ~docstring:"Returns current receiver lock states for the requested period"
                              ~path:Path.Format.("lock/archive" @/ empty)
                              ~query:Query.[ "id",      (module Option(Int))
                                           ; "from",    (module Option(Time.Show))
                                           ; "to",      (module Option(Time.Show))
                                           ; "duration",(module Option(Time.Relative)) ]
                              HTTP.Archive.lock
             ; create_handler ~docstring:"Returns measures from the receiver for the requested period"
                              ~path:Path.Format.("measures/archive" @/ empty)
                              ~query:Query.[ "id",      (module Option(Int))
                                           ; "from",    (module Option(Time.Show))
                                           ; "to",      (module Option(Time.Show))
                                           ; "duration",(module Option(Time.Relative)) ]
                              HTTP.Archive.measures
             ; create_handler ~docstring:"Returns parameters of received DVB-T2 signal for the requested period"
                              ~path:Path.Format.("parameters/archive" @/ empty)
                              ~query:Query.[ "id",      (module Option(Int))
                                           ; "from",    (module Option(Time.Show))
                                           ; "to",      (module Option(Time.Show))
                                           ; "duration",(module Option(Time.Relative)) ]
                              HTTP.Archive.parameters
             ]
    ]
