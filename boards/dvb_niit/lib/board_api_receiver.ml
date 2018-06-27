open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect

module WS = struct

  let mode (events:events) ids _ body sock_data () = match ids with
    | []  -> sock_handler sock_data events.mode mode_to_yojson body
    | ids -> let e = React.E.filter (fun (m:mode) -> List.mem ~eq:(=) m.id ids) events.mode in
             sock_handler sock_data e mode_to_yojson body

  let lock (events:events) ids _ body sock_data () = match ids with
    | []  -> sock_handler sock_data events.lock lock_to_yojson body
    | ids -> let e = React.E.filter (fun (l:lock) -> List.mem ~eq:(=) l.id ids) events.lock in
             sock_handler sock_data e lock_to_yojson body

  let measures (events:events) ids _ body sock_data () = match ids with
    | []  -> sock_handler sock_data events.measures measures_to_yojson body
    | ids -> let e = React.E.filter (fun (m:measures) -> List.mem ~eq:(=) m.id ids) events.measures in
             sock_handler sock_data e measures_to_yojson body

  let parameters (events:events) ids _ body sock_data () = match ids with
    | []  -> sock_handler sock_data events.params params_to_yojson body
    | ids -> let e = React.E.filter (fun (p:params) -> List.mem ~eq:(=) p.id ids) events.params in
             sock_handler sock_data e params_to_yojson body

  let plp_list (events:events) ids _ body sock_data () = match ids with
    | []  -> sock_handler sock_data events.plp_list plp_list_to_yojson body
    | ids -> let e = React.E.filter (fun (x:plp_list) -> List.mem ~eq:(=) x.id ids) events.plp_list in
             sock_handler sock_data e plp_list_to_yojson body

end

module HTTP = struct

  let post_mode (api:api) _ body () =
    of_body body >>= fun mode ->
    (match mode_of_yojson mode with
     | Error e -> Lwt_result.fail @@ Json.of_error_string e
     | Ok mode -> api.set_mode mode >|= (mode_rsp_to_yojson %> Result.return))
    >>= Json.respond_result

  let get_one id (get:unit -> 'a list) (get_id:'a -> int)
              (map:'a -> 'b) (to_yojson:'b -> Yojson.Safe.json) () =
    match List.find_opt (fun (x:'a) -> (get_id x) = id) @@ get () with
    | Some x -> to_yojson (map x) |> Result.return |> respond_result
    | None   -> `String "not found" |> Result.fail |> respond_result
  let get_some ids (get:unit -> 'a list) (get_id:'a -> int)
               (map:'a -> 'b) (to_yojson:'b -> Yojson.Safe.json) () =
    let l = match ids with
      | []  -> get ()
      | ids -> List.filter (fun x -> List.mem ~eq:(=) (get_id x) ids) @@ get ()
    in List.map map l |> list_to_yojson to_yojson |> Result.return |> respond_result

  let identity = fun x -> x

  let map_mode ((id,item):int * config_item) : mode =
    let channel = match item.standard with
      | T2 -> item.t2 | T -> item.t | C -> item.c
    in { id; standard = item.standard; channel }

  let mode_one  (api:api) id _ _ = get_one  id api.get_config fst map_mode mode_to_yojson
  let mode_some (api:api) id _ _ = get_some id api.get_config fst map_mode mode_to_yojson

  let lock_one  (api:api) id _ _ = get_one  id api.get_lock (fun x -> x.id) identity lock_to_yojson
  let lock_some (api:api) id _ _ = get_some id api.get_lock (fun x -> x.id) identity lock_to_yojson

  let meas_one  (api:api) id _ _ = get_one  id api.get_measures (fun x -> x.id) identity measures_to_yojson
  let meas_some (api:api) id _ _ = get_some id api.get_measures (fun x -> x.id) identity measures_to_yojson

  let params_one  (api:api) id _ _ = get_one  id api.get_params (fun x -> x.id) identity params_to_yojson
  let params_some (api:api) id _ _ = get_some id api.get_params (fun x -> x.id) identity params_to_yojson

  let plp_list_one  (api:api) id _ _ = get_one  id api.get_plp_list (fun x -> x.id) identity plp_list_to_yojson
  let plp_list_some (api:api) id _ _ = get_some id api.get_plp_list (fun x -> x.id) identity plp_list_to_yojson

  module Archive = struct

    let mode id limit from till duration _ _ () =
      respond_error ~status:`Not_implemented "not implemented" ()

    let measures id limit compress from till duration _ _ () =
      respond_error ~status:`Not_implemented "not implemented" ()

    let parameters id limit from till duration _ _ () =
      respond_error ~status:`Not_implemented "not implemented" ()

  end

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "receiver"
    (let query = Query.[ "id", (module List(Int)) ] in
     [ create_ws_handler ~docstring:"Returns receiver mode"
                         ~path:Path.Format.("mode" @/ empty) ~query (WS.mode events)
     ; create_ws_handler ~docstring:"Returns receiver lock state"
                         ~path:Path.Format.("lock" @/ empty) ~query (WS.lock events)
     ; create_ws_handler ~docstring:"Returns receiver measures"
                         ~path:Path.Format.("measures" @/ empty) ~query (WS.measures events)
     ; create_ws_handler ~docstring:"Returns DVB-T2 signal parameters"
                         ~path:Path.Format.("parameters" @/ empty) ~query (WS.parameters events)
     ; create_ws_handler ~docstring:"Returns available PLPs"
                         ~path:Path.Format.("plp-list" @/ empty) ~query (WS.plp_list events)
    ])
    [ `POST, [ create_handler ~docstring:"Sets receiver mode"
                              ~restrict:[ `Guest ]
                              ~path:Path.Format.("mode" @/ empty) ~query:Query.empty (HTTP.post_mode api)
             ]
    ; `GET,
      ( let one =
          let query = Query.empty in
          [ create_handler ~docstring:"Returns current mode for the selected receiver"
                           ~path:Path.Format.(Int ^/ "mode" @/ empty) ~query (HTTP.mode_one api)
          ; create_handler ~docstring:"Returns current lock state for the selected receiver"
                           ~path:Path.Format.(Int ^/ "lock" @/ empty) ~query (HTTP.lock_one api)
          ; create_handler ~docstring:"Returns measures from the selected receiver"
                           ~path:Path.Format.(Int ^/ "measures" @/ empty) ~query (HTTP.meas_one api)
          ; create_handler ~docstring:"Returns parameters of DVB-T2 signal for the selected receiver"
                           ~path:Path.Format.(Int ^/ "parameters" @/ empty) ~query (HTTP.params_one api)
          ; create_handler ~docstring:"Returns available PLPs for the selected receiver"
                           ~path:Path.Format.(Int ^/ "plp-list" @/ empty) ~query (HTTP.plp_list_one api)
          ]
        in
        let some =
          let query = Query.[ "id", (module List(Int)) ] in
          [ create_handler ~docstring:"Returns current mode"
                           ~path:Path.Format.("mode" @/ empty) ~query (HTTP.mode_some api)
          ; create_handler ~docstring:"Returns current lock state"
                           ~path:Path.Format.("lock" @/ empty) ~query (HTTP.lock_some api)
          ; create_handler ~docstring:"Returns current measures"
                           ~path:Path.Format.("measures" @/ empty) ~query (HTTP.meas_some api)
          ; create_handler ~docstring:"Returns parameters"
                           ~path:Path.Format.("parameters" @/ empty) ~query (HTTP.params_some api)
          ; create_handler ~docstring:"Returns available PLPs"
                           ~path:Path.Format.("plp-list" @/ empty) ~query (HTTP.plp_list_some api)
          ]
        in
        let archive =
          [ create_handler ~docstring:"Returns receiver mode for the requested period"
                           ~path:Path.Format.("mode/archive" @/ empty)
                           ~query:Query.[ "id",       (module List(Int))
                                        ; "limit",    (module Option(Int))
                                        ; "from",     (module Option(Time.Show))
                                        ; "to",       (module Option(Time.Show))
                                        ; "duration", (module Option(Time.Relative)) ]
                           HTTP.Archive.mode
          ; create_handler ~docstring:"Returns measures from the receiver for the requested period"
                           ~path:Path.Format.("measures/archive" @/ empty)
                           ~query:Query.[ "id",       (module List(Int))
                                        ; "limit",    (module Option(Int))
                                        ; "compress", (module Option(Bool))
                                        ; "from",     (module Option(Time.Show))
                                        ; "to",       (module Option(Time.Show))
                                        ; "duration", (module Option(Time.Relative)) ]
                           HTTP.Archive.measures
          ; create_handler ~docstring:"Returns parameters of received DVB-T2 signal for the requested period"
                           ~path:Path.Format.("parameters/archive" @/ empty)
                           ~query:Query.[ "id",       (module List(Int))
                                        ; "limit",    (module Option(Int))
                                        ; "from",     (module Option(Time.Show))
                                        ; "to",       (module Option(Time.Show))
                                        ; "duration", (module Option(Time.Relative)) ]
                           HTTP.Archive.parameters
          ]
        in
        one @ some @ archive
      )
    ]
