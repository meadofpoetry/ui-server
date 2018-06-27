open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect
open Common

module WS = struct

  let mode (events:events) id _ body sock_data () =
    let e = React.E.fmap (fun (x,m) -> if id = x then Some m else None) events.mode in
    sock_handler sock_data e mode_to_yojson body

  let lock (events:events) id _ body sock_data () =
    let e = React.E.fmap (fun (x,l) -> if id = x then Some l else None) events.lock in
    sock_handler sock_data e lock_to_yojson body

  let measures (events:events) id _ body sock_data () =
    let e = React.E.fmap (fun (x,m) -> if id = x then Some m else None) events.measures in
    sock_handler sock_data e measures_to_yojson body

  let parameters (events:events) id _ body sock_data () =
    let e = React.E.fmap (fun (x,p) -> if id = x then Some p else None) events.params in
    sock_handler sock_data e params_to_yojson body

  let plp_list (events:events) id _ body sock_data () =
    let e = React.E.fmap (fun (x,l) -> if id = x then Some l else None) events.plp_list in
    sock_handler sock_data e plp_list_to_yojson body

end

module HTTP = struct

  let post_mode (api:api) id _ body () =
    let to_yojson = Json.(Pair.to_yojson Int.to_yojson mode_rsp_to_yojson) in
    of_body body >>= fun mode ->
    (match mode_of_yojson mode with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok mode -> api.set_mode (id,mode) >|= (to_yojson %> Result.return))
    >>= respond_result

  let get id (get:unit -> (int * 'a) list) (map:'a -> 'b) (_to:'b -> Yojson.Safe.json) () =
    let to_yojson = Json.(Pair.to_yojson Int.to_yojson _to) in
    match List.find_opt (fun x -> (fst x) = id) @@ get () with
    | Some (id,x) -> to_yojson (id,(map x)) |> Result.return |> respond_result
    | None        -> `String "not found" |> Result.fail |> respond_result

  let identity = fun x -> x

  let map_mode (item:config_item) : mode =
    let channel = match item.standard with
      | T2 -> item.t2 | T -> item.t | C -> item.c
    in { standard = item.standard; channel }

  let mode   (api:api) id _ _ = get id api.get_config map_mode mode_to_yojson
  let lock   (api:api) id _ _ = get id api.get_lock identity lock_to_yojson
  let meas   (api:api) id _ _ = get id api.get_measures identity measures_to_yojson
  let params (api:api) id _ _ = get id api.get_params identity params_to_yojson
  let plps   (api:api) id _ _ = get id api.get_plp_list identity plp_list_to_yojson

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
    [ create_ws_handler ~docstring:"Returns receiver mode"
        ~path:Path.Format.(Int ^/ "mode" @/ empty)
        ~query:Query.empty
        (WS.mode events)
    ; create_ws_handler ~docstring:"Returns receiver lock state"
        ~path:Path.Format.(Int ^/ "lock" @/ empty)
        ~query:Query.empty
        (WS.lock events)
    ; create_ws_handler ~docstring:"Returns receiver measures"
        ~path:Path.Format.(Int ^/ "measures" @/ empty)
        ~query:Query.empty
        (WS.measures events)
    ; create_ws_handler ~docstring:"Returns DVB-T2 signal parameters"
        ~path:Path.Format.(Int ^/ "parameters" @/ empty)
        ~query:Query.empty
        (WS.parameters events)
    ; create_ws_handler ~docstring:"Returns available PLPs"
        ~path:Path.Format.(Int ^/ "plp-list" @/ empty)
        ~query:Query.empty
        (WS.plp_list events)
    ]
    [ `POST, [ create_handler ~docstring:"Sets receiver mode"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.(Int ^/ "mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.post_mode api)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns current receiver mode"
                 ~path:Path.Format.(Int ^/ "mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.mode api)
             ; create_handler ~docstring:"Returns whether a signal is locked"
                 ~path:Path.Format.(Int ^/ "lock" @/ empty)
                 ~query:Query.empty
                 (HTTP.lock api)
             ; create_handler ~docstring:"Returns measured parameters"
                 ~path:Path.Format.(Int ^/ "measures" @/ empty)
                 ~query:Query.empty
                 (HTTP.meas api)
             ; create_handler ~docstring:"Returns DVB-T2 signal parameters"
                 ~path:Path.Format.(Int ^/ "parameters" @/ empty)
                 ~query:Query.empty
                 (HTTP.params api)
             ; create_handler ~docstring:"Returns available PLPs"
                 ~path:Path.Format.(Int ^/ "plp-list" @/ empty)
                 ~query:Query.empty
                 (HTTP.plps api)
             (* Archive *)
             ; create_handler ~docstring:"Returns archived receiver mode"
                 ~path:Path.Format.(Int ^/ "mode/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.mode
             ; create_handler ~docstring:"Returns archived measures"
                 ~path:Path.Format.(Int ^/ "measures/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "compress", (module Option(Bool))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.measures
             ; create_handler ~docstring:"Returns archived DVB-T2 signal parameters"
                 ~path:Path.Format.(Int ^/ "parameters/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.parameters
             ]
    ]
