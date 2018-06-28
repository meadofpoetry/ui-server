open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect
open Common

module WS = struct

  let to_yojson f v = Json.(Pair.to_yojson Int.to_yojson f v)

  let mode (events:events) ids _ body sock_data () =
    let e = match ids with
      | []  -> events.mode
      | ids -> React.E.filter (fun (id,_) -> List.mem ~eq:(=) id ids) events.mode
    in Api.Socket.handler socket_table sock_data e (to_yojson mode_to_yojson) body

  let lock (events:events) ids _ body sock_data () =
    let e = match ids with
      | []  -> events.lock
      | ids -> React.E.filter (fun (id,_) -> List.mem ~eq:(=) id ids) events.lock
    in Api.Socket.handler socket_table sock_data e (to_yojson lock_to_yojson) body

  let measures (events:events) ids _ body sock_data () =
    let e = match ids with
      | []  -> events.measures
      | ids -> React.E.filter (fun (id,_) -> List.mem ~eq:(=) id ids) events.measures
    in Api.Socket.handler socket_table sock_data e (to_yojson measures_to_yojson) body

  let parameters (events:events) ids _ body sock_data () =
    let e = match ids with
      | []  -> events.params
      | ids -> React.E.filter (fun (id,_) -> List.mem ~eq:(=) id ids) events.params
    in Api.Socket.handler socket_table sock_data e (to_yojson params_to_yojson) body

  let plp_list (events:events) ids _ body sock_data () =
    let e = match ids with
      | []  -> events.plp_list
      | ids -> React.E.filter (fun (id,_) -> List.mem ~eq:(=) id ids) events.plp_list
    in Api.Socket.handler socket_table sock_data e (to_yojson plp_list_to_yojson) body

end

module HTTP = struct

  let get ids (get:unit -> (int * 'a) list) (map:'a -> 'b) (_to:'b -> Yojson.Safe.json) () =
    let to_yojson = Json.(Pair.to_yojson Int.to_yojson _to) in
    let l = match ids with
      | []  -> get ()
      | ids -> List.filter (fun x -> List.mem ~eq:(=) (fst x) ids) @@ get ()
    in List.map (fun (id,x) -> id, map x) l
       |> Json.List.to_yojson to_yojson |> Result.return |> respond_result

  let identity = fun x -> x

  let map_mode (item:config_item) : mode =
    let channel = match item.standard with
      | T2 -> item.t2 | T -> item.t | C -> item.c
    in { standard = item.standard; channel }

  let mode   (api:api) ids _ _ = get ids api.get_config map_mode mode_to_yojson
  let lock   (api:api) ids _ _ = get ids api.get_lock identity lock_to_yojson
  let meas   (api:api) ids _ _ = get ids api.get_measures identity measures_to_yojson
  let params (api:api) ids _ _ = get ids api.get_params identity params_to_yojson
  let plps   (api:api) ids _ _ = get ids api.get_plp_list identity plp_list_to_yojson

  module Archive = struct

    let mode ids limit from till duration _ _ () =
      respond_error ~status:`Not_implemented "not implemented" ()

    let measures ids limit compress from till duration _ _ () =
      respond_error ~status:`Not_implemented "not implemented" ()

    let parameters ids limit from till duration _ _ () =
      respond_error ~status:`Not_implemented "not implemented" ()

  end

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "receivers"
    [ create_ws_handler ~docstring:"Returns receiver mode"
        ~path:Path.Format.("mode" @/ empty)
        ~query:Query.[ "id", (module List(Int)) ]
        (WS.mode events)
    ; create_ws_handler ~docstring:"Returns receiver lock state"
        ~path:Path.Format.("lock" @/ empty)
        ~query:Query.[ "id", (module List(Int)) ]
        (WS.lock events)
    ; create_ws_handler ~docstring:"Returns receiver measures"
        ~path:Path.Format.("measures" @/ empty)
        ~query:Query.[ "id", (module List(Int)) ]
        (WS.measures events)
    ; create_ws_handler ~docstring:"Returns DVB-T2 signal parameters"
        ~path:Path.Format.("parameters" @/ empty)
        ~query:Query.[ "id", (module List(Int)) ]
        (WS.parameters events)
    ; create_ws_handler ~docstring:"Returns available PLPs"
        ~path:Path.Format.("plp-list" @/ empty)
        ~query:Query.[ "id", (module List(Int)) ]
        (WS.plp_list events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns current mode"
                ~path:Path.Format.("mode" @/ empty)
                ~query:Query.[ "id", (module List(Int)) ]
                (HTTP.mode api)
            ; create_handler ~docstring:"Returns current lock state"
                ~path:Path.Format.("lock" @/ empty)
                ~query:Query.[ "id", (module List(Int)) ]
                (HTTP.lock api)
            ; create_handler ~docstring:"Returns current measures"
                ~path:Path.Format.("measures" @/ empty)
                ~query:Query.[ "id", (module List(Int)) ]
                (HTTP.meas api)
            ; create_handler ~docstring:"Returns parameters"
                ~path:Path.Format.("parameters" @/ empty)
                ~query:Query.[ "id", (module List(Int)) ]
                (HTTP.params api)
            ; create_handler ~docstring:"Returns available PLPs"
                ~path:Path.Format.("plp-list" @/ empty)
                ~query:Query.[ "id", (module List(Int)) ]
                (HTTP.plps api)
            (* Archive *)
            ; create_handler ~docstring:"Returns archived receiver mode"
                ~path:Path.Format.("mode/archive" @/ empty)
                ~query:Query.[ "id",       (module List(Int))
                             ; "limit",    (module Option(Int))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.Archive.mode
            ; create_handler ~docstring:"Returns archived measures"
                ~path:Path.Format.("measures/archive" @/ empty)
                ~query:Query.[ "id",       (module List(Int))
                             ; "limit",    (module Option(Int))
                             ; "compress", (module Option(Bool))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.Archive.measures
            ; create_handler ~docstring:"Returns archived DVB-T2 signal parameters"
                ~path:Path.Format.("parameters/archive" @/ empty)
                ~query:Query.[ "id",       (module List(Int))
                             ; "limit",    (module Option(Int))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.Archive.parameters
            ]
    ]

