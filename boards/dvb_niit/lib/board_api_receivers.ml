open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction.Json
open Common

module WS = struct

  open Api.Socket

  let to_yojson f (v : int * 'a Time.timestamped) =
    Json.(Pair.to_yojson Int.to_yojson (Time.timestamped_to_yojson f) v)

  let get_measures (events : events) ids _ body sock_data () =
    let e = match ids with
      | [] -> events.measures
      | ids -> React.E.filter (fun (id, _) ->
                   List.mem ~eq:(=) id ids) events.measures
    in
    handler socket_table sock_data e (to_yojson Measure.to_yojson) body

  let get_parameters (events : events) ids _ body sock_data () =
    let e = match ids with
      | [] -> events.params
      | ids -> React.E.filter (fun (id, _) ->
                   List.mem ~eq:(=) id ids) events.params
    in
    handler socket_table sock_data e (to_yojson Params.to_yojson) body

  let get_plps (events : events) ids _ body sock_data () =
    let e = match ids with
      | [] -> events.plps
      | ids -> React.E.filter (fun (id, _) ->
                   List.mem ~eq:(=) id ids) events.plps
    in
    handler socket_table sock_data e (to_yojson Plp_list.to_yojson) body

end

module HTTP = struct

  let get (ids : int list)
        (get : unit -> (int * 'a) list)
        (_to : 'b -> Yojson.Safe.json)
        () =
    let to_yojson =
      Json.(Pair.to_yojson
              Int.to_yojson
              (Time.timestamped_to_yojson _to)) in
    let l = match ids with
      | []  -> get ()
      | ids -> List.filter (fun x -> List.mem ~eq:(=) (fst x) ids) @@ get ()
    in
    Json.List.to_yojson to_yojson l
    |> Result.return
    |> respond_result

  let get_measures (api : api) ids _ _ =
    get ids api.get_measures Measure.to_yojson

  let get_parameters (api : api) ids _ _ =
    get ids api.get_params Params.to_yojson

  let get_plps (api : api) ids _ _ =
    get ids api.get_plp_list Plp_list.to_yojson

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "receivers"
    [ create_ws_handler ~docstring:"Returns receiver measures"
        ~path:Path.Format.("measures" @/ empty)
        ~query:Query.["id", (module List(Int))]
        (WS.get_measures events)
    ; create_ws_handler ~docstring:"Returns DVB-T2 signal parameters"
        ~path:Path.Format.("parameters" @/ empty)
        ~query:Query.["id", (module List(Int))]
        (WS.get_parameters events)
    ; create_ws_handler ~docstring:"Returns available PLPs"
        ~path:Path.Format.("plp-list" @/ empty)
        ~query:Query.["id", (module List(Int))]
        (WS.get_plps events)
    ]
    [ `GET,
      [ create_handler ~docstring:"Returns current measures"
          ~path:Path.Format.("measures" @/ empty)
          ~query:Query.["id", (module List(Int))]
          (HTTP.get_measures api)
      ; create_handler ~docstring:"Returns parameters"
          ~path:Path.Format.("parameters" @/ empty)
          ~query:Query.["id", (module List(Int))]
          (HTTP.get_parameters api)
      ; create_handler ~docstring:"Returns available PLPs"
          ~path:Path.Format.("plp-list" @/ empty)
          ~query:Query.["id", (module List(Int))]
          (HTTP.get_plps api)
      ]
    ]

