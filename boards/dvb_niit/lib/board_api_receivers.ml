open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction.Json
open Common

module WS = struct

  open Api.Socket

  let to_yojson f v = Json.(Pair.to_yojson Int.to_yojson f v)

  let measures (events : events) ids _ body sock_data () =
    let e = match ids with
      | [] -> events.measures
      | ids -> React.E.filter (fun (id, _) ->
                   List.mem ~eq:(=) id ids) events.measures
    in
    handler socket_table sock_data e (to_yojson Measure.to_yojson) body

  let parameters (events : events) ids _ body sock_data () =
    let e = match ids with
      | [] -> events.params
      | ids -> React.E.filter (fun (id, _) ->
                   List.mem ~eq:(=) id ids) events.params
    in
    handler socket_table sock_data e (to_yojson Params.to_yojson) body

  let plp_list (events : events) ids _ body sock_data () =
    let e = match ids with
      | [] -> events.plp_list
      | ids -> React.E.filter (fun (id, _) ->
                   List.mem ~eq:(=) id ids) events.plp_list
    in
    handler socket_table sock_data e (to_yojson Plp_list.to_yojson) body

end

module HTTP = struct

  let get (ids : int list)
        (get : unit -> (int * 'a) list)
        (map : 'a -> 'b)
        (_to : 'b -> Yojson.Safe.json)
        () =
    let to_yojson = Json.(Pair.to_yojson Int.to_yojson _to) in
    let l = match ids with
      | []  -> get ()
      | ids -> List.filter (fun x -> List.mem ~eq:(=) (fst x) ids) @@ get ()
    in List.map (fun (id,x) -> id, map x) l
       |> Json.List.to_yojson to_yojson |> Result.return |> respond_result

  let meas (api : api) ids _ _ =
    get ids api.get_measures Fun.id Measure.to_yojson

  let params (api : api) ids _ _ =
    get ids api.get_params Fun.id Params.to_yojson

  let plps (api : api) ids _ _ =
    get ids api.get_plp_list Fun.id Plp_list.to_yojson

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "receivers"
    [ create_ws_handler ~docstring:"Returns receiver measures"
        ~path:Path.Format.("measures" @/ empty)
        ~query:Query.["id", (module List(Int))]
        (WS.measures events)
    ; create_ws_handler ~docstring:"Returns DVB-T2 signal parameters"
        ~path:Path.Format.("parameters" @/ empty)
        ~query:Query.["id", (module List(Int))]
        (WS.parameters events)
    ; create_ws_handler ~docstring:"Returns available PLPs"
        ~path:Path.Format.("plp-list" @/ empty)
        ~query:Query.["id", (module List(Int))]
        (WS.plp_list events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns current measures"
                ~path:Path.Format.("measures" @/ empty)
                ~query:Query.["id", (module List(Int))]
                (HTTP.meas api)
            ; create_handler ~docstring:"Returns parameters"
                ~path:Path.Format.("parameters" @/ empty)
                ~query:Query.["id", (module List(Int))]
                (HTTP.params api)
            ; create_handler ~docstring:"Returns available PLPs"
                ~path:Path.Format.("plp-list" @/ empty)
                ~query:Query.["id", (module List(Int))]
                (HTTP.plps api)
            ]
    ]

