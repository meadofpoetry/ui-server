open Board_niitv_dvb_types
open Application_types

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

module Event = struct
  open Util_react

  let list_to_option = function [] -> None | l -> Some l

  let mem = Boards.Util.List.mem ~eq:Stream.ID.equal

  let filter_if_needed ids event =
    match ids with
    | [] -> event
    | ids -> E.fmap (list_to_option % List.filter (fun (id, _) -> mem id ids)) event

  let to_yojson f (v : (Stream.ID.t * 'a ts) list) =
    Util_json.(
      List.to_yojson (Pair.to_yojson Stream.ID.to_yojson (ts_to_yojson f)) v)

  let map_event (source_id : int)
        (streams : Stream.t list signal)
        (event : (int * 'a) list event) =
    S.sample (fun (data : (int * 'a) list)
                  (streams : Stream.t list) ->
        Boards.Util.List.filter_map (fun (id, x) ->
            let multi_id = Stream.Multi_TS_ID.make
                             ~source_id
                             ~stream_id:id in
            match Stream.find_by_multi_id multi_id streams with
            | None -> None
            | Some s -> Some (s.id, x)) data) event streams
    |> E.fmap list_to_option

  let get_measures (api : Protocol.api) (ids : Stream.ID.t list)
        _user _body _env state =
    let event =
      map_event api.source_id api.notifs.streams api.notifs.measures
      |> filter_if_needed ids
      |> E.map (to_yojson Measure.to_yojson) in
    Lwt.return (`Ev (state, event))

  let get_parameters (api : Protocol.api) (ids : Stream.ID.t list)
        _user _body _env state =
    let event =
      map_event api.source_id api.notifs.streams api.notifs.params
      |> filter_if_needed ids
      |> E.map (to_yojson Params.to_yojson) in
    Lwt.return (`Ev (state, event))

  let get_plps (api : Protocol.api) (ids : Stream.ID.t list)
        _user _body _env state =
    let event =
      map_event api.source_id api.notifs.streams api.notifs.plps
      |> filter_if_needed ids
      |> E.map (to_yojson Plp_list.to_yojson) in
    Lwt.return (`Ev (state, event))

  let get_streams (api : Protocol.api) (ids : Stream.ID.t list)
        _user _body _env state =
    let event = match ids with
      | [] ->
         api.notifs.streams
         |> S.changes
         |> E.map Util_json.(List.to_yojson Stream.to_yojson)
      | ids ->
         api.notifs.streams
         |> S.changes
         |> E.fmap (list_to_option % List.filter (fun (s : Stream.t) -> mem s.id ids))
         |> E.map Util_json.(List.to_yojson Stream.to_yojson) in
    Lwt.return (`Ev (state, event))
end
