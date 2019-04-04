open Board_niitv_dvb_types

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

module Event = struct
  open Util_react

  let list_to_option = function [] -> None | l -> Some l

  let filter_if_needed ids event =
    match ids with
    | [] -> event
    | ids -> E.fmap (list_to_option % List.filter (fun (id, _) -> List.mem id ids)) event

  let to_yojson f (v : (int * 'a ts) list) =
    Util_json.(
      List.to_yojson (Pair.to_yojson Int.to_yojson (ts_to_yojson f)) v)

  let get_measures (api : Protocol.api) (ids : int list) _user _body _env state =
    let event =
      api.notifs.measures
      |> filter_if_needed ids
      |> E.map (to_yojson Measure.to_yojson) in
    Lwt.return (`Ev (state, event))

  let get_parameters (api : Protocol.api) (ids : int list) _user _body _env state =
    let event =
      api.notifs.params
      |> filter_if_needed ids
      |> E.map (to_yojson Params.to_yojson) in
    Lwt.return (`Ev (state, event))

  let get_plps (api : Protocol.api) (ids : int list) _user _body _env state =
    let event =
      api.notifs.plps
      |> filter_if_needed ids
      |> E.map (to_yojson Plp_list.to_yojson) in
    Lwt.return (`Ev (state, event))
end


