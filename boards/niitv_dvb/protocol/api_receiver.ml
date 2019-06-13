open Board_niitv_dvb_types
open Application_types
open Api_util

module Event = struct
  open Util_react

  let list_to_option = function [] -> None | l -> Some l

  let filter_if_needed ids event =
    match ids with
    | [] -> event
    | ids -> E.fmap (list_to_option % List.filter (fun (id, _) -> List.mem id ids)) event

  let to_json f (v : (int * 'a ts) list) =
    Util_json.(
      List.to_yojson (Pair.to_yojson Int.to_yojson (ts_to_yojson f)) v)

  let get_measurements (api : Protocol.api) (ids : int list) _user =
    let event =
      api.notifs.measures
      |> filter_if_needed ids
      |> E.map (to_json Measure.to_yojson) in
    Lwt.return event

  let get_parameters (api : Protocol.api) (ids : int list) _user =
    let event =
      api.notifs.params
      |> filter_if_needed ids
      |> E.map (to_json Params.to_yojson) in
    Lwt.return event

  let get_plp_list (api : Protocol.api) (ids : int list) _user =
    let event =
      api.notifs.plps
      |> filter_if_needed ids
      |> E.map (to_json Plp_list.to_yojson) in
    Lwt.return event
end

let to_json f x =
  Util_json.(Pair.to_yojson Int.to_yojson (ts_to_yojson f)) x

let set_mode (api : Protocol.api) id _user body _env _state =
  match Device.mode_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok mode ->
    api.channel (Set_mode (id, mode))
    >>=? fun x -> api.kv#get
    >>= fun config ->
    let mode = Boards.Util.List.Assoc.set ~eq:(=) id mode config.mode in
    api.kv#set { config with mode }
    >>= fun () -> return_value @@ Device.mode_rsp_to_yojson @@ snd x

let get_mode (api : Protocol.api) id _user _body _env _state =
  let value =
    List.find_opt (fun (id', _) -> id = id')
    @@ (React.S.value api.notifs.config).mode in
  let to_yojson = Util_json.(
      Option.to_yojson
      @@ Pair.to_yojson Int.to_yojson Device.mode_to_yojson) in
  return_value @@ to_yojson value

let get_stream (api : Protocol.api) (id : int) _user _body _env state =
  api.kv#get
  >>= fun { source; _ } ->
  let stream =
    Api_stream.find_stream_by_receiver_id
      ~source_id:source id
      (React.S.value api.notifs.streams) in
  return_value @@ Util_json.(Option.to_yojson Stream.to_yojson stream)

let get_measurements (api : Protocol.api) (id : int) _user _body _env _state =
  api.channel (Request.Get_measure id)
  >>=? return_value % to_json Measure.to_yojson

let get_parameters (api : Protocol.api) (id : int) _user _body _env _state =
  api.channel (Request.Get_params id)
  >>=? return_value % to_json Params.to_yojson

let get_plp_list (api : Protocol.api) (id : int) _user _body _env _state =
  api.channel (Request.Get_plp_list id)
  >>=? return_value % to_json Plp_list.to_yojson
