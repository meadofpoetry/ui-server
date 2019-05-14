open Application_types
open Board_niitv_tsan_types

let ( >>= ) = Lwt.( >>= )

let get_structure (api : Protocol.api) _user _body _env _state =
  api.channel (Request.Get_structure { request_id = 0; stream = `All })
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok structure ->
    let item_to_json =
      Util_json.Pair.to_yojson
        Stream.Multi_TS_ID.to_yojson
        Structure.to_yojson in
    let to_json = Util_json.List.to_yojson item_to_json in
    Lwt.return (`Value (to_json structure))

let get_section (api : Protocol.api) stream_id table_id
    section table_id_ext id_ext_1 id_ext_2
    _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  (match Stream.find_by_id stream_id streams with
   | Some { orig_id = TS_multi stream_id; _ } ->
     let request_id = Serializer.get_request_id () in
     let req = Request.Get_section { request_id
                                   ; stream_id
                                   ; table_id
                                   ; table_id_ext
                                   ; id_ext_1
                                   ; id_ext_2
                                   ; section
                                   } in
     api.channel req
   | Some _ -> Lwt.return_error Request.(Custom "Unsupported stream format")
   | None -> Lwt.return_error Request.(Custom "Stream not found"))
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok x ->
    let to_json = ts_to_yojson SI_PSI_section.Dump.to_yojson in
    Lwt.return (`Value (to_json x))

let get_streams (api : Protocol.api) _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  let to_json = Util_json.List.to_yojson Stream.to_yojson in
  Lwt.return (`Value (to_json streams))

let get_stream (api : Protocol.api) id _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  let to_json = Util_json.Option.to_yojson Stream.to_yojson in
  let stream = to_json @@ Stream.find_by_id id streams in
  Lwt.return (`Value stream)
