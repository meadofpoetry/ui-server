open Application_types

let ( >>= ) = Lwt.( >>= )

let get_structure (api : Protocol.api) _user _body _env _state =
  api.channel (Request.Get_structure { request_id = 0; stream = `All })
  >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok structure ->
    let item_to_json =
      Util_json.Pair.to_yojson
        Stream.Multi_TS_ID.to_yojson
        Request.structure_to_yojson in
    let to_json = Util_json.List.to_yojson item_to_json in
    Lwt.return (`Value (to_json structure))
