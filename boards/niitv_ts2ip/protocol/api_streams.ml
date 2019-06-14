open Application_types
open Api_util

module Event = struct
  open Util_react

  let get_streams (api : Protocol.api) incoming _user =
    let event = match incoming with
      | None | Some false -> React.S.changes api.notifs.outgoing_streams
      | Some true -> React.S.changes api.notifs.incoming_streams in
    Lwt.return (E.map Util_json.(List.to_yojson Stream.to_yojson) event)

end

let get_stream (api : Protocol.api) id incoming _user _body _env _state =
  let streams = match incoming with
    | None | Some false -> React.S.value api.notifs.outgoing_streams
    | Some true -> React.S.value api.notifs.incoming_streams in
  match Stream.find_by_id id streams with
  | None -> return_value `Null
  | Some x -> return_value @@ Stream.to_yojson x

let get_streams (api : Protocol.api) incoming _user _body _env _state =
  let streams = match incoming with
    | None | Some false -> React.S.value api.notifs.outgoing_streams
    | Some true -> React.S.value api.notifs.incoming_streams in
  return_value @@ Util_json.(List.to_yojson Stream.to_yojson streams)
