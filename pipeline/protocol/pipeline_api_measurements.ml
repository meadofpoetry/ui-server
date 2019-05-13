open Application_types
open Pipeline_types
open Qoe_errors

module Event = struct

  open Util_react

  let get_video (api : Protocol.api) stream channel pid _user _body _env state =
    match stream, channel, pid with
    | Some s, Some c, Some p ->
       let pred (x : Video_data.t) =
         x.pid = p
         && x.channel = c
         && Stream.ID.equal x.stream s in
       let event = E.filter pred api.notifs.vdata in
       let event =
         E.aggregate (fun () -> Lwt_unix.sleep 1.0) [event]
         |> E.map (Util_json.List.to_yojson Video_data.to_yojson)
       in
       Lwt.return (`Ev event)
    | Some s, Some c, _ ->
       let pred (x : Video_data.t) =
         x.channel = c
         && Stream.ID.equal x.stream s in
       let event = E.filter pred api.notifs.vdata in
       let event =
         E.aggregate (fun () -> Lwt_unix.sleep 1.0) [event]
         |> E.map (Util_json.List.to_yojson Video_data.to_yojson)
       in
       Lwt.return (`Ev event)
    | Some s, _, _ ->
       let pred (x : Video_data.t) = Stream.ID.equal x.stream s in
       let event = E.filter pred api.notifs.vdata in
       let event =
         E.aggregate (fun () -> Lwt_unix.sleep 1.0) [event]
         |> E.map (Util_json.List.to_yojson Video_data.to_yojson)
       in
       Lwt.return (`Ev event)
    | _ ->
       let event =
         E.aggregate (fun () -> Lwt_unix.sleep 1.0) [api.notifs.vdata]
         |> E.map (Util_json.List.to_yojson Video_data.to_yojson)
       in
       Lwt.return (`Ev event)
       

  let get_audio (api : Protocol.api) stream channel pid _user _body _env state =
    match stream, channel, pid with
    | Some s, Some c, Some p ->
       let pred (x : Audio_data.t) =
         x.pid = p
         && x.channel = c
         && Stream.ID.equal x.stream s in
       let event = E.filter pred api.notifs.adata in
       let event =
         E.aggregate (fun () -> Lwt_unix.sleep 1.0) [event]
         |> E.map (Util_json.List.to_yojson Audio_data.to_yojson)
       in
       Lwt.return (`Ev event)
    | Some s, Some c, _ ->
       let pred (x : Audio_data.t) =
         x.channel = c
         && Stream.ID.equal x.stream s in
       let event = E.filter pred api.notifs.adata in
       let event =
         E.aggregate (fun () -> Lwt_unix.sleep 1.0) [event]
         |> E.map (Util_json.List.to_yojson Audio_data.to_yojson)
       in
       Lwt.return (`Ev event)
    | Some s, _, _ ->
       let pred (x : Audio_data.t) = Stream.ID.equal x.stream s in
       let event = E.filter pred api.notifs.adata in
       let event =
         E.aggregate (fun () -> Lwt_unix.sleep 1.0) [event]
         |> E.map (Util_json.List.to_yojson Audio_data.to_yojson)
       in
       Lwt.return (`Ev event)
    | _ ->
       let event =
         E.aggregate (fun () -> Lwt_unix.sleep 1.0) [api.notifs.adata]
         |> E.map (Util_json.List.to_yojson Audio_data.to_yojson)
       in
       Lwt.return (`Ev event)
  
end
