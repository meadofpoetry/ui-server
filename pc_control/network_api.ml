open Lwt.Infix
open Pc_control_types

module Event = struct
  open Util_react

  let get_config (network : Network.t) _user =
    let event = match network.extern with
      | None -> E.never
      | Some (_, conf) ->
        E.map Network_config.to_yojson
        @@ S.changes conf#s in
    Lwt.return event
end

let set_config (network : Network.t) _user body _env _state =
  Network_config.of_yojson body
  |> function
    | Error _ -> Lwt.return (`Error "bad config")
    | Ok conf ->
       Network.apply_ext_settings network conf
       >>= function
       | Ok () ->
          Lwt.return `Unit
       | Error (`Network_conf_apply _) ->
          Lwt.return (`Error "failed to set the network config")
       | Error (`No_network_device d) ->
          Lwt.return (`Error (Printf.sprintf "No %s device" d))

let get_config (network : Network.t) _user _body _env _state =
  Network.get_ext_settings network >>= function
  | Error (`No_network_device d) ->
     Lwt.return (`Error (Printf.sprintf "No %s device" d))
  | Ok    r ->
     Lwt.return (`Value (Network_config.to_yojson r))
