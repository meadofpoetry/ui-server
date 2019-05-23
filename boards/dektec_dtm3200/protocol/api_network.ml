open Board_dektec_dtm3200_types
open Netlib
open Api_util

module Event = struct
  open Util_react

  let get_config (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.config
      |> E.map (fun x -> nw_to_yojson x.nw) in
    Lwt.return (`Ev event)
end

let get_config (api : Protocol.api) _user _body _env _state =
  return_value
  @@ nw_to_yojson
  @@ (React.S.value api.notifs.config).nw

let get_ip_address (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network (IP_address `R))
  >>=? return_value % Ipaddr.V4.to_yojson

let set_ip_address (api : Protocol.api) _user body _env _state =
  match Ipaddr.V4.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok ip_address ->
    api.channel Request.(Network (IP_address (`W ip_address)))
    >>=? fun ip' ->
    (* NOTE this value is not what we've really set.
       The value will be applied only after reboot. *)
    api.kv#get
    >>= fun cfg -> api.kv#set { cfg with nw = { cfg.nw with ip_address }}
    >>= fun () -> return_value @@ Ipaddr.V4.to_yojson ip'

let get_subnet_mask (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network (Subnet_mask `R))
  >>=? return_value % Ipaddr.V4.to_yojson

let set_subnet_mask (api : Protocol.api) _user body _env _state =
  match Ipaddr.V4.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok mask ->
    api.channel Request.(Network (Subnet_mask (`W mask)))
    >>=? fun mask' -> api.kv#get
    >>= fun cfg -> api.kv#set { cfg with nw = { cfg.nw with mask }}
    >>= fun () -> return_value @@ Ipaddr.V4.to_yojson mask'

let get_gateway (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network (Gateway `R))
  >>=? return_value % Ipaddr.V4.to_yojson

let set_gateway (api : Protocol.api) _user body _env _state =
  match Ipaddr.V4.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok gateway ->
    api.channel Request.(Network (Gateway (`W gateway)))
    >>=? fun gateway' -> api.kv#get
    >>= fun cfg -> api.kv#set { cfg with nw = { cfg.nw with gateway }}
    >>= fun () -> return_value @@ Ipaddr.V4.to_yojson gateway'

let get_dhcp (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network (DHCP `R))
  >>=? return_value % Util_json.Bool.to_yojson

let set_dhcp (api : Protocol.api) _user body _env _state =
  match Util_json.Bool.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok dhcp ->
    api.channel Request.(Network (DHCP (`W dhcp)))
    >>=? fun dhcp' -> api.kv#get
    >>= fun cfg -> api.kv#set { cfg with nw = { cfg.nw with dhcp }}
    >>= fun () -> return_value @@ Util_json.Bool.to_yojson dhcp'

let get_mac_address (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network MAC_address)
  >>=? return_value % Macaddr.to_yojson

let reboot (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network Reboot)
  >>=? fun () -> Lwt.return `Unit
