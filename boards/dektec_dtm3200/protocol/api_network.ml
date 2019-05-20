open Board_dektec_dtm3200_types
open Netlib

let ( >>= ) = Lwt.( >>= )

let ( >>=? ) x f =
  x >>= function
  | Error e -> Lwt.return (`Error (Request.error_to_string e))
  | Ok x -> f x

module Event = struct
  open Util_react

  let get_config (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.config
      |> E.map (fun x -> nw_to_yojson x.nw) in
    Lwt.return (`Ev event)
end

let get_config (api : Protocol.api) _user _body _env _state =
  let value = (React.S.value api.notifs.config).nw in
  Lwt.return (`Value (nw_to_yojson value))

let get_ip_address (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network (IP_address `R))
  >>=? fun x -> Lwt.return @@ `Value (Ipaddr.V4.to_yojson x)

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
    >>= fun () -> Lwt.return @@ `Value (Ipaddr.V4.to_yojson ip')

let get_subnet_mask (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network (Subnet_mask `R))
  >>=? fun x -> Lwt.return @@ `Value (Ipaddr.V4.to_yojson x)

let set_subnet_mask (api : Protocol.api) _user body _env _state =
  match Ipaddr.V4.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok mask ->
    api.channel Request.(Network (Subnet_mask (`W mask)))
    >>=? fun mask' ->
    api.kv#get
    >>= fun cfg -> api.kv#set { cfg with nw = { cfg.nw with mask }}
    >>= fun () -> Lwt.return @@ `Value (Ipaddr.V4.to_yojson mask')

let get_gateway (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network (Gateway `R))
  >>=? fun x -> Lwt.return @@ `Value (Ipaddr.V4.to_yojson x)

let set_gateway (api : Protocol.api) _user body _env _state =
  match Ipaddr.V4.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok gateway ->
    api.channel Request.(Network (Gateway (`W gateway)))
    >>=? fun gateway' ->
    api.kv#get
    >>= fun cfg -> api.kv#set { cfg with nw = { cfg.nw with gateway }}
    >>= fun () -> Lwt.return @@ `Value (Ipaddr.V4.to_yojson gateway')

let get_dhcp (api : Protocol.api) _user _body _env _state =
  api.channel Request.(Network (DHCP `R))
  >>=? fun x -> Lwt.return @@ `Value (Util_json.Bool.to_yojson x)

let set_dhcp (api : Protocol.api) _user body _env _state =
  match Util_json.Bool.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok dhcp ->
    api.channel Request.(Network (DHCP (`W dhcp)))
    >>=? fun dhcp' ->
    api.kv#get
    >>= fun cfg -> api.kv#set { cfg with nw = { cfg.nw with dhcp }}
    >>= fun () -> Lwt.return @@ `Value (Util_json.Bool.to_yojson dhcp')
