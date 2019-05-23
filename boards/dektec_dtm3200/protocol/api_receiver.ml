open Board_dektec_dtm3200_types
open Netlib
open Api_util

module Event = struct
  open Util_react

  let get_config (api : Protocol.api) _user _body _env _state =
    let event =
      S.changes api.notifs.config
      |> E.map (fun x -> ip_receive_to_yojson x.ip_receive) in
    Lwt.return (`Ev event)
end

let get (api : Protocol.api) req to_yojson =
  api.channel (Request.IP_receive req)
  >>=? return_value % to_yojson

let get_addressing_method (api : Protocol.api) _user _body _env _state =
  get api Request.(Addressing_method `R) meth_to_yojson

let set_addressing_method (api : Protocol.api) _user body _env _state =
  match meth_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
    api.channel Request.(IP_receive (Addressing_method (`W x)))
    >>=? fun (addressing_method : meth) -> api.kv#get
    >>= fun cfg ->
    api.kv#set { cfg with ip_receive = { cfg.ip_receive with addressing_method }}
    >>= fun () -> Lwt.return @@ `Value (meth_to_yojson addressing_method)

let get_enable (api : Protocol.api) _user _body _env _state =
  get api Request.(Enable `R) Util_json.Bool.to_yojson

let set_enable (api : Protocol.api) _user body _env _state =
  match Util_json.Bool.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
    api.channel Request.(IP_receive (Enable (`W x)))
    >>=? fun enable -> api.kv#get
    >>= fun cfg -> api.kv#set { cfg with ip_receive = { cfg.ip_receive with enable }}
    >>= fun () -> Lwt.return @@ `Value (Util_json.Bool.to_yojson enable)

let get_fec_delay (api : Protocol.api) _user _body _env _state =
  get api Request.(FEC_delay) Util_json.Int.to_yojson

let get_fec_enable (api : Protocol.api) _user _body _env _state =
  get api Request.(FEC_enable `R) Util_json.Bool.to_yojson

let set_fec_enable (api : Protocol.api) _user body _env _state =
  match Util_json.Bool.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
    api.channel Request.(IP_receive (FEC_enable (`W x)))
    >>=? fun fec_enable -> api.kv#get
    >>= fun cfg -> api.kv#set { cfg with ip_receive = { cfg.ip_receive with fec_enable }}
    >>= fun () -> Lwt.return @@ `Value (Util_json.Bool.to_yojson fec_enable)

let get_fec_columns (api : Protocol.api) _user _body _env _state =
  get api Request.(FEC_columns) Util_json.Int.to_yojson

let get_fec_rows (api : Protocol.api) _user _body _env _state =
  get api Request.(FEC_rows) Util_json.Int.to_yojson

let get_ip_jitter_tolerance (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_jitter_tolerance) Util_json.Int.to_yojson

let get_ip_lost_after_fec (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_lost_after_FEC) Util_json.Int64.to_yojson

let get_ip_lost_before_fec (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_lost_before_FEC) Util_json.Int64.to_yojson

let get_udp_port (api : Protocol.api) _user _body _env _state =
  get api Request.(UDP_port `R) Util_json.Int.to_yojson

let set_udp_port (api : Protocol.api) _user body _env _state =
  match Util_json.Int.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
    api.channel Request.(IP_receive (UDP_port (`W x)))
    >>=? fun udp_port ->
    api.kv#get
    >>= fun cfg ->
    api.kv#set { cfg with ip_receive = { cfg.ip_receive with udp_port }}
    >>= fun () -> Lwt.return @@ `Value (Util_json.Int.to_yojson udp_port)

let get_ip_to_output_delay (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_to_output_delay `R) Util_json.Int.to_yojson

let set_ip_to_output_delay (api : Protocol.api) _user body _env _state =
  match Util_json.Int.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
    api.channel Request.(IP_receive (IP_to_output_delay (`W x)))
    >>=? fun ip_to_output_delay -> api.kv#get
    >>= fun cfg ->
    api.kv#set { cfg with ip_receive = { cfg.ip_receive with ip_to_output_delay }}
    >>= fun () -> Lwt.return @@ `Value (Util_json.Int.to_yojson ip_to_output_delay)

let get_multicast_address (api : Protocol.api) _user _body _env _state =
  get api Request.(Multicast_address `R) Ipaddr.V4.to_yojson

let set_multicast_address (api : Protocol.api) _user body _env _state =
  match Ipaddr.V4.of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
    api.channel Request.(IP_receive (Multicast_address (`W x)))
    >>=? fun multicast -> api.kv#get
    >>= fun cfg -> api.kv#set { cfg with ip_receive = { cfg.ip_receive with multicast }}
    >>= fun () -> Lwt.return @@ `Value (Ipaddr.V4.to_yojson multicast)

let get_tp_per_ip (api : Protocol.api) _user _body _env _state =
  get api Request.(TP_per_IP) Util_json.Int.to_yojson

let get_status (api : Protocol.api) _user _body _env _state =
  get api Request.(Status) state_to_yojson

let get_protocol (api : Protocol.api) _user _body _env _state =
  get api Request.(Protocol) protocol_to_yojson

let get_index (api : Protocol.api) _user _body _env _state =
  get api Request.(Index) Util_json.Int32.to_yojson

let get_output_type (api : Protocol.api) _user _body _env _state =
  get api Request.(Output_type) output_to_yojson

let get_packet_size (api : Protocol.api) _user _body _env _state =
  get api Request.(Packet_size) packet_sz_to_yojson

let get_bitrate (api : Protocol.api) _user _body _env _state =
  get api Request.(Bitrate) Util_json.Int.to_yojson

let get_pcr_present (api : Protocol.api) _user _body _env _state =
  get api Request.(PCR_present) Util_json.Bool.to_yojson

let get_rate_change_counter (api : Protocol.api) _user _body _env _state =
  get api Request.(Rate_change_counter) Util_json.Int32.to_yojson

let get_rate_estimation_mode (api : Protocol.api) _user _body _env _state =
  get api Request.(Rate_estimation_mode `R) rate_mode_to_yojson

let set_rate_estimation_mode (api : Protocol.api) _user body _env _state =
  match rate_mode_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok x ->
    api.channel Request.(IP_receive (Rate_estimation_mode (`W x)))
    >>=? fun rate_mode -> api.kv#get
    >>= fun cfg -> api.kv#set { cfg with ip_receive = { cfg.ip_receive with rate_mode }}
    >>= fun () -> Lwt.return @@ `Value (rate_mode_to_yojson rate_mode)

let get_jitter_error_counter (api : Protocol.api) _user _body _env _state =
  get api Request.(Jitter_error_counter) Util_json.Int32.to_yojson

let get_lock_error_counter (api : Protocol.api) _user _body _env _state =
  get api Request.(Lock_error_counter) Util_json.Int32.to_yojson

let get_delay_factor (api : Protocol.api) _user _body _env _state =
  get api Request.(Delay_factor) Util_json.Int32.to_yojson
