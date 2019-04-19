open Board_dektec_dtm3200_types
open Netlib

let ( >>= ) = Lwt.( >>= )

module WS = struct
  open Util_react

end

let get (api : Protocol.api) req to_yojson =
  api.channel req
  >>= function
  | Ok x -> Lwt.return @@ `Value (to_yojson x)
  | Error e -> Lwt.return @@ `Error (Request.error_to_string e)

let get_addressing_method (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive (Addressing_method `R)) meth_to_yojson

let get_enable (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive (Enable `R)) Util_json.Bool.to_yojson

let get_fec_delay (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive FEC_delay) Util_json.Int.to_yojson

let get_fec_enable (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive (FEC_enable `R)) Util_json.Bool.to_yojson

let get_fec_columns (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive FEC_columns) Util_json.Int.to_yojson

let get_fec_rows (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive FEC_rows) Util_json.Int.to_yojson

let get_ip_jitter_tolerance (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive IP_jitter_tolerance) Util_json.Int.to_yojson

let get_ip_lost_after_fec (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive IP_lost_after_FEC) Util_json.Int64.to_yojson

let get_ip_lost_before_fec (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive IP_lost_before_FEC) Util_json.Int64.to_yojson

let get_udp_port (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive (UDP_port `R)) Util_json.Int.to_yojson

let get_ip_to_output_delay (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive (IP_to_output_delay `R)) Util_json.Int.to_yojson

let get_multicast_address (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive (Multicast_address `R)) Ipaddr.V4.to_yojson

let get_tp_per_ip (api : Protocol.api) _user _body _env _state =
  get api Request.(IP_receive TP_per_IP) Util_json.Int.to_yojson
