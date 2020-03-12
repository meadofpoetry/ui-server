open Board_dektec_dtm3200_types
open Api_util

let set_packet_size (api : Protocol.api) _user body _env _state =
  match asi_packet_sz_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok size ->
      api.channel Request.(ASI_output (Packet_size (`W size)))
      >>=? return_value % asi_packet_sz_to_yojson

let get_packet_size (api : Protocol.api) _user _body _env _state =
  api.channel Request.(ASI_output (Packet_size `R))
  >>=? return_value % asi_packet_sz_to_yojson

let get_physical_port (api : Protocol.api) _user _body _env _state =
  api.channel Request.(ASI_output Physical_port)
  >>=? return_value % Util_json.Int.to_yojson

let get_bitrate (api : Protocol.api) _user _body _env _state =
  api.channel Request.(ASI_output Bitrate)
  >>=? return_value % Util_json.Int.to_yojson
