open Board_dektec_dtm3200_types
open Netlib
open Netlib.Uri
open Util

module Event = struct
  let ( >>= ) = Lwt_result.( >>= )

  let get_config sock control =
    Api_websocket.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "receiver/config" @/ empty)
      ~query:Query.empty
      control ip_receive_of_yojson sock
end

let get_config control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/config" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % ip_receive_of_yojson))

let get_addressing_method control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/addressing-method" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % meth_of_yojson))

let set_addressing_method meth control =
  Api_http.perform
    ~meth:`POST
    ~body:(meth_to_yojson meth)
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/addressing-method" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % meth_of_yojson))

let get_enable control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/enabled" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Bool.of_yojson))

let set_enable enable control =
  Api_http.perform
    ~meth:`POST
    ~body:(Util_json.Bool.to_yojson enable)
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/enabled" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Bool.of_yojson))

let get_fec_delay control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/fec/delay" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_fec_enable control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/fec/enabled" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Bool.of_yojson))

let set_fec_enable enable control =
  Api_http.perform
    ~meth:`POST
    ~body:(Util_json.Bool.to_yojson enable)
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/fec/enabled" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Bool.of_yojson))

let get_fec_columns control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/fec/columns" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_fec_rows control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/fec/rows" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_ip_jitter_tolerance control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/ip-jitter-tolerance" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_ip_lost_after_fec control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/ip-lost-after-fec" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int64.of_yojson))

let get_ip_lost_before_fec control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/ip-lost-before-fec" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int64.of_yojson))

let get_udp_port control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/udp-port" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let set_udp_port port control =
  Api_http.perform
    ~meth:`POST
    ~body:(Util_json.Int.to_yojson port)
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/udp-port" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_ip_to_output_delay control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/ip-to-output-delay" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let set_ip_to_output_delay delay control =
  Api_http.perform
    ~meth:`POST
    ~body:(Util_json.Int.to_yojson delay)
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/ip-to-output-delay" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_multicast_address control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/multicast-address" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Ipaddr.V4.of_yojson))

let set_multicast_address address control =
  Api_http.perform
    ~meth:`POST
    ~body:(Ipaddr.V4.to_yojson address)
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/multicast-address" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Ipaddr.V4.of_yojson))

let get_status control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/status" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % state_of_yojson))

let get_protocol control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/protocol" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % protocol_of_yojson))

let get_index control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/index" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_output_type control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/output-type" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % output_of_yojson))

let get_packet_size control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/packet-size" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % packet_sz_of_yojson))

let get_bitrate control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/bitrate" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_pcr_present control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/pcr-present" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Bool.of_yojson))

let get_rate_change_counter control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/rate-change-counter" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int32.of_yojson))

let get_rate_estimation_mode control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/rate-estimation-mode" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % rate_mode_of_yojson))

let set_rate_estimation_mode mode control =
  Api_http.perform
    ~meth:`POST
    ~body:(rate_mode_to_yojson mode)
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/rate-estimation-mode" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % rate_mode_of_yojson))

let get_jitter_error_counter control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/jitter-error-counter" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int32.of_yojson))

let get_lock_error_counter control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/lock-error-counter" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int32.of_yojson))

let get_delay_factor control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receiver/delay-factor" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int32.of_yojson))
