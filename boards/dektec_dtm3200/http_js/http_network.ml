open Board_dektec_dtm3200_types
open Netlib
open Netlib.Uri
open Util

module Event = struct
  let ( >>= ) = Lwt_result.( >>= )

  let get_config sock control =
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "network/config" @/ empty)
      ~query:Query.empty
      control
      nw_of_yojson
      sock
end

let get_config control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "network/config" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % nw_of_yojson))

let get_ip_address control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "network/ip-address" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Ipaddr.V4.of_yojson))

let get_subnet_mask control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "network/subnet-mask" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Ipaddr.V4.of_yojson))

let get_gateway control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "network/gateway" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Ipaddr.V4.of_yojson))

let get_dhcp control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "network/dhcp" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Bool.of_yojson))

let get_mac_address control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "network/mac-address" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Macaddr.of_yojson))

let reboot control =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/board" @/ Int ^/ "network/reboot" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let set_ip_address ip control =
  Api_http.perform
    ~meth:`POST
    ~body:(Ipaddr.V4.to_yojson ip)
    ~path:Path.Format.("api/board" @/ Int ^/ "network/ip-address" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Ipaddr.V4.of_yojson))

let set_subnet_mask mask control =
  Api_http.perform
    ~meth:`POST
    ~body:(Ipaddr.V4.to_yojson mask)
    ~path:Path.Format.("api/board" @/ Int ^/ "network/subnet-mask" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Ipaddr.V4.of_yojson))

let set_gateway gateway control =
  Api_http.perform
    ~meth:`POST
    ~body:(Ipaddr.V4.to_yojson gateway)
    ~path:Path.Format.("api/board" @/ Int ^/ "network/gateway" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Ipaddr.V4.of_yojson))

let set_dhcp dhcp control =
  Api_http.perform
    ~meth:`POST
    ~body:(Util_json.Bool.to_yojson dhcp)
    ~path:Path.Format.("api/board" @/ Int ^/ "network/dhcp" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Bool.of_yojson))
