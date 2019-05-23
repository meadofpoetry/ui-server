open Board_dektec_dtm3200_types
open Netlib.Uri
open Util

module Event = struct
  let ( >>= ) = Lwt_result.( >>= )

  let get_status f control =
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "status" @/ empty)
      ~query:Query.empty
      control ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket status_of_yojson (f socket);
    Lwt.return_ok socket
end

let get_status ?timeout control =
  Api_http.perform
    ~path:Path.Format.("api/board" @/ Int ^/ "status" @/ empty)
    ~query:Query.["timeout", (module Option(Int))]
    control timeout
    (ignore_env_bind (Lwt.return % map_err % status_of_yojson))
