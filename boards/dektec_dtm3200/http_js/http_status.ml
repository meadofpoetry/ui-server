open Board_dektec_dtm3200_types
open Netlib.Uri
open Util

module Event = struct
  let ( >>= ) = Lwt_result.( >>= )

  let get_status sock control =
    Api_websocket.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "status" @/ empty)
      ~query:Query.empty
      control status_of_yojson sock
end

let get_status ?timeout control =
  Api_http.perform
    ~path:Path.Format.("api/board" @/ Int ^/ "status" @/ empty)
    ~query:Query.["timeout", (module Option(Int))]
    control timeout
    (ignore_env_bind (Lwt.return % map_err % status_of_yojson))
