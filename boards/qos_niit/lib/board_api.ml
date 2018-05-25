open Containers
open Api.Redirect

module Api_handler = Api.Handler.Make(Common.User)

(* let jitter_ws sock_data (events:events) body =
 *   sock_handler sock_data events.jitter Jitter.measures_to_yojson body *)

let handle api events _ meth path uri sock_data _ body =
  let scheme = Api.Api_types.meth_of_uri uri in
  match Api_utils.req_of_path path with
  | Some (`Device x)  -> Board_api_device.handle  api events scheme meth x uri sock_data body ()
  | Some (`Errors x)  -> Board_api_errors.handle  api events scheme meth x uri sock_data body ()
  | Some (`Streams x) -> Board_api_streams.handle api events scheme meth x uri sock_data body ()
  | _ -> not_found ()

let handlers id api events =
  [ (module struct
       let domain = Common.Topology.get_api_path id
       let handle = handle api events
     end : Api_handler.HANDLER) ]
