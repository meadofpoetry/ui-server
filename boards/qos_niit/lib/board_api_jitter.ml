open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect
open Common.Uri.Query
   
(**
 ** API
 **
 ** TODO TO BE DETERMINED
 **
 **)

include Api_utils.Jitter

let handle_ok api events scheme meth req (q:Raw.t list) sock_data body time () =
  match scheme,meth,req,time with
  | _ -> not_found ()

let handle api events scheme meth req uri sock_data body () =
  match get_time_query @@ Uri.query uri with
  | Error e,_ -> Json.respond_result (Error (Api_utils.err_to_yojson @@ Bad_query e))
  | Ok t,q    -> handle_ok api events scheme meth req q sock_data body t ()
