open Containers
open Api_js.Requests.Json_request
open Lwt.Infix

open Qoe_errors

open Queries

let get_structure () =
  get_result ~path:"api/pipeline/structure" Structure.Streams.of_yojson ()

let post_structure s =
  post_result ~path:"api/pipeline/structure" ~contents:(Structure.Streams.to_yojson s) (fun _ -> Ok ()) ()

let get_structure_socket () =
  WS.get ~path:"api/pipeline/structure" Structure.Streams.of_yojson ()

let get_settings () =
  get_result ~path:"api/pipeline/settings" Settings.of_yojson ()

let post_settings s =
  post_result ~path:"api/pipeline/settings" ~contents:(Settings.to_yojson s) (fun _ -> Ok ()) ()

let get_settings_socket () =
  WS.get ~path:"api/pipeline/settings" Settings.of_yojson ()

let get_wm () =
  get_result ~path:"api/pipeline/wm" Wm.of_yojson ()

let post_wm wm =
  post_result ~path:"api/pipeline/wm" ~contents:(Wm.to_yojson wm) (fun _ -> Ok ()) ()

let get_wm_socket () =
  WS.get ~path:"api/pipeline/wm" Wm.of_yojson ()

(* TODO single vdata call *)
  
let get_vdata_socket () =
  WS.get ~path:"api/pipeline/vdata" Video_data.of_yojson ()

let get_vdata_socket_stream stream =
  let query = Data_spec.to_query ~stream () in
  WS.get ~path:"api/pipeline/vdata" ~query Video_data.of_yojson ()

let get_vdata_socket_channel stream channel =
  let query = Data_spec.to_query ~stream ~channel () in
  WS.get ~path:"api/pipeline/vdata" ~query Video_data.of_yojson ()

let get_vdata_socket_pid stream channel pid =
  let query = Data_spec.to_query ~stream ~channel ~pid () in
  WS.get ~path:"api/pipeline/vdata" ~query Video_data.of_yojson ()
