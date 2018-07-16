open Containers
open Api_js.Requests.Json_request
open Lwt.Infix
open Common.Uri
   
open Qoe_errors

open Queries

let get_structure () =
  get_result ?scheme:None ?from_err:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/structure" @/ empty)
    ~query:Query.empty
    ~from:Structure.Streams.of_yojson

let post_structure s =
  post_result ?scheme:None ?from_err:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/structure" @/ empty)
    ~query:Query.empty
    ~contents:(Structure.Streams.to_yojson s)
    ~from:(fun _ -> Ok ())

let get_structure_socket () =
  WS.get ?secure:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/structure" @/ empty)
    ~query:Query.empty
    ~from:Structure.Streams.of_yojson

let get_settings () =
  get_result ?scheme:None ?from_err:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/settings" @/ empty)
    ~query:Query.empty
    ~from:Settings.of_yojson

let post_settings s =
  post_result ?scheme:None ?from_err:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/settings" @/ empty)
    ~query:Query.empty
    ~contents:(Settings.to_yojson s)
    ~from:(fun _ -> Ok ())

let get_settings_socket () =
  WS.get ?secure:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/settings" @/ empty)
    ~query:Query.empty
    ~from:Settings.of_yojson

let get_wm () =
  get_result ?scheme:None ?from_err:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/wm" @/ empty)
    ~query:Query.empty
    ~from:Wm.of_yojson 

let post_wm wm =
  post_result ?scheme:None ?from_err:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/wm" @/ empty)
    ~query:Query.empty
    ~contents:(Wm.to_yojson wm)
    ~from:(fun _ -> Ok ())

let get_wm_socket () =
  WS.get ?secure:None ?host:None ?port:None
    ~path:Path.Format.("api/pipeline/wm" @/ empty)
    ~query:Query.empty
    ~from:Wm.of_yojson

(* TODO single vdata call *)
  (*
let get_vdata_socket () =
  WS.get ~path:Path.Format.("api/pipeline/vdata" @/ empty) Video_data.of_yojson ()

let get_vdata_socket_stream stream =
  let query = Data_spec.to_query ~stream () in
  WS.get ~path:Path.Format.("api/pipeline/vdata" @/ empty) ~query Video_data.of_yojson ()

let get_vdata_socket_channel stream channel =
  let query = Data_spec.to_query ~stream ~channel () in
  WS.get ~path:Path.Format.("api/pipeline/vdata" @/ empty) ~query Video_data.of_yojson ()

let get_vdata_socket_pid stream channel pid =
  let query = Data_spec.to_query ~stream ~channel ~pid () in
  WS.get ~path:Path.Format.("api/pipeline/vdata" @/ empty) ~query Video_data.of_yojson ()
   *)
