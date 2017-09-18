open Lwt.Infix
open Interaction
open Board_dvb_protocol
open Common.Board.Dvb

let devinfo api () =
  api.devinfo () >>= fun devi ->
  respond_js (devinfo_response_to_yojson devi) ()
   
let reset api () =
  api.reset () >>= respond_ok

let settings (api : api) body () =
  yojson_of_body body >>= fun set ->
  match settings_request_of_yojson set with
  | Error e -> respond_error e ()
  | Ok set  ->
     api.settings set >>= fun set_rsp ->
     respond_js (settings_response_to_yojson set_rsp) ()

let plp_setting api body () =
  yojson_of_body body >>= fun plp ->
  match plp_setting_request_of_yojson plp with
  | Error e    -> respond_error e ()
  | Ok plp_set ->
     api.plp_setting plp_set >>= fun plp_set_rsp ->
     respond_js (plp_setting_response_to_yojson plp_set_rsp) ()

let plps (api : api) num () =
  try
    let i = int_of_string num in
    api.plps i >>= fun plp_rsp ->
    respond_js (plp_list_response_to_yojson plp_rsp) ()
  with _ -> respond_error (Printf.sprintf "plps: bad argument %s" num) ()
     
let handle api _ _ meth args _ _ body =
  let open Lwt.Infix in
  let open Redirect in
  (* let redirect_if_guest = redirect_if (User.eq id `Guest) in *)
  match meth, args with
  | `GET,  ["devinfo"]      -> devinfo api ()
  | `POST, ["reset"]        -> reset api ()
  | `POST, ["settings"]     -> settings api body ()
  | `POST, ["plp_settings"] -> plp_setting api body ()
  | `GET,  "plps"::[num]    -> plps api num ()
  | _ -> not_found ()

let handlers id api _ _ =
  [ (module struct
       let domain = Common.Hardware.get_api_path id
       let handle = handle api ()
     end : Api_handler.HANDLER) ]
