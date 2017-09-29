open Lwt.Infix
open Api.Interaction
open Board_protocol
open Board_types
open Containers
open Websocket_cohttp_lwt
open Frame

module Api_handler = Api.Handler.Make(Common.User)

let devinfo api () =
  api.devinfo () >>= fun devi ->
  respond_js (info_to_yojson devi) ()

let reset api () =
  api.reset () >>= respond_ok

let set_mode (api : api) body () =
  yojson_of_body body >>= fun mode ->
  match mode_of_yojson mode with
  | Error e -> respond_error e ()
  | Ok mode -> api.set_mode mode >>= respond_ok

let set_jitter_mode api body () =
  yojson_of_body body >>= fun mode ->
  match jitter_mode_of_yojson mode with
  | Error e -> respond_error e ()
  | Ok mode -> api.set_jitter_mode mode >>= respond_ok

let get_t2mi_seq api seconds () =
  match CCInt.of_string seconds with
  | None   -> respond_error "seconds parameter must be an integer" ()
  | Some x -> api.get_t2mi_seq x >>= fun rsp ->
              respond_js (`List (List.map t2mi_packet_to_yojson rsp)) ()

let handle api _ _ meth args _ _ body =
  let open Api.Redirect in
  match meth, args with
  | `GET,  ["devinfo"]      -> devinfo api ()
  | `POST, ["reset"]        -> reset api ()
  | `POST, ["mode"]         -> set_mode api body ()
  | `GET, "t2mi_seq"::[sec] -> get_t2mi_seq api sec ()
  | _ -> not_found ()

let handlers id api _ =
  [ (module struct
       let domain = Common.Hardware.get_api_path id
       let handle = handle api ()
     end : Api_handler.HANDLER) ]
