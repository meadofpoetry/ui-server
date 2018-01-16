open Lwt.Infix
open Api.Interaction
open Board_protocol
open Board_types
open Containers
open Websocket_cohttp_lwt
open Frame

module Api_handler = Api.Handler.Make(Common.User)

let devinfo api () =
  api.devinfo >>= fun devi ->
  respond_js (devinfo_to_yojson devi) ()

let set_factory_mode (api : api) body () =
  yojson_of_body body >>= fun mode ->
  match factory_settings_of_yojson mode with
  | Error e -> respond_error e ()
  | Ok mode -> api.set_factory_mode mode
               >>= function
               | Error e -> respond_error e ()
               | Ok ()   -> respond_ok ()

let set_mode (api : api) body () =
  yojson_of_body body >>= fun mode ->
  match Common.Stream.t_list_of_yojson mode with
  | Error e -> respond_error e ()
  | Ok mode -> api.set_mode mode
               >>= function
               | Error e -> respond_error e ()
               | Ok ()   -> respond_ok ()

let handle api _ _ meth args _ _ body =
  let open Api.Redirect in
  match meth, args with
  | `POST, ["factory_mode"] -> set_factory_mode api body ()
  | `POST, ["mode"]         -> set_mode api body ()
  | `GET,  ["devinfo"]      -> devinfo api ()
  | `GET,  ["streams"]      -> respond_ok ()
  | _ -> not_found ()

let handlers id api _ =
  [ (module struct
       let domain = Common.Topology.get_api_path id
       let handle = handle api ()
     end : Api_handler.HANDLER) ]
