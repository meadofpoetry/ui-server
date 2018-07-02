open Containers
open Board_types
open Api_js.Requests.Json_request
open Lwt.Infix

include Boards_js.Requests

(* let to_unit = fun _ -> Ok ()
 * 
 * let post_factory_mode control settings =
 *   let path = Printf.sprintf "api/board/%d/factory_mode" control in
 *   post_result ~path ~contents:(factory_settings_to_yojson settings) to_unit ()
 * 
 * let post_nw_mode control settings =
 *   let path = Printf.sprintf "api/board/%d/nw_mode" control in
 *   post_result ~path ~contents:(nw_settings_to_yojson settings) to_unit ()
 * 
 * let post_streams_simple control streams =
 *   let path = Printf.sprintf "api/board/%d/streams_simple" control in
 *   post_result ~path ~contents:(Common.Stream.t_list_to_yojson streams) to_unit ()
 * 
 * let post_streams_full control streams =
 *   let path = Printf.sprintf "api/board/%d/streams_full" control in
 *   post_result ~path ~contents:(streams_full_request_to_yojson streams) to_unit ()
 * 
 * let get_devinfo control =
 *   let path = Printf.sprintf "api/board/%d/devinfo" control in
 *   get_result ~path devinfo_of_yojson ()
 * 
 * let get_config control =
 *   let path = Printf.sprintf "api/board/%d/config" control in
 *   get_result ~path config_response_of_yojson ()
 * 
 * let get_streams control =
 *   let path = Printf.sprintf "api/board/%d/streams" control in
 *   get_result ~path Common.Stream.t_list_of_yojson ()
 * 
 * let get_status_ws control =
 *   let path = Printf.sprintf "api/board/%d/status_ws" control in
 *   WS.get ~path status_of_yojson ()
 * 
 * let get_config_ws control =
 *   let path = Printf.sprintf "api/board/%d/config_ws" control in
 *   WS.get ~path config_response_of_yojson ()
 * 
 * let get_streams_ws control =
 *   let path = Printf.sprintf "api/board/%d/streams_ws" control in
 *   WS.get ~path Common.Stream.t_list_of_yojson () *)
