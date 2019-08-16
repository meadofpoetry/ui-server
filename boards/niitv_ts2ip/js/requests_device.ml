(* open Board_types
 * open Api_js.Requests.Json_request
 * open Common
 * 
 * let get_base_path = Boards_js.Requests.Device.get_device_path
 * 
 * module WS = struct
 * 
 *   open Common.Uri
 * 
 *   include Boards_js.Requests.Device.WS
 * 
 *   let get_mode (control : int) =
 *     WS.get ~from:nw_settings_of_yojson
 *       ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
 *       ~query:Query.empty
 *       control
 * 
 * end
 * 
 * module HTTP = struct
 * 
 *   open Common.Uri
 * 
 *   include (Boards_js.Requests.Device.HTTP:
 *            module type of Boards_js.Requests.Device.HTTP
 *                           with module Archive := Boards_js.Requests.Device.HTTP.Archive)
 * 
 *   let set_mode (mode : nw_settings) control =
 *     post_result_unit ~contents:(nw_settings_to_yojson mode)
 *       ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
 *       ~query:Query.empty
 *       control
 * 
 *   let get_devinfo control =
 *     get_result ~from:(Json.Option.of_yojson devinfo_of_yojson)
 *       ~path:Path.Format.(get_base_path () / ("info" @/ empty))
 *       ~query:Query.empty
 *       control
 * 
 *   let get_mac control =
 *     get_result ~from:Macaddr.of_yojson
 *       ~path:Path.Format.(get_base_path () / ("mac" @/ empty))
 *       ~query:Query.empty
 *       control
 * 
 *   let get_mode control =
 *     get_result ~from:nw_settings_of_yojson
 *       ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
 *       ~query:Query.empty
 *       control
 * 
 *   module Archive = struct
 * 
 *     include Boards_js.Requests.Device.HTTP.Archive
 * 
 *   end
 * 
 * end *)
