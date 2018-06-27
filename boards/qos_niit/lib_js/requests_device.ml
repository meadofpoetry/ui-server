open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

(* let make_path = Boards_js.Requests.Device.make_path
 * 
 * module WS = struct
 * 
 *   include Boards_js.Requests.Device.WS
 * 
 *   let get_status control =
 *     let path = make_path control ["status"] in
 *     WS.get ~path status_of_yojson ()
 * 
 *   let get_errors control =
 *     let path = make_path control ["errors"] in
 *     WS.get ~path board_errors_of_yojson ()
 * 
 *   let get_t2mi_mode control =
 *     let path = make_path control ["mode";"t2mi"] in
 *     WS.get ~path t2mi_mode_opt_of_yojson ()
 * 
 *   let get_jitter_mode control =
 *     let path = make_path control ["mode";"jitter"] in
 *     WS.get ~path jitter_mode_opt_of_yojson ()
 * 
 * end
 * 
 * module HTTP = struct
 * 
 *   include (Boards_js.Requests.Device.HTTP:
 *            module type of Boards_js.Requests.Device.HTTP
 *                           with module Archive := Boards_js.Requests.Device.HTTP.Archive)
 * 
 *   (\** Resets the board **\)
 *   let post_reset control =
 *     let path = make_path control ["reset"] in
 *     post_result_unit ~path ()
 * 
 *   (\** Sets T2-MI analysis settings **\)
 *   let post_t2mi_mode control mode =
 *     let path = make_path control ["mode";"t2mi"] in
 *     let contents = t2mi_mode_opt_to_yojson mode in
 *     post_result_unit ~path ~contents ()
 * 
 *   (\** Sets jitter measurements settings **\)
 *   let post_jitter_mode control mode =
 *     let path = make_path control ["mode";"jitter"] in
 *     let contents = jitter_mode_opt_to_yojson mode in
 *     post_result_unit ~path ~contents ()
 * 
 *   let get_devinfo control =
 *     let path = make_path control ["info"] in
 *     get_result ~path devinfo_opt_of_yojson ()
 * 
 *   let get_t2mi_mode control =
 *     let path = make_path control ["mode";"t2mi"] in
 *     get_result ~path t2mi_mode_opt_of_yojson ()
 * 
 *   let get_jitter_mode control =
 *     let path = make_path control ["mode";"jitter"] in
 *     get_result ~path jitter_mode_opt_of_yojson ()
 * 
 *   module Archive = struct
 * 
 *     include Boards_js.Requests.Device.HTTP.Archive
 * 
 *     let get_status ?limit ?total time control =
 *       let query =
 *         let open Uri.Query in
 *         let coll = Api_js.Query.Collection.make ?limit ?total () in
 *         let time = Api_js.Query.Time.make time in
 *         merge coll time
 *       in
 *       let path = make_path control ["status"] in
 *       get_result ~query ~path (fun _ -> Error "not implemented") ()
 * 
 *     (\* TODO add filter *\)
 *     let get_errors ?limit ?thin ?total time control =
 *       let query =
 *         let open Uri.Query in
 *         let coll = Api_js.Query.Collection.make ?limit ?total ?thin () in
 *         let time = Api_js.Query.Time.make time in
 *         merge coll time
 *       in
 *       let path = make_path control ["errors"] in
 *       get_result ~query ~path (fun _ -> Error "not implemented") ()
 * 
 *   end
 * 
 * end *)
