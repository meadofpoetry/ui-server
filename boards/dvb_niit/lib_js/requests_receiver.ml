open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let make_path control path = Boards_js.Requests.make_path control ("receiver"::path)

let set_id = Uri.Query.(make_query ["id", (module Option(Int))])

module WS = struct

  let get_mode ?id control =
    let query = set_id id in
    let path  = make_path control ["mode"] in
    WS.get ~path ~query mode_of_yojson ()

  let get_lock ?id control =
    let query = set_id id in
    let path  = make_path control ["lock"] in
    WS.get ~path ~query lock_of_yojson ()

  let get_measures ?id control =
    let query = set_id id in
    let path  = make_path control ["measures"] in
    WS.get ~path ~query measures_of_yojson ()

  let get_parameters ?id control =
    let query = set_id id in
    let path  = make_path control ["parameters"] in
    WS.get ~path ~query params_of_yojson ()

  let get_plp_list ?id control =
    let query = set_id id in
    let path  = make_path control ["plp-list"] in
    WS.get ~path ~query plp_list_of_yojson ()

end

module HTTP = struct

  let post_mode mode control =
    let path     = make_path control ["mode"] in
    let contents = mode_to_yojson mode in
    post_result ~contents ~path mode_rsp_of_yojson ()

  let get_mode_for_id id control =
    let query = set_id (Some id) in
    let path  = make_path control ["mode"] in
    get_result ~query ~path mode_of_yojson ()

  let get_mode control =
    let path = make_path control ["mode"] in
    get_result ~path modes_of_yojson ()

  let get_lock_for_id id control =
    let query = set_id (Some id) in
    let path  = make_path control ["lock"] in
    get_result ~query ~path lock_of_yojson ()

  let get_lock_for_all control =
    let path = make_path control ["lock"] in
    get_result ~path lock_all_of_yojson ()

  let get_measures_for_id id control =
    let query = set_id (Some id) in
    let path  = make_path control ["measures"] in
    get_result ~query ~path measures_of_yojson ()

  let get_measures_for_all control =
    let path = make_path control ["measures"] in
    get_result ~path measures_all_of_yojson ()

  let get_parameters_for_id id control =
    let query = set_id (Some id) in
    let path  = make_path control ["parameters"] in
    get_result ~query ~path params_of_yojson ()

  let get_parameters_for_all control =
    let path = make_path control ["parameters"] in
    get_result ~path params_all_of_yojson ()

  let get_plp_list_for_id id control =
    let query = set_id (Some id) in
    let path  = make_path control ["plp-list"] in
    get_result ~query ~path plp_list_of_yojson ()

  let get_plp_list_for_all id control =
    let query = set_id (Some id) in
    let path  = make_path control ["plp-list"] in
    get_result ~query ~path plp_list_all_of_yojson ()

  module Archive = struct

    let get_mode_for_id ?limit ?total time id control =
      let query =
        let id   = set_id (Some id) in
        let coll = Api_js.Query.Collection.make ?limit ?total () in
        let time = Api_js.Query.Time.make time in
        List.fold_left Uri.Query.merge [] [id;coll;time]
      in
      let path  = make_path control ["mode"] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

    let get_mode ?limit ?total time control =
      let query =
        let coll = Api_js.Query.Collection.make ?limit ?total () in
        let time = Api_js.Query.Time.make time in
        List.fold_left Uri.Query.merge [] [coll;time]
      in
      let path  = make_path control ["mode"] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

    let get_lock_for_id ?limit ?total time id control =
      let query =
        let id   = set_id (Some id) in
        let coll = Api_js.Query.Collection.make ?limit ?total () in
        let time = Api_js.Query.Time.make time in
        List.fold_left Uri.Query.merge [] [id;coll;time]
      in
      let path  = make_path control ["lock"] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

    let get_lock_for_all ?limit ?total time control =
      let query =
        let coll = Api_js.Query.Collection.make ?limit ?total () in
        let time = Api_js.Query.Time.make time in
        List.fold_left Uri.Query.merge [] [coll;time]
      in
      let path  = make_path control ["lock"] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

    let get_measures_for_id ?limit ?total ?thin time id control =
      let query =
        let id   = set_id (Some id) in
        let coll = Api_js.Query.Collection.make ?limit ?total ?thin () in
        let time = Api_js.Query.Time.make time in
        List.fold_left Uri.Query.merge [] [id;coll;time]
      in
      let path  = make_path control ["measures"] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

    let get_measures_for_all ?limit ?total ?thin time control =
      let query =
        let coll = Api_js.Query.Collection.make ?limit ?total ?thin () in
        let time = Api_js.Query.Time.make time in
        List.fold_left Uri.Query.merge [] [coll;time]
      in
      let path  = make_path control ["measures"] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

    let get_parameters_for_id ?limit ?total time id control =
      let query =
        let id   = set_id (Some id) in
        let coll = Api_js.Query.Collection.make ?limit ?total () in
        let time = Api_js.Query.Time.make time in
        List.fold_left Uri.Query.merge [] [id;coll;time]
      in
      let path  = make_path control ["parameters"] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

    let get_parameters_for_all ?limit ?total time control =
      let query =
        let coll = Api_js.Query.Collection.make ?limit ?total () in
        let time = Api_js.Query.Time.make time in
        List.fold_left Uri.Query.merge [] [coll;time]
      in
      let path  = make_path control ["parameters"] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

  end

end
