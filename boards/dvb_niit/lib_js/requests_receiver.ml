open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () =
  Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("receiver" @/ empty))

let set_id = Uri.Query.(make_query ["id", (module Option(Int))])

module WS = struct

  open Common.Uri

  let get_mode ?id control =
    WS.get ~from:mode_of_yojson
           ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
           ~query:Query.["id", (module Option(Int))] control id

  let get_lock ?id control =
    WS.get ~from:lock_of_yojson
           ~path:Path.Format.(get_base_path () / ("lock" @/ empty))
           ~query:Query.["id", (module Option(Int))] control id

  let get_measures ?id control =
    WS.get ~from:measures_of_yojson
           ~path:Path.Format.(get_base_path () / ("measures" @/ empty))
           ~query:Query.["id", (module Option(Int))] control id

  let get_parameters ?id control =
    WS.get ~from:params_of_yojson
           ~path:Path.Format.(get_base_path () / ("parameters" @/ empty))
           ~query:Query.["id", (module Option(Int))] control id

  let get_plp_list ?id control =
    WS.get ~from:plp_list_of_yojson
           ~path:Path.Format.(get_base_path () / ("plp-list" @/ empty))
           ~query:Query.["id", (module Option(Int))] control id

end

module HTTP = struct

  open Common.Uri

  let post_mode mode control =
    let contents = mode_to_yojson mode in
    post_result ~contents
                ~from:mode_rsp_of_yojson
                ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
                ~query:Query.empty control

  let get_mode ~id control =
    get_result ~from:mode_of_yojson
               ~path:Path.Format.(get_base_path () / ("mode" @/ Int ^/ empty))
               ~query:Query.empty id control

  let get_mode_for_all control =
    get_result ~from:modes_of_yojson
               ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
               ~query:Query.empty control

  let get_lock ~id control =
    get_result ~from:lock_of_yojson
               ~path:Path.Format.(get_base_path () / ("lock" @/ Int ^/ empty))
               ~query:Query.empty id control

  let get_lock_for_all control =
    get_result ~from:lock_all_of_yojson
               ~path:Path.Format.(get_base_path () / ("lock" @/ empty))
               ~query:Query.empty control

  let get_measures ~id control =
    get_result ~from:measures_of_yojson
               ~path:Path.Format.(get_base_path () / ("measures" @/ Int ^/ empty))
               ~query:Query.empty id control

  let get_measures_for_all control =
    get_result ~from:measures_all_of_yojson
               ~path:Path.Format.(get_base_path () / ("measures" @/ empty))
               ~query:Query.empty control

  let get_parameters ~id control =
    get_result ~from:params_of_yojson
               ~path:Path.Format.(get_base_path () / ("parameters" @/ Int ^/ empty))
               ~query:Query.empty id control

  let get_measures_for_all control =
    get_result ~from:params_all_of_yojson
               ~path:Path.Format.(get_base_path () / ("parameters" @/ empty))
               ~query:Query.empty control

  let get_plp_list ~id control =
    get_result ~from:plp_list_of_yojson
               ~path:Path.Format.(get_base_path () / ("plp-list" @/ Int ^/ empty))
               ~query:Query.empty id control

  let get_plp_list_for_all control =
    get_result ~from:plp_list_all_of_yojson
               ~path:Path.Format.(get_base_path () / ("plp-list" @/ empty))
               ~query:Query.empty control

  module Archive = struct

    let get_mode ?limit ?total ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
                 ~path:Path.Format.(get_base_path () / ("mode/archive" @/ empty))
                 ~query:Uri.Query.[ "limit",    (module Option(Int))
                                  ; "total",    (module Option(Bool))
                                  ; "from",     (module Option(Time.Show))
                                  ; "to",       (module Option(Time.Show))
                                  ; "duration", (module Option(Time.Relative))]
                 control limit total from till duration

    let get_lock ?limit ?total ?compress ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
                 ~path:Path.Format.(get_base_path () / ("lock/archive" @/ empty))
                 ~query:Uri.Query.[ "limit",    (module Option(Int))
                                  ; "total",    (module Option(Bool))
                                  ; "compress", (module Option(Bool))
                                  ; "from",     (module Option(Time.Show))
                                  ; "to",       (module Option(Time.Show))
                                  ; "duration", (module Option(Time.Relative))]
                 control limit total compress from till duration

    let get_measures ?limit ?total ?compress ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
                 ~path:Path.Format.(get_base_path () / ("measures/archive" @/ empty))
                 ~query:Uri.Query.[ "limit",    (module Option(Int))
                                  ; "total",    (module Option(Bool))
                                  ; "compress", (module Option(Bool))
                                  ; "from",     (module Option(Time.Show))
                                  ; "to",       (module Option(Time.Show))
                                  ; "duration", (module Option(Time.Relative))]
                 control limit total compress from till duration

    let get_parameters ?limit ?total ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
                 ~path:Path.Format.(get_base_path () / ("parameters/archive" @/ empty))
                 ~query:Uri.Query.[ "limit",    (module Option(Int))
                                  ; "total",    (module Option(Bool))
                                  ; "from",     (module Option(Time.Show))
                                  ; "to",       (module Option(Time.Show))
                                  ; "duration", (module Option(Time.Relative))]
                 control limit total from till duration

  end

end
