open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () =
  Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("receiver" @/ empty))

let set_id = Uri.Query.(make_query ["id", (module Option(Int))])

module WS = struct

  open Common.Uri

  let get_mode ?(ids=[]) control =
    WS.get ~from:mode_of_yojson
           ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
           ~query:Query.[ "id", (module List(Int)) ]
           control ids

  let get_lock ?(ids=[]) control =
    WS.get ~from:lock_of_yojson
           ~path:Path.Format.(get_base_path () / ("lock" @/ empty))
           ~query:Query.[ "id", (module List(Int)) ]
           control ids

  let get_measures ?(ids=[]) control =
    WS.get ~from:measures_of_yojson
           ~path:Path.Format.(get_base_path () / ("measures" @/ empty))
           ~query:Query.[ "id", (module List(Int)) ]
           control ids

  let get_parameters ?(ids=[]) control =
    WS.get ~from:params_of_yojson
           ~path:Path.Format.(get_base_path () / ("parameters" @/ empty))
           ~query:Query.[ "id", (module List(Int)) ]
           control ids

  let get_plp_list ?(ids=[]) control =
    WS.get ~from:plp_list_of_yojson
           ~path:Path.Format.(get_base_path () / ("plp-list" @/ empty))
           ~query:Query.[ "id", (module List(Int)) ]
           control ids

end

module HTTP = struct

  open Common.Uri

  let post_mode mode control =
    let contents = mode_to_yojson mode in
    post_result ~contents
                ~from:mode_rsp_of_yojson
                ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
                ~query:Query.empty control

  let get_mode_one ~id control =
    get_result ~from:mode_of_yojson
               ~path:Path.Format.(get_base_path () / (Int ^/ "mode" @/ empty))
               ~query:Query.empty id control
  let get_lock_one ~id control =
    get_result ~from:lock_of_yojson
               ~path:Path.Format.(get_base_path () / (Int ^/ "lock" @/ empty))
               ~query:Query.empty id control
  let get_measures_one ~id control =
    get_result ~from:measures_of_yojson
               ~path:Path.Format.(get_base_path () / (Int ^/ "measures" @/ empty))
               ~query:Query.empty id control
  let get_parameters_one ~id control =
    get_result ~from:params_of_yojson
               ~path:Path.Format.(get_base_path () / (Int ^/ "parameters" @/ empty))
               ~query:Query.empty id control
  let get_plp_list_one ~id control =
    get_result ~from:plp_list_of_yojson
               ~path:Path.Format.(get_base_path () / (Int ^/ "plp-list" @/ empty))
               ~query:Query.empty id control

  let get_mode_some ?(ids=[]) control =
    get_result ~from:(list_of_yojson mode_of_yojson)
               ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
               ~query:Query.[ "id", (module List(Int)) ]
               control ids
  let get_lock_some ?(ids=[]) control =
    get_result ~from:(list_of_yojson lock_of_yojson)
               ~path:Path.Format.(get_base_path () / ("lock" @/ empty))
               ~query:Query.[ "id", (module List(Int)) ]
               control ids
  let get_measures_some ?(ids=[]) control =
    get_result ~from:(list_of_yojson measures_of_yojson)
               ~path:Path.Format.(get_base_path () / ("measures" @/ empty))
               ~query:Query.[ "id", (module List(Int)) ]
               control ids
  let get_parameters_some ?(ids=[]) control =
    get_result ~from:(list_of_yojson params_of_yojson)
               ~path:Path.Format.(get_base_path () / ("parameters" @/ empty))
               ~query:Query.[ "id", (module List(Int)) ]
               control ids
  let get_plp_list_some ?(ids=[]) control =
    get_result ~from:(list_of_yojson plp_list_of_yojson)
               ~path:Path.Format.(get_base_path () / ("plp-list" @/ empty))
               ~query:Query.[ "id", (module List(Int)) ]
               control ids

  module Archive = struct

    let get_mode ?(ids=[]) ?limit ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
                 ~path:Path.Format.(get_base_path () / ("mode/archive" @/ empty))
                 ~query:Uri.Query.[ "id",       (module List(Int))
                                  ; "limit",    (module Option(Int))
                                  ; "from",     (module Option(Time.Show))
                                  ; "to",       (module Option(Time.Show))
                                  ; "duration", (module Option(Time.Relative)) ]
                 control ids limit from till duration

    let get_measures ?(ids=[]) ?limit ?compress ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
                 ~path:Path.Format.(get_base_path () / ("measures/archive" @/ empty))
                 ~query:Uri.Query.[ "id",       (module List(Int))
                                  ; "limit",    (module Option(Int))
                                  ; "compress", (module Option(Bool))
                                  ; "from",     (module Option(Time.Show))
                                  ; "to",       (module Option(Time.Show))
                                  ; "duration", (module Option(Time.Relative)) ]
                 control ids limit compress from till duration

    let get_parameters ?(ids=[]) ?limit ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
                 ~path:Path.Format.(get_base_path () / ("parameters/archive" @/ empty))
                 ~query:Uri.Query.[ "id",       (module List(Int))
                                  ; "limit",    (module Option(Int))
                                  ; "from",     (module Option(Time.Show))
                                  ; "to",       (module Option(Time.Show))
                                  ; "duration", (module Option(Time.Relative)) ]
                 control ids limit from till duration

  end

end
