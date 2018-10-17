open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () =
  Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("receiver" @/ empty))

module WS = struct

  open Common.Uri

  let get_mode ~id control =
    WS.get ~from:mode_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "mode" @/ empty))
      ~query:Query.empty
      control id

  let get_lock ~id control =
    WS.get ~from:lock_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "lock" @/ empty))
      ~query:Query.empty
      control id

  let get_measures ~id control =
    WS.get ~from:measures_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "measures" @/ empty))
      ~query:Query.empty
      control id

  let get_parameters ~id control =
    WS.get ~from:params_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "parameters" @/ empty))
      ~query:Query.empty
      control id

  let get_plp_list ~id control =
    WS.get ~from:plp_list_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "plp-list" @/ empty))
      ~query:Query.empty
      control id

end

module HTTP = struct

  open Common.Uri

  let post_mode ~id mode control =
    let contents = mode_to_yojson mode in
    post_result ~contents
      ~from:Json.(Pair.of_yojson Int.of_yojson mode_rsp_of_yojson)
      ~path:Path.Format.(get_base_path () / (Int ^/ "mode" @/ empty))
      ~query:Query.empty control id

  let get_mode ~id control =

    get_result ~from:mode_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "mode" @/ empty))
      ~query:Query.empty
      control id
  let get_lock ~id control =
    get_result ~from:lock_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "lock" @/ empty))
      ~query:Query.empty
      control id
  let get_measures ~id control =
    get_result ~from:measures_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "measures" @/ empty))
      ~query:Query.empty
      control id
  let get_parameters ~id control =
    get_result ~from:params_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "parameters" @/ empty))
      ~query:Query.empty
      control id
  let get_plp_list ~id control =
    get_result ~from:plp_list_of_yojson
      ~path:Path.Format.(get_base_path () / (Int ^/ "plp-list" @/ empty))
      ~query:Query.empty
      control id

  module Archive = struct

    let get_mode ?limit ?from ?till ?duration ~id control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / (Int ^/ "mode/archive" @/ empty))
        ~query:Uri.Query.[ "limit",    (module Option(Int))
                         ; "from",     (module Option(Time.Show))
                         ; "to",       (module Option(Time.Show))
                         ; "duration", (module Option(Time.Relative)) ]
        control id limit from till duration

    let get_measures ?limit ?compress ?from ?till ?duration ~id control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / (Int ^/ "measures/archive" @/ empty))
        ~query:Uri.Query.[ "limit",    (module Option(Int))
                         ; "compress", (module Option(Bool))
                         ; "from",     (module Option(Time.Show))
                         ; "to",       (module Option(Time.Show))
                         ; "duration", (module Option(Time.Relative)) ]
        control id limit compress from till duration

    let get_parameters ?limit ?from ?till ?duration ~id control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / (Int ^/ "parameters/archive" @/ empty))
        ~query:Uri.Query.[ "limit",    (module Option(Int))
                         ; "from",     (module Option(Time.Show))
                         ; "to",       (module Option(Time.Show))
                         ; "duration", (module Option(Time.Relative)) ]
        control id limit from till duration

  end

end
