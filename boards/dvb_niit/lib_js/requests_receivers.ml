open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () =
  Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("receiver" @/ empty))

module WS = struct

  open Common.Uri

  let of_yojson f v = Json.(Pair.of_yojson Int.of_yojson f v)

  let get_mode ?(ids=[]) control =
    WS.get ~from:(of_yojson mode_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids

  let get_lock ?(ids=[]) control =
    WS.get ~from:(of_yojson lock_of_yojson)
      ~path:Path.Format.(get_base_path () / ("lock" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids

  let get_measures ?(ids=[]) control =
    WS.get ~from:(of_yojson measures_of_yojson)
      ~path:Path.Format.(get_base_path () / ("measures" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids

  let get_parameters ?(ids=[]) control =
    WS.get ~from:(of_yojson params_of_yojson)
      ~path:Path.Format.(get_base_path () / ("parameters" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids

  let get_plp_list ?(ids=[]) control =
    WS.get ~from:(of_yojson plp_list_of_yojson)
      ~path:Path.Format.(get_base_path () / ("plp-list" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids

end

module HTTP = struct

  open Common.Uri

  let of_yojson f = Json.(List.of_yojson (Pair.of_yojson Int.of_yojson f))

  let get_mode ?(ids=[]) control =
    get_result ~from:(of_yojson mode_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids
  let get_lock ?(ids=[]) control =
    get_result ~from:(of_yojson lock_of_yojson)
      ~path:Path.Format.(get_base_path () / ("lock" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids
  let get_measures ?(ids=[]) control =
    get_result ~from:(of_yojson measures_of_yojson)
      ~path:Path.Format.(get_base_path () / ("measures" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids
  let get_parameters ?(ids=[]) control =
    get_result ~from:(of_yojson params_of_yojson)
      ~path:Path.Format.(get_base_path () / ("parameters" @/ empty))
      ~query:Query.[ "id", (module List(Int)) ]
      control ids
  let get_plp_list ?(ids=[]) control =
    get_result ~from:(of_yojson plp_list_of_yojson)
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
