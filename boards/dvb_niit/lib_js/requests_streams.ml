open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () =
  Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("streams" @/ empty))

module WS = struct

  open Common.Uri

  let of_yojson f v =
    Json.(Pair.of_yojson
            Stream.of_yojson
            (Time.timestamped_of_yojson f) v)

  let get_measures ?(ids = []) control =
    WS.get ~from:(of_yojson Measure.of_yojson)
      ~path:Path.Format.(get_base_path () / ("measures" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_parameters ?(ids = []) control =
    WS.get ~from:(of_yojson Params.of_yojson)
      ~path:Path.Format.(get_base_path () / ("parameters" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_plp_list ?(ids = []) control =
    WS.get ~from:(of_yojson Plp_list.of_yojson)
      ~path:Path.Format.(get_base_path () / ("plp-list" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

end

module HTTP = struct

  open Common.Uri

  let of_yojson f =
    Json.(List.of_yojson (Pair.of_yojson
                            Stream.of_yojson
                            (Time.timestamped_of_yojson f)))

  let get_measures ?(ids = []) control =
    get_result ~from:(of_yojson Measure.of_yojson)
      ~path:Path.Format.(get_base_path () / ("measures" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_parameters ?(ids = []) control =
    get_result ~from:(of_yojson Params.of_yojson)
      ~path:Path.Format.(get_base_path () / ("parameters" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_plp_list ?(ids = []) control =
    get_result ~from:(of_yojson Plp_list.of_yojson)
      ~path:Path.Format.(get_base_path () / ("plp-list" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

end
