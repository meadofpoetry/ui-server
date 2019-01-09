open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () =
  Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("history" @/ empty))

module HTTP = struct

  module Measurements = struct

    let raw_of_yojson data =
      let open Json in
      List.of_yojson
        (Pair.of_yojson
           id_of_yojson
           (List.of_yojson @@ Time.timestamped_of_yojson Measure.of_yojson))
        data

    let get ?(ids = []) ?(tuners = []) ?limit
          ?from ?till ?duration control =
      get_result ~from:(Api_js.Api_types.rows_of_yojson
                          raw_of_yojson
                          (fun _ -> Error "got compressed"))
        ~path:Uri.Path.Format.(get_base_path () / ("measurements" @/ empty))
        ~query:Uri.Query.[ "id", (module List(Stream.ID))
                         ; "tuner", (module List(Int))
                         ; "limit", (module Option(Int))
                         ; "from", (module Option(Time.Show))
                         ; "to", (module Option(Time.Show))
                         ; "duration", (module Option(Time.Relative)) ]
        control ids tuners limit from till duration

  end

end
