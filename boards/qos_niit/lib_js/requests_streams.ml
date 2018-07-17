open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path' () = Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("streams" @/ empty))

module TS = struct
  let get_base_path () = Uri.Path.Format.(get_base_path' () / ("ts" @/ empty))
end
module T2MI = struct
  let get_base_path () = Uri.Path.Format.(get_base_path' () / ("t2mi" @/ empty))
end

module WS = struct

  open Common.Uri

  module TS = struct

    open Streams.TS
    include TS

    let of_yojson f v = Json.(List.of_yojson (Pair.of_yojson Stream.id_of_yojson f) v)

    let get_streams ?(ids=[]) control =
      WS.get ~from:(Json.List.of_yojson Stream.of_yojson)
        ~path:Path.Format.(get_base_path ())
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    let get_state ?(ids=[]) control =
      WS.get ~from:(of_yojson state_of_yojson)
        ~path:Path.Format.(get_base_path () / ("state" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    let get_bitrate ?(ids=[]) control =
      WS.get ~from:(of_yojson bitrate_of_yojson)
        ~path:Path.Format.(get_base_path () / ("bitrate" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    let get_structure ?(ids=[]) control =
      WS.get ~from:(of_yojson structure_of_yojson)
        ~path:Path.Format.(get_base_path () / ("structure" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

  end

  module T2MI = struct

    open Streams.T2MI
    include T2MI

    let of_yojson f v = Json.(List.of_yojson (Pair.of_yojson Int.of_yojson f) v)

    let get_state ?(ids=[]) control =
      WS.get ~from:(of_yojson state_of_yojson)
        ~path:Path.Format.(get_base_path () / ("state" @/ empty))
        ~query:Query.[ "id", (module List(Int)) ]
        control ids

    let get_structure ?(ids=[]) control =
      WS.get ~from:(of_yojson structure_of_yojson)
        ~path:Path.Format.(get_base_path () / ("structure" @/ empty))
        ~query:Query.[ "id", (module List(Int)) ]
        control ids

  end

end

module HTTP = struct

  open Common.Uri

  module TS = struct

    open Streams.TS
    include TS

    let of_yojson f v = Json.(List.of_yojson (Pair.of_yojson Stream.id_of_yojson f) v)

    let get_streams ?(ids=[]) control =
      get_result ~from:(Json.List.of_yojson Stream.of_yojson)
        ~path:(get_base_path ())
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    let get_state ?(ids=[]) control =
      get_result ~from:(of_yojson state_of_yojson)
        ~path:Path.Format.(get_base_path () / ("state" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    let get_bitrate ?(ids=[]) control =
      get_result ~from:(of_yojson bitrate_of_yojson)
        ~path:Path.Format.(get_base_path () / ("bitrate" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    let get_structure ?(ids=[]) control =
      get_result ~from:(of_yojson structure_of_yojson)
        ~path:Path.Format.(get_base_path () / ("structure" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    module Archive = struct

      let get_streams ?(ids=[]) ?limit ?from ?till ?duration control =
        get_result ~from:(Api_js.Api_types.rows_of_yojson
                            archived_list_of_yojson
                            (fun _ -> Error "cannot be compressed"))
          ~path:Path.Format.(get_base_path () / ("archive" @/ empty))
          ~query:Query.[ "id",       (module List(Int32))
                       ; "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control (List.map Stream.id_to_int32 ids) limit from till duration

      let get_state ?(ids=[]) ?limit ?compress ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("state/archive" @/ empty))
          ~query:Query.[ "id",       (module List(Int32))
                       ; "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control (List.map Stream.id_to_int32 ids) limit compress from till duration

      let get_bitrate ?(ids=[]) ?limit ?compress ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("bitrate/archive" @/ empty))
          ~query:Query.[ "id",       (module List(Int32))
                       ; "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control (List.map Stream.id_to_int32 ids) limit compress from till duration

      let get_structure ?(ids=[]) ?limit ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("structure/archive" @/ empty))
          ~query:Query.[ "id",       (module List(Int32))
                       ; "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control (List.map Stream.id_to_int32 ids) limit from till duration

    end

  end

  module T2MI = struct

    open Streams.T2MI
    include T2MI

    let of_yojson f v = Json.(List.of_yojson (Pair.of_yojson Int.of_yojson f) v)

    let get_state ?(ids=[]) control =
      get_result ~from:(of_yojson state_of_yojson)
        ~path:Path.Format.(get_base_path () / ("state" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    let get_structure ?(ids=[]) control =
      get_result ~from:(of_yojson structure_of_yojson)
        ~path:Path.Format.(get_base_path () / ("structure" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    let get_sequence ?(ids=[]) control =
      get_result ~from:sequence_of_yojson
        ~path:Path.Format.(get_base_path () / ("sequence" @/ empty))
        ~query:Query.[ "id", (module List(Int32)) ]
        control (List.map Stream.id_to_int32 ids)

    module Archive = struct

      let get_state ?(ids=[]) ?limit ?compress ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("state/archive" @/ empty))
          ~query:Query.[ "id",       (module List(Int))
                       ; "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control ids limit compress from till duration

      let get_structure ?(ids=[]) ?limit ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("structure/archive" @/ empty))
          ~query:Query.[ "id",       (module List(Int))
                       ; "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control ids limit from till duration

    end

  end

end
