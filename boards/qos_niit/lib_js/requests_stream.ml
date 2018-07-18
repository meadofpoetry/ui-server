open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path' () = Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("stream" @/ empty))
module TS = struct
  let get_base_path ()   = Uri.Path.Format.(get_base_path' () / ("ts" @/ Int32 ^/ empty))
end
module T2MI = struct
  let get_base_path () = Uri.Path.Format.(get_base_path' () / ("t2mi" @/ Int ^/ empty))
end

module WS = struct

  open Common.Uri

  module TS = struct

    open Streams.TS
    include TS

    let get_stream ~id control =
      WS.get ~from:Stream.of_yojson
        ~path:(get_base_path ())
        ~query:Query.empty
        control (Stream.id_to_int32 id)

    let get_state ~id control =
      WS.get ~from:state_of_yojson
        ~path:Path.Format.(get_base_path () / ("state" @/ empty))
        ~query:Query.empty
        control (Stream.id_to_int32 id)

    let get_bitrate ~id control =
      WS.get ~from:bitrate_of_yojson
        ~path:Path.Format.(get_base_path () / ("bitrate" @/ empty))
        ~query:Query.empty
        control (Stream.id_to_int32 id)

    let get_structure ~id control =
      WS.get ~from:structure_of_yojson
        ~path:Path.Format.(get_base_path () / ("structure" @/ empty))
        ~query:Query.empty
        control (Stream.id_to_int32 id)

  end

  module T2MI = struct

    open Streams.T2MI
    include T2MI

    let get_state ~id control =
      WS.get ~from:state_of_yojson
        ~path:Path.Format.(get_base_path () / ("state" @/ empty))
        ~query:Query.empty
        control id

    let get_structure ~id control =
      WS.get ~from:structure_of_yojson
        ~path:Path.Format.(get_base_path () / ("structure" @/ empty))
        ~query:Query.empty
        control id

  end

end

module HTTP = struct

  open Common.Uri

  module TS = struct

    open Streams.TS
    include TS

    let get_stream ~id control =
      get_result ~from:Stream.of_yojson
        ~path:(get_base_path ())
        ~query:Query.empty
        control (Stream.id_to_int32 id)

    let get_state ~id control =
      get_result ~from:state_of_yojson
        ~path:Path.Format.(get_base_path () / ("state" @/ empty))
        ~query:Query.empty
        control (Stream.id_to_int32 id)

    let get_bitrate ~id control =
      get_result ~from:bitrate_of_yojson
        ~path:Path.Format.(get_base_path () / ("bitrate" @/ empty))
        ~query:Query.empty
        control (Stream.id_to_int32 id)

    let get_structure ~id control =
      get_result ~from:structure_of_yojson
        ~path:Path.Format.(get_base_path () / ("structure" @/ empty))
        ~query:Query.empty
        control (Stream.id_to_int32 id)

    let get_si_psi_section ?section ?table_id_ext ?eit_ts_id ?eit_orig_nw_id ~id ~table_id control =
      get_result ~from:section_of_yojson
        ~from_err:section_error_of_yojson
        ~path:Path.Format.(get_base_path () / ("section" @/ Int ^/ empty))
        ~query:Query.[ "section",        (module Option(Int))
                     ; "table-id-ext",   (module Option(Int))
                     ; "eit-ts-id",      (module Option(Int))
                     ; "eit-orig-nw-id", (module Option(Int)) ]
        control (Stream.id_to_int32 id) table_id section table_id_ext eit_ts_id eit_orig_nw_id

    module Archive = struct

      let get_stream ?limit ?from ?till ?duration ~id control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("archive" @/ empty))
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control (Stream.id_to_int32 id) limit from till duration

      let get_state ?limit ?compress ?from ?till ?duration ~id control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("state/archive" @/ empty))
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control (Stream.id_to_int32 id) limit compress from till duration

      let get_bitrate ?limit ?compress ?from ?till ?duration ~id control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("bitrate/archive" @/ empty))
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control (Stream.id_to_int32 id) limit compress from till duration

      let get_structure ?limit ?from ?till ?duration ~id control =
        get_result
          ~from:(Api_js.Api_types.rows_of_yojson
                   Json.(List.of_yojson (Pair.of_yojson
                                           Stream.id_of_yojson
                                           structure_of_yojson))
                   (fun _ -> Error "cannot be compressed"))
          ~path:Path.Format.(get_base_path () / ("structure/archive" @/ empty))
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control (Stream.id_to_int32 id) limit from till duration

    end

  end

  module T2MI = struct

    open Streams.T2MI
    include T2MI

    let get_state ~id control =
      get_result ~from:state_of_yojson
        ~path:Path.Format.(get_base_path () / ("state" @/ empty))
        ~query:Query.empty
        control id

    let get_structure ~id control =
      get_result ~from:structure_of_yojson
        ~path:Path.Format.(get_base_path () / ("structure" @/ empty))
        ~query:Query.empty
        control id

    let get_sequence ?duration ~id control =
      get_result ~from:sequence_of_yojson
        ~path:Path.Format.(get_base_path () / ("sequence" @/ empty))
        ~query:Query.[ "duration", (module Option(Time.Relative)) ]
        control id duration

    module Archive = struct

      let get_state ?limit ?compress ?from ?till ?duration ~id control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("state/archive" @/ empty))
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control id limit compress from till duration

      let get_structure ?limit ?from ?till ?duration ~id control =
        get_result ~from:(fun _ -> Error "FIXME Not implemented")
          ~path:Path.Format.(get_base_path () / ("structure/archive" @/ empty))
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control id limit from till duration

    end

  end

end
