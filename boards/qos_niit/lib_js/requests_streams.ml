open Containers
open Board_types
open Api_js.Requests.Json_request
open Api_js.Api_types
open Common

let get_base_path () = Uri.Path.Format.(
    Boards_js.Requests.get_board_path () / ("streams" @/ empty))

module WS = struct

  open Common.Uri
  open Streams.TS
  open Stream

  let get_streams ?(inputs=[]) ?(ids=[]) control =
    WS.get ~from:(Json.List.of_yojson of_yojson)
      ~path:Path.Format.(get_base_path ())
      ~query:Query.[ "id",    (module List(ID))
                   ; "input", (module List(Topology.Show_topo_input)) ]
      control ids inputs

  let get_bitrate ~id control =
    WS.get ~from:bitrate_of_yojson
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "bitrate" @/ empty))
      ~query:Query.empty
      control id

  let get_info ~id control =
    WS.get ~from:info_of_yojson
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "info" @/ empty))
      ~query:Query.empty
      control id

  let get_services ~id control =
    WS.get ~from:services_of_yojson
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "services" @/ empty))
      ~query:Query.empty
      control id

  let get_tables ~id control =
    WS.get ~from:tables_of_yojson
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "tables" @/ empty))
      ~query:Query.empty
      control id

  let get_pids ~id control =
    WS.get ~from:pids_of_yojson
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "pids" @/ empty))
      ~query:Query.empty
      control id

  module T2MI = struct

    open Streams.T2MI

    let get_structure ~id control =
      WS.get ~from:structure_of_yojson
        ~path:Path.Format.(get_base_path () / (Int32 ^/ "t2mi/structure" @/ empty))
        ~query:Query.empty
        control id

  end

  module Errors = struct

    open Errors

    let get_errors ?(errors=[]) ?(priority=[]) ?(pids=[]) ~id control =
      WS.get ~from:(Json.List.of_yojson of_yojson)
        ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "errors" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))]
        control id errors priority pids

  end

end

module HTTP = struct

  open Common.Uri
  open Streams.TS
  open Stream

  let get_streams ?(ids=[]) ?(inputs=[]) ?limit ?compress ?from ?till ?duration control =
    get_result ~from:(Api_js.Api_types.rows_of_yojson
                        streams_states_of_yojson
                        streams_unique_of_yojson)
      ~path:(get_base_path ())
      ~query:Query.[ "id", (module List(ID))
                   ; "input", (module List(Topology.Show_topo_input))
                   ; "limit", (module Option(Int))
                   ; "compress", (module Option(Bool))
                   ; "from", (module Option(Time.Show))
                   ; "to", (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control ids inputs limit compress from till duration

  let get_si_psi_section ?section ?table_id_ext ?ext_info_1 ?ext_info_2 ~id ~table_id control =
    get_result ~from:section_of_yojson
      ~from_err:section_error_of_yojson
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "section" @/ Int ^/ empty))
      ~query:Query.[ "section", (module Option(Int))
                   ; "table-id-ext", (module Option(Int))
                   ; "ext-info-1", (module Option(Int))
                   ; "ext-info-2", (module Option(Int)) ]
      control id table_id section table_id_ext ext_info_1 ext_info_2

  let get_bitrate ?limit ?compress ?from ?till ?duration ~id control =
    get_result ~from:(fun _ -> Error "FIXME Not implemented")
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "bitrate" @/ empty))
      ~query:Query.[ "limit", (module Option(Int))
                   ; "compress", (module Option(Bool))
                   ; "from", (module Option(Time.Show))
                   ; "to", (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control id limit compress from till duration

  let raw_of_yojson f =
    Api_js.Api_types.rows_of_yojson
      Json.(List.of_yojson (Pair.of_yojson Stream.ID.of_yojson f))
      (fun _ -> Error "cannot be compressed")

  let get_info ?limit ?from ?till ?duration ~id control =
    get_result
      ~from:(raw_of_yojson info_of_yojson)
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "info" @/ empty))
      ~query:Query.[ "limit", (module Option(Int))
                   ; "from", (module Option(Time.Show))
                   ; "to", (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control id limit from till duration

  let get_services ?limit ?from ?till ?duration ~id control =
    get_result
      ~from:(raw_of_yojson services_of_yojson)
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "services" @/ empty))
      ~query:Query.[ "limit", (module Option(Int))
                   ; "from", (module Option(Time.Show))
                   ; "to", (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control id limit from till duration

  let get_tables ?limit ?from ?till ?duration ~id control =
    get_result
      ~from:(raw_of_yojson tables_of_yojson)
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "tables" @/ empty))
      ~query:Query.[ "limit", (module Option(Int))
                   ; "from", (module Option(Time.Show))
                   ; "to", (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control id limit from till duration

  let get_pids ?limit ?from ?till ?duration ~id control =
    get_result
      ~from:(raw_of_yojson pids_of_yojson)
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "pids" @/ empty))
      ~query:Query.[ "limit", (module Option(Int))
                   ; "from", (module Option(Time.Show))
                   ; "to", (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control id limit from till duration

  module T2MI = struct

    open Streams.T2MI

    let get_sequence ?duration ~id control =
      get_result ~from:sequence_of_yojson
        ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "sequence" @/ empty))
        ~query:Query.[ "duration", (module Option(Time.Relative)) ]
        control id duration

    let get_structure ?limit ?from ?till ?duration ~id control =
      get_result
        ~from:(raw_of_yojson structure_of_yojson)
        ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "structure" @/ empty))
        ~query:Query.[ "limit",    (module Option(Int))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control id limit from till duration

  end

  module Errors = struct

    open Errors

    let get_errors?(errors=[]) ?(priority=[]) ?(pids=[])
          ?limit ?compress ?order ?from ?till ?duration ~id control =
      let desc = Option.map (function `Asc -> false | `Desc -> true) order in
      get_result ~from:(rows_of_yojson raw_of_yojson compressed_of_yojson)
        ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "errors" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))
                     ; "limit",    (module Option(Int))
                     ; "compress", (module Option(Bool))
                     ; "desc",     (module Option(Bool))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control id errors priority pids limit compress desc from till duration

    let get_errors_percent ?(errors=[]) ?(priority=[]) ?(pids=[])
          ?from ?till ?duration ~id control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "errors/percent" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control id errors priority pids from till duration

    let get_errors_has_any ?(errors=[]) ?(priority=[]) ?(pids=[])
          ?from ?till ?duration ~id control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "errors/has-any" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control id errors priority pids from till duration

  end

end
