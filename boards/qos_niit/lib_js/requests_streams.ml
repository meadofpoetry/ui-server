open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () = Uri.Path.Format.(
    Boards_js.Requests.get_board_path () / ("streams" @/ empty))

module WS = struct

  open Common.Uri
  open Stream

  let get_streams ?(inputs = []) ?(ids = []) ?incoming control =
    WS.get ~from:(Json.List.of_yojson of_yojson)
      ~path:(get_base_path ())
      ~query:Query.[ "id", (module List(ID))
                   ; "input", (module List(Topology.Show_topo_input))
                   ; "incoming", (module Option(Bool)) ]
      control ids inputs incoming

  let get_bitrate ?(ids = []) control =
    WS.get ~from:bitrates_of_yojson
      ~path:Path.Format.(get_base_path () / ("bitrate" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_ts_info ?(ids = []) control =
    WS.get ~from:ts_info_of_yojson
      ~path:Path.Format.(get_base_path () / ("ts-info" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_services ?(ids = []) control =
    WS.get ~from:services_of_yojson
      ~path:Path.Format.(get_base_path () / ("services" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_sections ?(ids = []) control =
    WS.get ~from:sections_of_yojson
      ~path:Path.Format.(get_base_path () / ("sections" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_tables ?(ids = []) control =
    WS.get ~from:tables_of_yojson
      ~path:Path.Format.(get_base_path () / ("tables" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_pids ?(ids = []) control =
    WS.get ~from:pids_of_yojson
      ~path:Path.Format.(get_base_path () / ("pids" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_t2mi_info ?(ids = []) control =
    WS.get ~from:t2mi_info_of_yojson
      ~path:Path.Format.(get_base_path () / ("t2mi-info" @/ empty))
      ~query:Query.[ "id", (module List(Stream.ID))
                   ; "t2mi-stream-id", (module List(Int)) ]
      control ids

  let get_errors ?(errors = []) ?(priority = []) ?(pids = [])
        ?(ids = []) control =
    WS.get ~from:errors_of_yojson
      ~path:Path.Format.(get_base_path () / ("errors" @/ empty))
      ~query:Query.[ "ids", (module List(Stream.ID))
                   ; "errors", (module List(Int))
                   ; "priority", (module List(Int))
                   ; "pid", (module List(Int)) ]
      control ids errors priority pids

end

module HTTP = struct

  open Common.Uri
  open Stream

  let get_streams ?(ids = []) ?(inputs = []) ?incoming control =
    get_result ~from:(Json.List.of_yojson Stream.of_yojson)
      ~path:(get_base_path ())
      ~query:Query.[ "id", (module List(ID))
                   ; "input", (module List(Topology.Show_topo_input))
                   ; "incoming", (module Option(Bool)) ]
      control ids inputs incoming

  let get_ts_info ?(ids = []) control =
    get_result ~from:ts_info_of_yojson
      ~path:Path.Format.(get_base_path () / ("ts-info" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_pids ?(ids = []) control =
    get_result ~from:pids_of_yojson
      ~path:Path.Format.(get_base_path () / ("pids" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_services ?(ids = []) control =
    get_result ~from:services_of_yojson
      ~path:Path.Format.(get_base_path () / ("services" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_sections ?(ids = []) control =
    get_result ~from:sections_of_yojson
      ~path:Path.Format.(get_base_path () / ("sections" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_tables ?(ids = []) control =
    get_result ~from:tables_of_yojson
      ~path:Path.Format.(get_base_path () / ("tables" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_t2mi_info ?(ids = []) control =
    get_result ~from:t2mi_info_of_yojson
      ~path:Path.Format.(get_base_path () / ("t2mi-info" @/ empty))
      ~query:Query.["id", (module List(Stream.ID))]
      control ids

  let get_si_psi_section ?section ?table_id_ext ?id_ext_1 ?id_ext_2
        ~id ~table_id control =
    let open SI_PSI_section.Dump in
    get_result ~from:(Time.timestamped_of_yojson of_yojson)
      ~from_err:error_of_yojson
      ~path:Path.Format.(get_base_path () / ("dump/si-psi-section"
                                             @/ ID.fmt ^/ Int ^/ empty))
      ~query:Query.[ "section", (module Option(Int))
                   ; "table-id-ext", (module Option(Int))
                   ; "id-ext-1", (module Option(Int))
                   ; "id-ext-2", (module Option(Int)) ]
      control id table_id section table_id_ext id_ext_1 id_ext_2

  let get_t2mi_sequence ?duration ~id control =
    let open T2mi_sequence in
    get_result ~from:(Time.timestamped_of_yojson of_yojson)
      ~path:Path.Format.(get_base_path () / (ID.fmt ^/ "dump/t2mi-sequence" @/ empty))
      ~query:Query.[ "duration", (module Option(Time.Relative)) ]
      control id duration

end

