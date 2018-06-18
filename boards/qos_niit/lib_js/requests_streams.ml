open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let ts   = "ts"
let t2mi = "t2mi"

let state     = "state"
let structure = "structure"
let bitrate   = "bitrate"
let sequence  = "sequence"
let section   = "section"

let make_path control path = Boards_js.Requests.make_path control ("streams"::path)

let make_t2mi_id id = Uri.Query.(make_query [ Api_js.Query.Stream.k_id, (module Option(Int)) ] id)


module WS = struct

  let get_streams control =
    let path = make_path control [ts] in
    WS.get ~path Common.Stream.t_list_of_yojson ()

  module TS = struct

    open Streams.TS

    let get_state control =
      let path = make_path control [ts;state] in
      WS.get ~path states_of_yojson ()

    let get_state_for_stream id control =
      let query = Api_js.Query.Stream.make (Some id) in
      let path  = make_path control [ts;state] in
      WS.get ~query ~path state_of_yojson ()

    let get_bitrate control =
      let path = make_path control [ts;bitrate] in
      WS.get ~path structures_of_yojson ()

    let get_bitrate_for_stream id control =
      let query = Api_js.Query.Stream.make (Some id) in
      let path  = make_path control [ts;bitrate] in
      WS.get ~query ~path structure_of_yojson ()

    let get_structure control =
      let path = make_path control [ts;structure] in
      WS.get ~path structures_of_yojson ()

    let get_structure_for_stream id control =
      let query = Api_js.Query.Stream.make (Some id) in
      let path  = make_path control [ts;structure] in
      WS.get ~query ~path structures_of_yojson ()

  end

  module T2MI = struct

    open Streams.T2MI

    let get_state control =
      let path = make_path control [t2mi;state] in
      WS.get ~path states_of_yojson ()

    let get_state_for_stream id control =
      let query = make_t2mi_id (Some id) in
      let path  = make_path control [t2mi;state] in
      WS.get ~query ~path states_of_yojson ()

    let get_structure control =
      let path = make_path control [t2mi;structure] in
      WS.get ~path structures_of_yojson ()

    let get_structure_for_stream id control =
      let query = make_t2mi_id (Some id) in
      let path  = make_path control [t2mi;structure] in
      WS.get ~query ~path structures_of_yojson ()

  end

end

module REST = struct

  let get_streams control =
    let path = make_path control [ts] in
    get_result ~path Common.Stream.t_list_of_yojson ()

  let get_stream id control =
    let query = Api_js.Query.Stream.make (Some id) in
    let path  = make_path control [ts] in
    get_result ~query ~path Common.Stream.t_opt_of_yojson ()

  module RT = struct

    module TS = struct

      open Streams.TS

      let get_bitrate control =
        let path = make_path control [ts;bitrate] in
        get_result ~path structures_of_yojson ()

      let get_bitrate_for_stream id control =
        let query = Api_js.Query.Stream.make (Some id) in
        let path  = make_path control [ts;bitrate] in
        get_result ~query ~path structure_opt_of_yojson ()

      let get_structure control =
        let path = make_path control [ts;structure] in
        get_result ~path structures_of_yojson ()

      let get_structure_for_stream id control =
        let query = Api_js.Query.Stream.make (Some id) in
        let path  = make_path control [ts;structure] in
        get_result ~query ~path structure_opt_of_yojson ()

      let get_si_psi_section ?section ?table_id_ext ?eit_ts_id ?eit_orig_nw_id
                             stream table_id control =
        let query = Uri.Query.(make_query [ "section",        (module Option(Int))
                                          ; "table_id_ext",   (module Option(Int))
                                          ; "eit_ts_id",      (module Option(Int))
                                          ; "eit_orig_nw_id", (module Option(Int))
                                          ]
                                          section table_id_ext eit_ts_id eit_orig_nw_id)
        in
        let path  = make_path control [ ts
                                      ; Int32.to_string (Stream.id_to_int32 stream)
                                      ; Int.to_string table_id]
        in get_result ~query ~path ~from_err:section_error_of_yojson section_of_yojson ()

    end

    module T2MI = struct

      open Streams.T2MI

      let get_structure control =
        let path = make_path control [t2mi;structure] in
        get_result ~path structures_of_yojson ()

      let get_structure_for_stream id control =
        let query = make_t2mi_id (Some id) in
        let path  = make_path control [t2mi;structure] in
        get_result ~query ~path structure_opt_of_yojson ()

      let get_sequence ?seconds ?stream control =
        let query =
          let id = make_t2mi_id stream in
          let s  = Uri.Query.(make_query ["seconds", (module Option(Int)) ] seconds) in
          Uri.Query.merge id s
        in
        let path  = make_path control [t2mi;sequence] in
        get_result ~query ~path sequence_of_yojson ()

    end

  end

  module AR = struct

    let to_query ?limit ?thin ?total ?stream time =
      let coll = Api_js.Query.Collection.make ?limit ?thin ?total () in
      let strm = match stream with
        | Some (`TS id)   -> Api_js.Query.Stream.make (Some id)
        | Some (`T2MI id) -> make_t2mi_id (Some id)
        | None            -> Uri.Query.empty
      in
      let time = Api_js.Query.Time.make time in
      List.fold_left Uri.Query.merge [] [coll;strm;time]

    let get_streams ?limit ?total time control =
      let query = to_query ?limit ?total time in
      let path  = make_path control [ts] in
      get_result ~query ~path (fun _ -> Error "not implemented") ()

    module TS = struct

      let get_state ?limit ?thin ?total time control =
        let query = to_query ?limit ?thin ?total time in
        let path  = make_path control [ts;state] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_state_for_stream ?limit ?thin ?total time id control =
        let query = to_query ?limit ?thin ?total ~stream:(`TS id) time in
        let path  = make_path control [ts;state] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_bitrate ?limit ?thin ?total time control =
        let query = to_query ?limit ?thin ?total time in
        let path  = make_path control [ts;bitrate] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_bitrate_for_stream ?limit ?thin ?total time id control =
        let query = to_query ?limit ?thin ?total ~stream:(`TS id) time in
        let path  = make_path control [ts;bitrate] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_structure ?limit ?total time control =
        let query = to_query ?limit ?total time in
        let path  = make_path control [ts;structure] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_structure_for_stream ?limit ?total time id control =
        let query = to_query ?limit ?total ~stream:(`TS id) time in
        let path  = make_path control [ts;structure] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

    end

    module T2MI = struct

      let get_state ?limit ?thin ?total time control =
        let query = to_query ?limit ?thin ?total time in
        let path  = make_path control [t2mi;state] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_state_for_stream ?limit ?thin ?total time id control =
        let query = to_query ?limit ?thin ?total ~stream:(`T2MI id) time in
        let path  = make_path control [t2mi;state] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_structure ?limit ?total time control =
        let query = to_query ?limit ?total time in
        let path  = make_path control [t2mi;structure] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

      let get_structure_for_stream ?limit ?total time id control =
        let query = to_query ?limit ?total ~stream:(`T2MI id) time in
        let path  = make_path control [t2mi;structure] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

    end

  end

end
