open Containers
open Board_types
open Api_js.Requests.Json
open Requests_common
open Common

open Api_utils.Streams

let req_to_uri control req = req_to_uri control (`Streams req)

module WS = struct

  let get_streams control =
    let uri = req_to_uri control (`Streams None) in
    WS.get (Uri.to_string uri) Common.Stream.t_list_of_yojson

  module TS = struct

    open Streams.TS

    let get_state control =
      let uri = req_to_uri control (`State (`TS None)) in
      WS.get (Uri.to_string uri) states_of_yojson

    let get_state_for_stream id control =
      let uri = req_to_uri control (`State (`TS (Some id))) in
      WS.get (Uri.to_string uri) state_of_yojson

    let get_bitrate control =
      let uri = req_to_uri control (`Bitrate None) in
      WS.get (Uri.to_string uri) structures_of_yojson

    let get_bitrate_for_stream id control =
      let uri = req_to_uri control (`Bitrate (Some id)) in
      WS.get (Uri.to_string uri) structure_of_yojson

    let get_structure control =
      let uri = req_to_uri control (`Structure (`TS None)) in
      WS.get (Uri.to_string uri) structures_of_yojson

    let get_structure_for_stream id control =
      let uri = req_to_uri control (`Structure (`TS (Some id))) in
      WS.get (Uri.to_string uri) structures_of_yojson

  end

  module T2MI = struct

    open Streams.T2MI

    let get_state control =
      let uri = req_to_uri control (`State (`T2MI None)) in
      WS.get (Uri.to_string uri) states_of_yojson

    let get_state_for_stream id control =
      let uri = req_to_uri control (`State (`T2MI (Some id))) in
      WS.get (Uri.to_string uri) state_of_yojson

    let get_structure control =
      let uri = req_to_uri control (`Structure (`T2MI None)) in
      WS.get (Uri.to_string uri) structures_of_yojson

    let get_structure_for_stream id control =
      let uri = req_to_uri control (`Structure (`T2MI (Some id))) in
      WS.get (Uri.to_string uri) structures_of_yojson

  end

end

module REST = struct

  let get_streams control =
    let uri = req_to_uri control (`Streams None) in
    get_result Common.Stream.t_list_of_yojson (Uri.to_string uri)

  let get_stream id control =
    let uri = req_to_uri control (`Streams (Some id)) in
    get_result Common.Stream.t_opt_of_yojson (Uri.to_string uri)

  module RT = struct

    module TS = struct

      open Streams.TS

      let get_bitrate control =
        let uri = req_to_uri control (`Bitrate None) in
        get_result structures_of_yojson (Uri.to_string uri)

      let get_bitrate_for_stream id control =
        let uri = req_to_uri control (`Bitrate (Some id)) in
        get_result structure_opt_of_yojson (Uri.to_string uri)

      let get_structure control =
        let uri = req_to_uri control (`Structure (`TS None)) in
        get_result structures_of_yojson (Uri.to_string uri)

      let get_structure_for_stream id control =
        let uri = req_to_uri control (`Structure (`TS (Some id))) in
        get_result structure_opt_of_yojson (Uri.to_string uri)

      let get_si_psi_section ?section ?table_id_ext ?eit_ts_id ?eit_orig_nw_id
                             stream table_id control =
        let uri = Query.(req_to_uri control (`Section (stream, table_id))
                         |> set section_query section
                         |> set table_id_ext_query table_id_ext
                         |> set eit_ts_id_query eit_ts_id
                         |> set eit_orig_nw_id_query eit_orig_nw_id)
        in get_result section_of_yojson ~from_err:section_error_of_yojson (Uri.to_string uri)

    end

    module T2MI = struct

      open Streams.T2MI

      let get_structure control =
        let uri = req_to_uri control (`Structure (`T2MI None)) in
        get_result structures_of_yojson (Uri.to_string uri)

      let get_structure_for_stream id control =
        let uri = req_to_uri control (`Structure (`T2MI (Some id))) in
        get_result structure_opt_of_yojson (Uri.to_string uri)

      let get_sequence ?seconds ?stream control =
        let uri = Query.(req_to_uri control (`Sequence stream)
                         |> set seconds_query seconds)
        in get_result sequence_of_yojson (Uri.to_string uri)

    end

  end

  module AR = struct

    let to_uri ?limit ?thin ?total time req control =
      Query.(req_to_uri control req
             |> set limit_query limit
             |> set thin_query  thin
             |> set total_query total
             |> set_time_query  time)

    let get_streams ?limit ?total time control =
      let uri = to_uri ?limit ?total time (`Streams None) control in
      get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    module TS = struct

      let get_state ?filter ?limit ?thin ?total time control =
        let uri = Query.(to_uri ?limit ?thin ?total time (`State (`TS None)) control
                         |> set state_query filter)
        in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_state_for_stream ?filter ?limit ?thin ?total time id control =
        let uri = Query.(to_uri ?limit ?thin ?total time (`State (`TS (Some id))) control
                         |> set state_query filter)
        in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_bitrate ?limit ?thin ?total time control =
        let uri = to_uri ?limit ?thin ?total time (`Bitrate None) control in
        get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_bitrate_for_stream ?limit ?thin ?total time id control =
        let uri = to_uri ?limit ?thin ?total time (`Bitrate (Some id)) control in
        get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_structure ?limit ?total time control =
        let uri = to_uri ?limit ?total time (`Structure (`TS None)) control in
        get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_structure_for_stream ?limit ?total time id control =
        let uri = to_uri ?limit ?total time (`Structure (`TS (Some id))) control in
        get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    end

    module T2MI = struct

      let get_state ?filter ?limit ?thin ?total time control =
        let uri = Query.(to_uri ?limit ?thin ?total time (`State (`T2MI None)) control
                         |> set state_query filter)
        in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_state_for_stream ?filter ?limit ?thin ?total time id control =
        let uri = Query.(to_uri ?limit ?thin ?total time (`State (`T2MI (Some id))) control
                         |> set state_query filter)
        in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_structure ?limit ?total time control =
        let uri = to_uri ?limit ?total time (`Structure (`T2MI None)) control in
        get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_structure_for_stream ?limit ?total time id control =
        let uri = to_uri ?limit ?total time (`Structure (`T2MI (Some id))) control in
        get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    end

  end

end
