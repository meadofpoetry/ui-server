open Containers
open Board_types
open Api_js.Requests.Json
open Requests_common
open Common

open Api_utils.Streams

let req_to_uri ?uri control req = req_to_uri ?uri control (`Streams req)

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
        get_result structure_response_of_yojson (Uri.to_string uri)

      let get_structure control =
        let uri = req_to_uri control (`Structure (`TS None)) in
        get_result structures_of_yojson (Uri.to_string uri)

      let get_structure_for_stream id control =
        let uri = req_to_uri control (`Structure (`TS (Some id))) in
        get_result structure_response_of_yojson (Uri.to_string uri)

      let get_si_psi_section (req:section_request) control =
        let uri = Query.(
            (Uri.empty,req.section)
            >>* (fun (u,sec) -> set section_query sec u, req.table_id_ext)
            >>* (fun (u,ext) -> set table_id_ext_query ext u, req.eit_ts_id)
            >>* (fun (u,sid) -> set eit_ts_id_query sid u, req.eit_orig_nw_id)
            >>* (fun (u,nid) -> set eit_orig_nw_id_query nid u, None)
            |>  (fun (uri,_) -> let req = `Section (req.stream_id, req.table_id) in
                                req_to_uri ~uri control req))
        in get_result section_of_yojson ~from_err:section_error_of_yojson (Uri.to_string uri)

    end

    module T2MI = struct

      open Streams.T2MI

      let get_structure control =
        let uri = req_to_uri control (`Structure (`T2MI None)) in
        get_result structures_of_yojson (Uri.to_string uri)

      let get_structure_for_stream id control =
        let uri = req_to_uri control (`Structure (`T2MI (Some id))) in
        get_result structure_response_of_yojson (Uri.to_string uri)

      let get_sequence ?seconds ?stream control =
        let uri = Query.(
            (Uri.empty,seconds)
            >>* (fun (u,sec) -> set seconds_query sec u, None)
            |>  (fun (uri,_) -> req_to_uri ~uri control (`Sequence stream)))
        in get_result sequence_of_yojson (Uri.to_string uri)

    end

  end

  module AR = struct

    let to_uri ?limit ?thin ?total time req control =
      Query.((Uri.empty,limit)
             >>* (fun (u,lim) -> set limit_query lim u, thin)
             >>* (fun (u,thn) -> set thin_query thn u, total)
             >>* (fun (u,tot) -> set total_query tot u, None)
             |>  (fun (u,_)   -> set_time_query time u)
             |>  (fun uri     -> req_to_uri ~uri control req))

    let get_streams ?limit ?total time control =
      let uri = to_uri ?limit ?total time (`Streams None) control in
      get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    module TS = struct

      let get_state ?filter ?limit ?thin ?total time control =
        let uri,_ = Query.(((to_uri ?limit ?thin ?total time (`State (`TS None)) control), filter)
                           >>* (fun (u,fil) -> set state_query fil u, None))
        in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_state_for_stream ?filter ?limit ?thin ?total time id control =
        let uri,_ = Query.(((to_uri ?limit ?thin ?total time (`State (`TS (Some id))) control), filter)
                           >>* (fun (u,fil) -> set state_query fil u, None))
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
        let uri,_ = Query.(((to_uri ?limit ?thin ?total time (`State (`T2MI None)) control), filter)
                           >>* (fun (u,fil) -> set state_query fil u, None))
        in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

      let get_state_for_stream ?filter ?limit ?thin ?total time id control =
        let uri,_ = Query.(((to_uri ?limit ?thin ?total time (`State (`T2MI (Some id))) control), filter)
                           >>* (fun (u,fil) -> set state_query fil u, None))
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
