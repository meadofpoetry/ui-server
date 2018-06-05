open Containers
open Board_types
open Api_js.Requests.Json
open Requests_common
open Common

open Api_utils.Errors

let req_to_uri control req = req_to_uri control (`Errors req)

module WS = struct

  module TS = struct

    open Errors.TS

    let get_errors ?stream control =
      let uri = req_to_uri control (`Errors (`TS stream)) in
      WS.get (Uri.to_string uri) t_list_of_yojson

  end

  module T2MI = struct

    open Errors.T2MI

    let get_errors ?stream control =
      let uri = req_to_uri control (`Errors (`T2MI stream)) in
      WS.get (Uri.to_string uri) t_list_of_yojson

  end

end

module REST = struct

  module AR = struct

    let to_errors_uri ?errors ?level ?limit ?thin ?total time stream control =
      let uri = Query.(req_to_uri control (`Errors stream)
                       |> set errors_query errors
                       |> set level_query  level
                       |> set limit_query  limit
                       |> set total_query  total
                       |> set thin_query   thin
                       |> set_time_query   time)
      in Uri.to_string uri

    let to_percent_uri ?errors ?level time stream control =
      let uri = Query.(req_to_uri control (`Percent stream)
                       |> set errors_query errors
                       |> set level_query  level
                       |> set_time_query   time)
      in Uri.to_string uri

    let to_has_any_uri ?errors ?level time stream control =
      let uri = Query.(req_to_uri control (`Has_any stream)
                       |> set errors_query errors
                       |> set level_query  level
                       |> set_time_query   time)
      in Uri.to_string uri

    module TS = struct

      let get_errors ?errors ?priority ?limit ?thin ?total time control =
        let uri = to_errors_uri ?errors ?level:priority ?limit ?thin ?total time (`TS None) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_errors_for_stream ?errors ?priority ?limit ?thin ?total time id control =
        let uri = to_errors_uri ?errors ?level:priority ?limit ?thin ?total time (`TS id) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_percent ?errors ?priority time control =
        let uri = to_percent_uri ?errors ?level:priority time (`TS None) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_percent_for_stream ?errors ?priority time id control =
        let uri = to_percent_uri ?errors ?level:priority time (`TS id) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_has_any ?errors ?priority time control =
        let uri = to_has_any_uri ?errors ?level:priority time (`TS None) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_has_any_for_stream ?errors ?priority time id control =
        let uri = to_has_any_uri ?errors ?level:priority time (`TS id) control in
        get_result (fun _ -> Error "not implemented") uri

    end

    module T2MI = struct

      let get_errors ?errors ?level ?limit ?thin ?total time control =
        let uri = to_errors_uri ?errors ?level ?limit ?thin ?total time (`T2MI None) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_errors_for_stream ?errors ?level ?limit ?thin ?total time id control =
        let uri = to_errors_uri ?errors ?level ?limit ?thin ?total time (`T2MI id) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_percent ?errors ?level time control =
        let uri = to_percent_uri ?errors ?level time (`T2MI None) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_percent_for_stream ?errors ?level time id control =
        let uri = to_percent_uri ?errors ?level time (`T2MI id) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_has_any ?errors ?level time control =
        let uri = to_has_any_uri ?errors ?level time (`T2MI None) control in
        get_result (fun _ -> Error "not implemented") uri

      let get_has_any_for_stream ?errors ?level time id control =
        let uri = to_has_any_uri ?errors ?level time (`T2MI id) control in
        get_result (fun _ -> Error "not implemented") uri

    end

  end

end
