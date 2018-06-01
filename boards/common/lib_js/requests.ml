open Containers
open Api_js.Requests.Json
open Lwt.Infix
open Common

let req_to_uri ~conv control req : Uri.t =
  let path = ["/api";"board";string_of_int control] @ (conv req)
             |> String.concat "/" in
  Uri.with_path Uri.empty path

module Device = struct

  let req_to_uri ?uri control req = req_to_uri ~conv:Api_common.req_to_path control (`Device req)

  module WS = struct

    let get_state control =
      let uri = req_to_uri control `State in
      WS.get (Uri.to_string uri) Topology.state_of_yojson

  end

  module REST = struct

    (** Sets board port to listen **)
    let post_port control port state =
      let uri = req_to_uri control (`Port (port,state)) in
      post_result_unit (Uri.to_string uri)

    module RT = struct

      let get_state control =
        let uri = req_to_uri control `State in
        get_result Topology.state_of_yojson (Uri.to_string uri)

    end

    module AR = struct

      let get_state ?filter ?limit ?total time control =
        let uri = Api_common.Device.Query.(
            req_to_uri control `State
            |> set state_query filter
            |> set limit_query limit
            |> set total_query total
            |> set_time_query  time)
        in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    end

  end

end
