open Containers
open Api_js.Requests.Json
open Lwt.Infix

let req_to_uri ?(uri=Uri.empty) ~conv control req : Uri.t =
  let path = ["/api";"board";string_of_int control] @ (conv req)
             |> String.concat "/" in
  Uri.with_path uri path

module Device = struct

  let req_to_uri ?uri control req = req_to_uri ?uri ~conv:Api_common.req_to_path control (`Device req)

  module WS = struct

    let get_state control =
      let uri = req_to_uri control `State in
      WS.get (Uri.to_string uri) Common.Topology.state_of_yojson

  end

  module REST = struct

    (** Sets board port to listen **)
    let post_port control port state =
      let uri = req_to_uri control (`Port (port,state)) in
      post_result_unit (Uri.to_string uri)

    module RT = struct

      let get_state control =
        let uri = req_to_uri control `State in
        get_result Common.Topology.state_of_yojson (Uri.to_string uri)

    end

    module AR = struct

      let get_state ?filter ?limit ?total time control =
        let uri = Api_common.Device.Query.(
            (Uri.empty,filter)
            >>* (fun (u,fil) -> set state_query fil u, limit)
            >>* (fun (u,lim) -> set limit_query lim u, total)
            >>* (fun (u,tot) -> set total_query tot u, None)
            |>  (fun (u,_)   -> set_time_query time u)
            |>  (fun uri     -> req_to_uri ~uri control `State))
        in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    end

  end

end
