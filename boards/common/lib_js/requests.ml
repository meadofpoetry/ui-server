open Containers
open Api_js.Requests.Json_request
open Lwt.Infix
open Common

let make_path control (path:Uri.Path.t) : string =
  ["/api";"board";string_of_int control] @ path
  |> Uri.Path.to_string

module Device = struct

  let make_path control path = make_path control ("device"::path)

  module WS = struct

    let get_state control =
      let path = make_path control ["state"] in
      WS.get ~path Topology.state_of_yojson ()

  end

  module REST = struct

    (** Sets board port to listen **)
    let post_port control port state =
      let path = make_path control ["port";string_of_int port;string_of_bool state] in
      post_result_unit ~path ()

    module RT = struct

      let get_state control =
        let path = make_path control ["state"] in
        get_result ~path Topology.state_of_yojson ()

    end

    module AR = struct

      (* TODO add filter *)
      let get_state ?limit ?total time control =
        let query =
          let open Uri.Query in
          let coll = Api_js.Query.Collection.make ?limit ?total () in
          let time = Api_js.Query.Time.make time in
          merge coll time
        in
        let path = make_path control ["state"] in
        get_result ~query ~path (fun _ -> Error "not implemented") ()

    end

  end

end
