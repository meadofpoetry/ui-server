open Containers
open Api_js.Requests.Json_request
open Application_types
open Common

module WS = struct

  let get_topology () =
    WS.get ?secure:None ?host:None ?port:None
      ~from:Common.Topology.of_yojson
      ~path:Uri.Path.Format.("api/topology" @/ empty)
      ~query:Uri.Query.empty

  let get_streams () =
    WS.get ?secure:None ?host:None ?port:None
      ~from:stream_table_of_yojson
      ~path:Uri.Path.Format.("api/topology/streams" @/ empty)
      ~query:Uri.Query.empty

end

module HTTP = struct

  let set_streams streams =
    post_result_unit ?scheme:None ?host:None ?port:None
      ~path:Uri.Path.Format.("api/topology/streams" @/ empty)
      ~contents:(stream_setting_to_yojson streams)
      ~from_err:Stream.Table.set_error_of_yojson
      ~query:Uri.Query.empty

  let get_topology () =
    get_result ?scheme:None ?host:None ?port:None ?from_err:None
      ~from:Topology.of_yojson
      ~path:Uri.Path.Format.("api/topology" @/ empty)
      ~query:Uri.Query.empty

  let get_streams () =
    get_result ?scheme:None ?host:None ?port:None ?from_err:None
      ~from:stream_table_of_yojson
      ~path:Uri.Path.Format.("api/topology/streams" @/ empty)
      ~query:Uri.Query.empty

end
