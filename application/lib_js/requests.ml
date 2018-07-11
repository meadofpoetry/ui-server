open Containers
open Common.Topology
open Api_js.Requests.Json_request
open Common.Uri
open Application_types

module WS = struct

  let get_topology () =
    WS.get ?secure:None ?host:None ?port:None
      ~from:Common.Topology.of_yojson
      ~path:Path.Format.("api/topology" @/ empty)
      ~query:Query.empty

  let get_streams () =
    WS.get ?secure:None ?host:None ?port:None
      ~from:stream_table_of_yojson
      ~path:Path.Format.("api/topology/streams" @/ empty)
      ~query:Query.empty

end

module HTTP = struct

  let set_streams streams =
    post_result_unit ?scheme:None ?host:None ?port:None
      ~path:Path.Format.("api/topology/streams" @/ empty)
      ~contents:(stream_setting_to_yojson streams)
      ~from_err:set_error_of_yojson
      ~query:Query.empty

  let get_topology () =
    get_result ?scheme:None ?host:None ?port:None ?from_err:None
      ~from:Common.Topology.of_yojson
      ~path:Path.Format.("api/topology" @/ empty)
      ~query:Query.empty

  let get_streams () =
    get_result ?scheme:None ?host:None ?port:None ?from_err:None
      ~from:stream_table_of_yojson
      ~path:Path.Format.("api/topology/streams" @/ empty)
      ~query:Query.empty

end
