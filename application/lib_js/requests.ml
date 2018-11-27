open Containers
open Api_js.Requests.Json_request
open Api_js.Api_types
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
      ~path:Uri.Path.Format.("api/topology/stream_table" @/ empty)
      ~query:Uri.Query.empty

  let get_log ?(inputs = []) ?(streams = []) () =
    WS.get ?secure:None ?host:None ?port:None
      ~from:(Json.List.of_yojson Stream.Log_message.of_yojson)
      ~path:Uri.Path.Format.("api/topology/log" @/ empty)
      ~query:Uri.Query.["input", (module List(Topology.Show_topo_input));
                        "id", (module List(Stream.ID))]
      inputs streams
    
end

module HTTP = struct

  let set_streams streams =
    post_result_unit ?scheme:None ?host:None ?port:None
      ~path:Uri.Path.Format.("api/topology/stream_table" @/ empty)
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
      ~path:Uri.Path.Format.("api/topology/stream_table" @/ empty)
      ~query:Uri.Query.empty

  let get_all_streams () =
    get_result ?scheme:None ?host:None ?port:None ?from_err:None
      ~from:Application_types.stream_list_of_yojson
      ~path:Uri.Path.Format.("api/topology/streams" @/ empty)
      ~query:Uri.Query.["input", (module Single(Topology.Show_topo_input))]

  let get_stream_source ~stream_id () =
    get_result ?scheme:None ?host:None ?port:None ?from_err:None
      ~from:Stream.source_of_yojson
      ~path:Uri.Path.Format.("api/topology/source" @/ empty)
      ~query:Uri.Query.["id", (module Single(Stream.ID))]
      stream_id

  let get_log ?(boards = []) ?(cpu = [])?(inputs = []) ?(streams = [])
        ?limit ?from ?till ?duration  () =
    get_result ?scheme:None ?host:None ?port:None ?from_err:None
      ~from:(rows_of_yojson
               (Json.List.of_yojson Stream.Log_message.of_yojson)
               (fun _ -> assert false))
      ~path:Uri.Path.Format.("api/topology/log" @/ empty)
      ~query:Uri.Query.[ "board", (module List(Int))
                       ; "cpu", (module List(String))
                       ; "input", (module List(Topology.Show_topo_input))
                       ; "id", (module List(Stream.ID))
                       ; "limit", (module Option(Int))
                       ; "from", (module Option(Time.Show))
                       ; "to", (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
      boards cpu inputs streams limit from till duration

end
