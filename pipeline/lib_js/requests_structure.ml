open Api_js.Requests.Json_request
open Common

module WS = struct

  let get ?(inputs = []) ?(ids = []) ?analyzed () =
    WS.get
      ~path:Uri.Path.Format.("api/pipeline/structure" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))
                       ; "analyzed", (module Option(Bool)) ]
      ~from:Structure.Streams.of_yojson
      ids inputs analyzed

  let get_streams ?(inputs = []) ?(ids = []) ?analyzed () =
    WS.get
      ~path:Uri.Path.Format.("api/pipeline/structure/streams" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))
                       ; "analyzed", (module Option(Bool)) ]
      ~from:(Json.List.of_yojson Stream.of_yojson)
      ids inputs analyzed

end

module HTTP = struct

  let set s =
    post_result ?scheme:None ?from_err:None ?host:None ?port:None
      ~path:Uri.Path.Format.("api/pipeline/structure" @/ empty)
      ~query:Uri.Query.empty
      ~contents:(Structure.Streams.to_yojson s)
      ~from:(fun _ -> Ok ())

  let get ?(inputs = []) ?(ids = []) ?analyzed () =
    get_result
      ~path:Uri.Path.Format.("api/pipeline/structure" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))
                       ; "analyzed", (module Option(Bool)) ]
      ~from:Structure.Streams.of_yojson
      ids inputs analyzed

  let get_streams ?(inputs = []) ?(ids = []) ?analyzed () =
    get_result
      ~path:Uri.Path.Format.("api/pipeline/structure/streams" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))
                       ; "analyzed", (module Option(Bool)) ]
      ~from:(Json.List.of_yojson Stream.of_yojson)
      ids inputs analyzed

end
