open Api_js.Requests.Json_request
open Common

module WS = struct

  let get_streams ?(inputs = []) ?(ids = []) () =
    WS.get
      ~path:Uri.Path.Format.("api/pipeline/structure" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input)) ]
      ~from:(Json.List.of_yojson Structure.of_yojson)
      ids inputs

  let get_applied ?(inputs = []) ?(ids = []) () =
    WS.get
      ~path:Uri.Path.Format.("api/pipeline/structure/applied" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input)) ]
      ~from:(Json.List.of_yojson Structure.of_yojson)
      ids inputs

  let get_streams_with_source ?(inputs = []) ?(ids = []) () =
    WS.get
      ~path:Uri.Path.Format.("api/pipeline/structure/with_source" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input)) ]
      ~from:(Json.List.of_yojson Structure.packed_of_yojson)
      ids inputs

  let get_applied_with_source ?(inputs = []) ?(ids = []) () =
    WS.get
      ~path:Uri.Path.Format.("api/pipeline/structure/applied_with_source" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input)) ]
      ~from:(Json.List.of_yojson Structure.packed_of_yojson)
      ids inputs

end

module HTTP = struct

  let apply_streams s =
    post_result ?scheme:None ?from_err:None ?host:None ?port:None
      ~path:Uri.Path.Format.("api/pipeline/structure" @/ empty)
      ~query:Uri.Query.empty
      ~contents:(Json.List.to_yojson Structure.to_yojson s)
      ~from:(fun _ -> Ok ()) (* TODO proper error checking *)

  let get_streams ?(inputs = []) ?(ids = []) () =
    get_result
      ~path:Uri.Path.Format.("api/pipeline/structure" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))  ]
      ~from:(Json.List.of_yojson Structure.of_yojson)
      ids inputs

  let get_applied ?(inputs = []) ?(ids = []) () =
    get_result
      ~path:Uri.Path.Format.("api/pipeline/structure/applied" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))  ]
      ~from:(Json.List.of_yojson Structure.of_yojson)
      ids inputs

  let get_streams_with_source ?(inputs = []) ?(ids = []) () =
    get_result
      ~path:Uri.Path.Format.("api/pipeline/structure/with_source" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))  ]
      ~from:(Json.List.of_yojson Structure.packed_of_yojson)
      ids inputs

  let get_applied_with_source ?(inputs = []) ?(ids = []) () =
    get_result
      ~path:Uri.Path.Format.("api/pipeline/structure/applied_with_source" @/ empty)
      ~query:Uri.Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))  ]
      ~from:(Json.List.of_yojson Structure.packed_of_yojson)
      ids inputs  

end
