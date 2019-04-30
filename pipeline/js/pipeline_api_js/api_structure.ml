open Api_common
open Application_types
open Pipeline_types
open Netlib.Uri

module Event = struct

  let get_streams ?on_error ?f ?(inputs = []) ?(ids = []) () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("api/pipeline/streams" @/ empty)
        ~query:Query.[ "id", (module List(Stream.ID))
                     ; "input", (module List(Topology.Show_topo_input)) ]
        ids inputs in
    t

  let get_applied ?(inputs = []) ?(ids = []) () =
    Api_websocket.create
      ~path:Path.Format.("api/pipeline/streams/applied" @/ empty)
      ~query:Query.[ "id", (module List(Stream.ID))
                   ; "input", (module List(Topology.Show_topo_input)) ]
      ids inputs

  let get_streams_with_source ?(inputs = []) ?(ids = []) () =
    Api_websocket.create
      ~path:Path.Format.("api/pipeline/streams/with_source" @/ empty)
      ~query:Query.[ "id", (module List(Stream.ID))
                   ; "input", (module List(Topology.Show_topo_input)) ]
      ids inputs

  let get_applied_with_source ?(inputs = []) ?(ids = []) () =
    Api_websocket.create
      ~path:Path.Format.("api/pipeline/streams/applied_with_source" @/ empty)
      ~query:Query.[ "id", (module List(Stream.ID))
                   ; "input", (module List(Topology.Show_topo_input)) ]
      ids inputs

end

let apply_streams s =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/pipeline/streams" @/ empty)
    ~query:Query.empty
    ~body:(Util_json.List.to_yojson Structure.to_yojson s)
    (fun _env res -> Lwt.return res)

let get_streams ?(inputs = []) ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/streams" @/ empty)
    ~query:Query.[ "id", (module List(Stream.ID))
                 ; "input", (module List(Topology.Show_topo_input)) ]
    ids inputs
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.List.of_yojson Structure.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_applied ?(inputs = []) ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/streams/applied" @/ empty)
    ~query:Query.[ "id", (module List(Stream.ID))
                 ; "input", (module List(Topology.Show_topo_input)) ]
    ids inputs
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.List.of_yojson Structure.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_streams_with_source ?(inputs = []) ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/streams/with_source" @/ empty)
    ~query:Query.[ "id", (module List(Stream.ID))
                 ; "input", (module List(Topology.Show_topo_input)) ]
    ids inputs
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.List.of_yojson Structure.packed_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_applied_with_source ?(inputs = []) ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/streams/applied_with_source" @/ empty)
    ~query:Query.[ "id", (module List(Stream.ID))
                 ; "input", (module List(Topology.Show_topo_input)) ]
    ids inputs
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.List.of_yojson Structure.packed_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
