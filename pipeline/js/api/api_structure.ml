open Pipeline_types
open Netlib.Uri

module Api_websocket = Api_js.Websocket.Make(Body)

module Api_http = Api_js.Http.Make(Body)

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_streams ?f ?applied ?(inputs = []) ?(ids = []) () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("api/pipeline/streams" @/ empty)
        ~query:Query.[ "id", (module List(Application_types.Stream.ID))
                     ; "input", (module List(Application_types.Topology.Show_topo_input))
                     ; "applied", (module Option(Bool)) ]
        ids inputs applied () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Util_json.List.of_yojson Structure.of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe_map socket of_json @@ f socket;
      Lwt.return_ok socket

  let get_streams_with_source ?f ?applied ?(inputs = []) ?(ids = []) () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("api/pipeline/streams/with-source" @/ empty)
        ~query:Query.[ "id", (module List(Application_types.Stream.ID))
                     ; "input", (module List(Application_types.Topology.Show_topo_input))
                     ; "applied", (module Option(Bool)) ]
        ids inputs applied () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Util_json.List.of_yojson Structure.packed_of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe_map socket of_json @@ f socket;
      Lwt.return_ok socket

end

let apply_streams s =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/pipeline/streams" @/ empty)
    ~query:Query.empty
    ~body:(Util_json.List.to_yojson Structure.to_yojson s)
    (fun _env res -> Lwt.return res)

let get_streams ?applied ?(inputs = []) ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/streams" @/ empty)
    ~query:Query.[ "id", (module List(Application_types.Stream.ID))
                 ; "input", (module List(Application_types.Topology.Show_topo_input))
                 ; "applied", (module Option(Bool)) ]
    ids inputs applied
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.List.of_yojson Structure.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_streams_with_source ?applied ?(inputs = []) ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/streams/with-source" @/ empty)
    ~query:Query.[ "id", (module List(Application_types.Stream.ID))
                 ; "input", (module List(Application_types.Topology.Show_topo_input))
                 ; "applied", (module Option(Bool)) ]
    ids inputs applied
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.List.of_yojson Structure.packed_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
