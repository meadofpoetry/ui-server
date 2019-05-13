open Application_types
open Netlib.Uri

module Body = struct
  open Js_of_ocaml
  open Js_of_ocaml.WebSockets

  include Application_types.Body
  let of_event evt = of_string @@ Js.to_string evt##.data

end

module Api_websocket = Api_js.Websocket.Make(Body)

module Api_http = Api_js.Http.Make(Body)

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_topology ?f () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("ws/topology" @/ empty)
        ~query:Query.empty () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      Api_websocket.subscribe_map socket Topology.of_yojson @@ f socket;
      Lwt.return_ok socket

  let get_streams ?f () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("ws/topology/stream_table" @/ empty)
        ~query:Query.empty () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      Api_websocket.subscribe_map socket Stream.stream_table_of_yojson @@ f socket;
      Lwt.return_ok socket

  let get_log ?f ?(inputs = []) ?(streams = []) () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("ws/topology/log" @/ empty)
        ~query:Query.[ "input", (module List(Topology.Show_topo_input))
                     ; "id", (module List(Stream.ID))]
        inputs streams () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      let of_json = Util_json.List.of_yojson Stream.Log_message.of_yojson in
      Api_websocket.subscribe_map socket of_json @@ f socket;
      Lwt.return_ok socket

end

let set_streams streams =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/topology/stream_table" @/ empty)
    ~body:(Stream.stream_setting_to_yojson streams)
    ~query:Query.empty
    (fun _env res -> Lwt.return res)

let get_topology () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/topology" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Topology.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_streams () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/topology/stream_table" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Stream.stream_table_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_all_streams ~input () =
  Api_http.perform
    ~path:Path.Format.("api/topology/streams" @/ empty)
    ~query:Query.["input", (module Single(Topology.Show_topo_input))]
    input
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Stream.stream_list_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_stream_source ~stream_id () =
  Api_http.perform
    ~path:Path.Format.("api/topology/source" @/ empty)
    ~query:Query.["id", (module Single(Stream.ID))]
    stream_id
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Stream.source_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_log ?(boards = []) ?(cpu = [])?(inputs = []) ?(streams = [])
    ?limit ?from ?till ?duration  () =
  Api_http.perform
    ~path:Path.Format.("api/topology/log" @/ empty)
    ~query:Query.[ "board", (module List(Int))
                 ; "cpu", (module List(String))
                 ; "input", (module List(Topology.Show_topo_input))
                 ; "id", (module List(Stream.ID))
                 ; "limit", (module Option(Int))
                 ; "from", (module Option(Time_uri.Show))
                 ; "to", (module Option(Time_uri.Show))
                 ; "duration", (module Option(Time_uri.Show_relative)) ]
    boards cpu inputs streams limit from till duration
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         let of_json =
           Api.rows_of_yojson
             (Util_json.List.of_yojson Stream.Log_message.of_yojson)
             (fun _ -> Ok []) in
         match of_json x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
