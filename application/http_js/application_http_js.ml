open Application_types
open Netlib.Uri

module Body = struct
  open Js_of_ocaml

  include Application_types.Body
  let of_event evt = of_string @@ Js.to_string evt##.data

end

module Api_http = Api_js.Http.Make(Body)

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_topology sock =
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("application/topology" @/ empty)
      ~query:Query.empty
      Topology.of_yojson sock

  let get_streams sock =
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("application/stream-table" @/ empty)
      ~query:Query.empty
      Stream.stream_table_of_yojson sock

  let get_log ?(inputs = []) ?(streams = []) sock =
    let of_yojson = Util_json.List.of_yojson Stream.Log_message.of_yojson in
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("application/log" @/ empty)
      ~query:Query.[ "input", (module List(Topology.Show_topo_input))
                   ; "id", (module List(Stream.ID))]
      inputs streams of_yojson sock

end

let set_streams streams =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/application/stream_table" @/ empty)
    ~body:(Stream.stream_setting_to_yojson streams)
    ~query:Query.empty
    (fun _env res -> Lwt.return res)

let get_topology () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/application/topology" @/ empty)
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
    ~path:Path.Format.("api/application/stream-table" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Stream.stream_table_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_all_streams ~input () =
  Api_http.perform
    ~path:Path.Format.("api/application/streams" @/ empty)
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
    ~path:Path.Format.("api/application/stream-source" @/ empty)
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
    ~path:Path.Format.("api/application/log" @/ empty)
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

let set_user_password (pass : User.pass_change) =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("/api/user/password" @/ empty)
    ~query:Query.empty
    ~body:(User.pass_change_to_yojson pass)
    (fun _env x -> Lwt.return x)
