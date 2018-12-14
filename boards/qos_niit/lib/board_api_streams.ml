open Containers
open Board_qos_types
open Board_api_common
open Api.Interaction.Json
open Common

let filter_streams (ids : Stream.ID.t list) x =
  match ids with
  | [] -> x
  | ids ->
     let eq = Stream.ID.equal in
     List.filter (fun (s, _) -> List.mem ~eq s ids) x

module WS = struct

  open Common.Stream
  open React

  let get id l =
    List.find_map (fun ((s : Stream.ID.t), x) ->
        if ID.equal s id then Some x else None) l

  let streams (events : events) (sources : init)
        (ids : ID.t list) (inputs : Topology.topo_input list)
        (incoming : bool option) _ body sock_data () =
    let filter_incoming = match incoming with
      | Some true -> Some (Board_protocol.is_incoming sources)
      | _ -> None in
    let filter_inputs = match inputs with
      | [] -> None
      | l ->
         let f s =
           match get_input s with
           | None -> false
           | Some i -> List.mem ~eq:Topology.equal_topo_input i l in
         Option.return f in
    let filter_ids = match ids with
      | [] -> None
      | l -> let f (s : Stream.t) = List.mem ~eq:ID.equal s.id l in
             Option.return f in
    let filter = List.keep_some [filter_incoming; filter_inputs; filter_ids] in
    let rec apply fns s = match fns with
      | [] -> true
      | f :: tl -> if f s then apply tl s else false in
    let e = match filter with
      | [] -> S.changes events.streams
      | fns ->
         E.changes ~eq:(Equal.list equal)
         @@ E.map (fun streams ->
                List.filter (apply fns) streams)
              (S.changes events.streams)
    in Api.Socket.handler socket_table sock_data e
         (Json.List.to_yojson Stream.to_yojson) body

  let bitrate (events : events) ids _ body sock_data () =
    let e = E.map (filter_streams ids) events.ts.bitrates in
    Api.Socket.handler socket_table sock_data e bitrates_to_yojson body

  let ts_info (events : events) ids _ body sock_data () =
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.info in
    Api.Socket.handler socket_table sock_data e ts_info_to_yojson body

  let services (events : events) ids _ body sock_data () =
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.services in
    Api.Socket.handler socket_table sock_data e services_to_yojson body

  let tables (events : events) ids _ body sock_data () =
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.tables in
    Api.Socket.handler socket_table sock_data e tables_to_yojson body

  let sections (events : events) ids _ body sock_data () =
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.sections in
    Api.Socket.handler socket_table sock_data e sections_to_yojson body

  let pids (events : events) ids _ body sock_data () =
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.pids in
    Api.Socket.handler socket_table sock_data e pids_to_yojson body

  let t2mi_info (events : events) ids t2mi_stream_ids _ body sock_data () =
    let open T2mi_info in
    let filter_t2mi_ids x = match t2mi_stream_ids with
      | [] -> x
      | l ->
         List.filter_map (fun (id, (x : t list timestamped)) ->
             List.filter (fun (sid, _) -> List.mem ~eq:equal_id sid l) x.data
             |> function [] -> None | l -> Some (id, { x with data = l })) x in
    let e =
      React.E.map (filter_t2mi_ids % filter_streams ids)
      @@ React.S.changes events.t2mi.structures in
    Api.Socket.handler socket_table sock_data e t2mi_info_to_yojson body

  let errors (events : events) ids errors priority pids _ body sock_data () =
    let open Error in
    let eq = ( = ) in
    let f_streams vals x = match vals with
      | [] -> x
      | vals ->
         List.filter (fun (id, _) ->
             List.mem ~eq:Stream.ID.equal id vals) x in
    let f_errors vals x = match vals with
      | [] -> x
      | vals ->
         List.filter_map (fun (id, (e : t_ext list)) ->
             List.filter (fun (x : t_ext) ->
                 List.mem ~eq x.err_code vals) e
             |> function [] -> None | x -> Some (id, x)) x in
    let f_priority vals x = match vals with
      | [] -> x
      | vals ->
         List.filter_map (fun (id, (e : t_ext list)) ->
             List.filter (fun (x : t_ext) ->
                 List.mem ~eq x.priority vals) e
             |> function [] -> None | x -> Some (id, x)) x in
    let f_pids vals x = match vals with
      | [] -> x
      | vals ->
         List.filter_map (fun (id, (e : t_ext list)) ->
             List.filter (fun (x : t_ext) ->
                 List.mem ~eq (fst x.pid) vals) e
             |> function [] -> None | x -> Some (id, x)) x in
    let filter =
      f_pids pids
      % f_priority priority
      % f_errors errors
      % f_streams ids in
    let to_yojson =
      Json.(List.to_yojson
            @@ Pair.to_yojson
                 Stream.ID.to_yojson
                 (List.to_yojson Error.t_ext_to_yojson)) in
    let e =
      React.E.fmap (fun l ->
          match filter l with
          | [] -> None
          | l  -> Some l) events.ts.errors in
    Api.Socket.handler socket_table sock_data e to_yojson body

end

module HTTP = struct

  let streams (api : api) ids inputs incoming _ _ () =
    api.get_streams ?incoming ~ids ~inputs ()
    >|= (Result.return % (Json.List.to_yojson Stream.to_yojson))
    >>= respond_result

  let ts_info (api : api) ids _ _ () =
    api.get_ts_info ()
    >|= filter_streams ids
    >|= (Result.return % ts_info_to_yojson)
    >>= respond_result

  let pids (api : api) ids _ _ () =
    api.get_pids ()
    >|= filter_streams ids
    >|= (Result.return % pids_to_yojson)
    >>= respond_result

  let services (api : api) ids _ _ () =
    api.get_services ()
    >|= (filter_streams ids)
    >|= (Result.return % services_to_yojson)
    >>= respond_result

  let tables (api : api) ids _ _ () =
    api.get_tables ()
    >|= (filter_streams ids)
    >|= (Result.return % tables_to_yojson)
    >>= respond_result

  let sections (api : api) ids _ _ () =
    api.get_sections ()
    >|= (filter_streams ids)
    >|= (Result.return % sections_to_yojson)
    >>= respond_result

  let t2mi_info (api : api) ids _ _ () =
    api.get_t2mi_info ()
    >|= filter_streams ids
    >|= (Result.return % t2mi_info_to_yojson)
    >>= respond_result

  let si_psi_section (api : api) id table_id section
        table_id_ext id_ext_1 id_ext_2 _ _ () =
    let open SI_PSI_section.Dump in
    api.get_section ?section ?table_id_ext ?id_ext_1 ?id_ext_2
      ~id ~table_id ()
    >|= (function
         | Ok x -> Ok ((timestamped_to_yojson to_yojson) x)
         | Error e -> Error (error_to_yojson e))
    >>= respond_result

  let t2mi_sequence (api : api) id stream_ids
        (duration : Time.Relative.t option) _ _ () =
    let seconds =
      Option.flat_map Time.Relative.to_int_s duration
      |> Option.get_or ~default:5 in
    api.get_t2mi_seq { stream = id; seconds }
    >|= (fun (x : T2mi_sequence.t timestamped) ->
      List.filter (fun (x : T2mi_sequence.item) ->
          match stream_ids with
          | [] -> true
          | l -> List.mem ~eq:(=) x.stream_id l) x.data
      |> (fun items -> { x with data = items })
      |> (timestamped_to_yojson T2mi_sequence.to_yojson)
      |> Result.return)
    >>= respond_result

end

let handler (sources : init) (api : api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "streams"
    [ create_ws_handler ~docstring:"Pushes available streams to the client"
        ~path:Path.Format.empty
        ~query:Query.[ "id", (module List(Stream.ID))
                     ; "input", (module List(Topology.Show_topo_input))
                     ; "incoming", (module Option(Bool)) ]
        (WS.streams events sources)
    ; create_ws_handler ~docstring:"Pushes stream bitrate to the client"
        ~path:Path.Format.("bitrate" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        (WS.bitrate events)
    ; create_ws_handler ~docstring:"Pushes TS info to the client"
        ~path:Path.Format.("ts-info" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        (WS.ts_info events)
    ; create_ws_handler ~docstring:"Pushes TS services to the client"
        ~path:Path.Format.("services" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        (WS.services events)
    ; create_ws_handler ~docstring:"Pushes TS SI/PSI tables to the client"
        ~path:Path.Format.("tables" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        (WS.tables events)
    ; create_ws_handler ~docstring:"Pushes TS SI/PSI sections to the client"
        ~path:Path.Format.("sections" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        (WS.sections events)
    ; create_ws_handler ~docstring:"Pushes TS PIDs to the client"
        ~path:Path.Format.("pids" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        (WS.pids events)
    ; create_ws_handler ~docstring:"Pushes T2-MI info to the client"
        ~path:Path.Format.("t2mi-info" @/ empty)
        ~query:Query.[ "id", (module List(Stream.ID))
                     ; "t2mi-stream-id", (module List(Int)) ]
        (WS.t2mi_info events)
    ; create_ws_handler ~docstring:"Pushes TS errors to the client"
        ~path:Path.Format.("errors" @/ empty)
        ~query:Query.[ "ids", (module List(Stream.ID))
                     ; "errors", (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid", (module List(Int))]
        (WS.errors events)
    ]
    [ `GET,
      [ create_handler ~docstring:"Returns list of streams"
          ~path:Path.Format.empty
          ~query:Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))
                       ; "incoming", (module Option(Bool))]
          (HTTP.streams api)
      (* Dump *)
      ; create_handler ~docstring:"Returns SI/PSI table section"
          ~path:Path.Format.("dump/si-psi-section"
                             @/ Stream.ID.fmt ^/ Int ^/ empty)
          ~query:Query.[ "section", (module Option(Int))
                       ; "table-id-ext", (module Option(Int))
                       ; "id-ext-1", (module Option(Int))
                       ; "id-ext-2", (module Option(Int)) ]
          (HTTP.si_psi_section api)
      ; create_handler ~docstring:"Returns T2-MI packet sequence"
          ~path:Path.Format.(Stream.ID.fmt ^/ "dump/t2mi-sequence" @/ empty)
          ~query:Query.[ "t2mi-stream-id", (module List(Int))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.t2mi_sequence api)
      (* TS structure components *)
      ; create_handler ~docstring:"Returns TS info"
          ~path:Path.Format.("ts-info" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.ts_info api)
      ; create_handler ~docstring:"Returns TS services"
          ~path:Path.Format.("services" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.services api)
      ; create_handler ~docstring:"Returns TS SI/PSI tables"
          ~path:Path.Format.("tables" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.tables api)
      ; create_handler ~docstring:"Returns TS SI/PSI sections"
          ~path:Path.Format.("sections" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.sections api)
      ; create_handler ~docstring:"Returns TS PIDs"
          ~path:Path.Format.("pids" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.pids api)
      (* T2-MI structure *)
      ; create_handler ~docstring:"Returns T2-MI structure"
          ~path:Path.Format.("t2mi-info" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.t2mi_info api)
      ]
    ]
