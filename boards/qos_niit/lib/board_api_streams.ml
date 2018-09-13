open Containers
open Board_types
open Board_api_common
open Api.Interaction.Json
open Common
open Types

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

  let streams (events : events) (sources : Types.init)
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
         E.fmap (fun streams ->
             List.filter (apply fns) streams
             |> function [] -> None | l -> Some l)
           (S.changes events.streams)
    in Api.Socket.handler socket_table sock_data e
         (Json.List.to_yojson Stream.to_yojson) body

  let bitrate (events : events) ids _ body sock_data () =
    let to_yojson =
      stream_assoc_to_yojson
      @@ timestamped_to_yojson Bitrate.to_yojson in
    let e = E.map (filter_streams ids) events.ts.bitrates in
    Api.Socket.handler socket_table sock_data e to_yojson body

  let ts_info (events : events) ids _ body sock_data () =
    let to_yojson =
      stream_assoc_to_yojson
      @@ timestamped_to_yojson Ts_info.to_yojson in
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.info in
    Api.Socket.handler socket_table sock_data e to_yojson body

  let services (events : events) ids _ body sock_data () =
    let to_yojson =
      stream_assoc_to_yojson
      @@ timestamped_to_yojson
      @@ Json.List.to_yojson Service.to_yojson in
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.services in
    Api.Socket.handler socket_table sock_data e to_yojson body

  let tables (events : events) ids _ body sock_data () =
    let to_yojson =
      stream_assoc_to_yojson
      @@ timestamped_to_yojson
      @@ Json.List.to_yojson SI_PSI_table.to_yojson in
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.tables in
    Api.Socket.handler socket_table sock_data e to_yojson body

  let pids (events : events) ids _ body sock_data () =
    let to_yojson =
      stream_assoc_to_yojson
      @@ timestamped_to_yojson
      @@ Json.List.to_yojson Pid.to_yojson in
    let e =
      E.map (filter_streams ids)
      @@ React.S.changes events.ts.pids in
    Api.Socket.handler socket_table sock_data e to_yojson body

  let t2mi_structure (events : events) ids _ _ body sock_data () =
    let to_yojson =
      stream_assoc_to_yojson
      @@ timestamped_to_yojson
      @@ Json.List.to_yojson T2mi_info.to_yojson in
    let e =
      React.E.map (filter_streams ids)
      @@ React.S.changes events.t2mi.structures in
    Api.Socket.handler socket_table sock_data e to_yojson body

  let rec filter (acc : Error.t list) = function
    | [] -> acc
    | f :: tl -> filter (f acc) tl

  let flst fltr f = match fltr with [] -> None | l -> Some (f l)

  let errors (events : events) id errors priority pids _ body sock_data () =
    let open Error in
    let eq = ( = ) in
    let f_errors =
      flst errors (fun l e ->
          List.filter (fun x -> List.mem ~eq x.err_code l) e) in
    let f_prior =
      flst priority (fun l e ->
          List.filter (fun x -> List.mem ~eq x.priority l) e) in
    let f_pids =
      flst pids (fun l e ->
          List.filter (fun x -> List.mem ~eq x.pid l) e) in
    let fns = List.filter_map (fun x -> x) [f_errors; f_prior; f_pids] in
    let e =
      React.E.fmap (fun l ->
          match List.fold_left (fun acc ((x : Stream.ID.t), errs) ->
                    if not (ID.equal x id)
                    then acc
                    else acc @ filter errs fns) [] l with
          | [] -> None
          | l  -> Some l) events.ts.errors
    in Api.Socket.handler socket_table sock_data e
         (Json.List.to_yojson to_yojson) body

end

module HTTP = struct

  let si_psi_section (api : api) id table_id section
        table_id_ext ext_info_1 ext_info_2 _ _ () =
    api.get_section ?section ?table_id_ext ?ext_info_1 ?ext_info_2
      ~id ~table_id ()
    >|= (function
         | Ok x -> Ok ((timestamped_to_yojson SI_PSI_section.to_yojson) x)
         | Error e -> Error (SI_PSI_section.dump_error_to_yojson e))
    >>= respond_result

  let ts_info (api : api) ids _ _ () =
    let to_yojson = timestamped_to_yojson Ts_info.to_yojson in
    api.get_ts_info ()
    >|= filter_streams ids
    >|= (Result.return % stream_assoc_to_yojson to_yojson)
    >>= respond_result

  let pids (api : api) ids _ _ () =
    let to_yojson : Pid.t list timestamped -> Yojson.Safe.json =
      timestamped_to_yojson
      @@ Json.List.to_yojson Pid.to_yojson in
    api.get_pids ()
    >|= filter_streams ids
    >|= (Result.return % stream_assoc_to_yojson to_yojson)
    >>= respond_result

  let services (api : api) ids _ _ () =
    let to_yojson : Service.t list timestamped -> Yojson.Safe.json =
      timestamped_to_yojson
      @@ Json.List.to_yojson Service.to_yojson in
    api.get_services ()
    >|= (filter_streams ids)
    >|= (Result.return % stream_assoc_to_yojson to_yojson)
    >>= respond_result

  let tables (api : api) ids _ _ () =
    let to_yojson : SI_PSI_table.t list timestamped -> Yojson.Safe.json =
      timestamped_to_yojson
      @@ Json.List.to_yojson SI_PSI_table.to_yojson in
    api.get_tables ()
    >|= (filter_streams ids)
    >|= (Result.return % stream_assoc_to_yojson to_yojson)
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

  let t2mi_info (api : api) ids _ _ () =
    let to_yojson : T2mi_info.t list timestamped -> Yojson.Safe.json =
      timestamped_to_yojson
      @@ Json.List.to_yojson T2mi_info.to_yojson in
    api.get_t2mi_info ()
    >|= filter_streams ids
    >|= (Result.return % stream_assoc_to_yojson to_yojson)
    >>= respond_result

end

let handler (sources : Types.init) (api : api) events =
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
        ~path:Path.Format.("info" @/ empty)
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
    ; create_ws_handler ~docstring:"Pushes TS PIDs to the client"
        ~path:Path.Format.("pids" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        (WS.pids events)
    ; create_ws_handler ~docstring:"Pushes T2-MI structure to the client"
        ~path:Path.Format.("t2mi/structure" @/ empty)
        ~query:Query.[ "id", (module List(Stream.ID))
                     ; "t2mi-stream-id", (module List(Int)) ]
        (WS.t2mi_structure events)
    ; create_ws_handler ~docstring:"Pushes TS errors to the client"
        ~path:Path.Format.(Stream.ID.fmt ^/ "errors" @/ empty)
        ~query:Query.[ "errors", (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid", (module List(Int))]
        (WS.errors events)
    ]
    [ `GET,
      [ create_handler ~docstring:"Returns SI/PSI table section"
          ~path:Path.Format.(Stream.ID.fmt ^/ "section" @/ Int ^/ empty)
          ~query:Query.[ "section", (module Option(Int))
                       ; "table-id-ext", (module Option(Int))
                       ; "ext-info-1", (module Option(Int))
                       ; "ext-info-2", (module Option(Int)) ]
          (HTTP.si_psi_section api)
      ; create_handler ~docstring:"Returns TS info"
          ~path:Path.Format.("ts-info" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.ts_info api)
      ; create_handler ~docstring:"Returns TS services"
          ~path:Path.Format.("services" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.services api)
      ; create_handler ~docstring:"Returns TS tables"
          ~path:Path.Format.("tables" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.tables api)
      ; create_handler ~docstring:"Returns TS PIDs"
          ~path:Path.Format.("pids" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.pids api)
      (* T2-MI*)
      ; create_handler ~docstring:"Returns T2-MI packet sequence"
          ~path:Path.Format.(Stream.ID.fmt ^/ "t2mi/sequence" @/ empty)
          ~query:Query.[ "t2mi-stream-id", (module List(Int))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.t2mi_sequence api)
      ; create_handler ~docstring:"Returns T2-MI structure"
          ~path:Path.Format.("t2mi/info" @/ empty)
          ~query:Query.["id", (module List(Stream.ID))]
          (HTTP.t2mi_info api)
      ]
    ]
