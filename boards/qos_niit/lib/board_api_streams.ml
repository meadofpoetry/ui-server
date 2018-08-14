open Containers
open Board_types
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Common
open Types

module WS = struct

  open Board_types.Streams.TS
  open React

  let streams (events:events) ids inputs _ body sock_data () =
    let rec input_of_stream (t:Stream.t) = match t.source with
      | Input x  -> x
      | Parent t -> input_of_stream t in
    let ids = List.map Stream.id_of_int32 ids in
    let e = match ids with
      | [] -> S.changes events.streams
      | l  ->
         E.fmap (fun streams ->
             List.filter (fun (s:Stream.t) ->
                 let mem = List.mem ~eq:Topology.equal_topo_input
                             (input_of_stream s) inputs in
                 match s.id, mem with
                 | `Ts id, true -> List.mem ~eq:(Stream.equal_id) id l
                 | _ -> false) streams
             |> function [] -> None | l -> Some l)
           (S.changes events.streams)
    in Api.Socket.handler socket_table sock_data e
         (Json.List.to_yojson Stream.to_yojson) body

  let bitrate (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let e  = E.fmap (List.Assoc.get ~eq:Stream.equal_id id)
               events.ts.bitrates in
    Api.Socket.handler socket_table sock_data e
      bitrate_to_yojson body

  let info (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let eq = Stream.equal_id in
    let e  = E.fmap (List.Assoc.get ~eq id) events.ts.info in
    Api.Socket.handler socket_table sock_data e info_to_yojson body

  let services (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let eq = Stream.equal_id in
    let e  = E.fmap (List.Assoc.get ~eq id) events.ts.services in
    Api.Socket.handler socket_table sock_data e services_to_yojson body

  let tables (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let eq = Stream.equal_id in
    let e  = E.fmap (List.Assoc.get ~eq id) events.ts.tables in
    Api.Socket.handler socket_table sock_data e tables_to_yojson body

  let pids (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let eq = Stream.equal_id in
    let e  = E.fmap (List.Assoc.get ~eq id) events.ts.pids in
    Api.Socket.handler socket_table sock_data e pids_to_yojson body

  module T2MI = struct

    open Board_types.Streams.T2MI

    let structure (events:events) id stream_ids _ body sock_data () =
      let id = Stream.id_of_int32 id in
      let eq = Stream.equal_id in
      let e  = React.E.fmap (List.Assoc.get ~eq id) events.t2mi.structures in
      Api.Socket.handler socket_table sock_data e structure_to_yojson body

  end

  module Errors = struct

    open Errors

    let rec filter (acc:t list) = function
      | [ ]     -> acc
      | f :: tl -> filter (f acc) tl

    let flst fltr f = match fltr with [] -> None | l -> Some (f l)

    let errors (events:events) id errors priority pids _ body sock_data () =
      let id = Stream.id_of_int32 id in
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
            match List.fold_left (fun acc (x, errs) ->
                      if not @@ Stream.equal_id x id
                      then acc
                      else acc @ filter errs fns) [] l with
            | [] -> None
            | l  -> Some l) events.ts.errors
      in Api.Socket.handler socket_table sock_data e
           (Json.List.to_yojson to_yojson) body

  end

end

module HTTP = struct

  open Board_types.Streams.TS

  (* merge ordered descending lists of streams and board states *)
  let merge_streams_state
        (streams : (Common.Stream.t list * Time.t * Time.t) list)
        (state : (Common.Topology.state * Time.t * Time.t) list) =
    (* TODO consider ord checks *)
    let (<=) l r = Time.compare l r <= 0 in
    let (>=) l r = Time.compare l r >= 0 in
    let join streams states =
      List.fold_left (fun acc (s,f,t) ->
          List.append acc
          @@ List.filter_map
               (function (`Fine,ff,tt) ->
                          if f <= ff && t >= tt then Some (s,ff,tt)
                          else if f <=ff && t >= ff then Some (s, ff, t)
                          else if f <= tt && t >= tt then Some (s, f, tt)
                          else if f >= ff && t <= tt then Some (s, f, t)
                          else None
                       | _ -> None) states) [] streams
    in (* TODO add compress *)
    join streams state

  let streams_unique db (events:events) inputs from till duration () =
    let open Api.Api_types in
    let open Common.Stream in
    let open Lwt_result.Infix in
    let merge (cur:t list) streams =
      let filter (s, id, typ, t) =
        let id = `Ts (id_of_int32 id) in
        if List.exists (fun s -> equal_stream_id id s.id
                                 && equal_typ typ s.typ) cur
        then None
        else Some (s,`Last t)
      in
      let streams = List.filter_map filter streams in
      (List.map (fun s -> s,`Now) cur) @ streams
    in
    match Time.make_interval ?from ?till ?duration () with
    | Ok `Range (from,till) ->
       Db.Streams.select_stream_unique db ~inputs ~from ~till ()
       >>= (fun (Compressed { data }) ->
        let current =
          React.S.value events.streams
          |> List.filter (fun (s:Stream.t) ->
                 let input = Stream.get_input s in
                 List.mem ~eq:Topology.equal_topo_input input inputs) in
        Lwt_result.return (Compressed { data = merge current data }))
       |> Lwt_result.map (fun x ->
              rows_to_yojson
                (fun () -> `Null)
                streams_unique_to_yojson x)
       |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
       |> fun t -> Lwt.bind t respond_result
    | _ -> respond_error ~status:`Not_implemented "FIXME" ()

  let streams_states db ids inputs limit from till duration () =
    let open Api.Api_types in
    match Time.make_interval ?from ?till ?duration () with
    | Ok `Range (from,till) ->
       (* TODO make it more sound *)
       Db.Streams.select_streams ?limit ~ids ~inputs ~from ~till db
       |> Lwt_result.map (fun x ->
              rows_to_yojson
                streams_states_to_yojson
                (fun () -> `Null) x)
       |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
       >>= respond_result
    | _ -> respond_error ~status:`Not_implemented "FIXME" ()

  let streams db events ids inputs limit compress from till duration _ _ () =
    if Option.get_or ~default:false compress
    then streams_unique db events inputs from till duration ()
    else streams_states db ids inputs limit from till duration ()

  let si_psi_section (api:api) id table_id section
        table_id_ext eit_ts_id eit_orig_nw_id _ _ () =
    let stream_id = Stream.id_of_int32 id in
    let req = { stream_id
              ; table_id
              ; section
              ; table_id_ext
              ; eit_ts_id
              ; eit_orig_nw_id } in
    api.get_section req
    >|= (function
         | Ok x    -> Ok    (section_to_yojson x)
         | Error e -> Error (section_error_to_yojson e))
    >>= respond_result

  let to_yojson _to =
    Json.(List.to_yojson (Pair.to_yojson Stream.id_to_yojson _to))

  let get' (db:Db.t) select _to from till duration () =
    match Time.make_interval ?from ?till ?duration () with
    | Ok `Range (from, till) ->
       select from till
       |> Lwt_result.map (fun d ->
              Api.Api_types.rows_to_yojson
                (to_yojson _to)
                (fun () -> `Null) d)
       |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
       >>= respond_result
    | _ -> respond_error ~status:`Not_implemented "FIXME" ()

  let info db id limit from till duration _ _ () =
    let select from till =
      Db.Streams.select_ts_info
        ~with_pre:false ?limit ~ids:[id] ~from ~till db in
    get' db select info_to_yojson from till duration ()

  let services db id limit from till duration _ _ () =
    let select from till =
      Db.Streams.select_services
        ~with_pre:false ?limit ~ids:[id] ~from ~till db in
    get' db select services_to_yojson from till duration ()

  let tables db id limit from till duration _ _ () =
    let select from till =
      Db.Streams.select_tables
        ~with_pre:false ?limit ~ids:[id] ~from ~till db in
    get' db select tables_to_yojson from till duration ()

  let pids db id limit from till duration _ _ () =
    let select from till =
      Db.Streams.select_pids
        ~with_pre:false ?limit ~ids:[id] ~from ~till db in
    get' db select pids_to_yojson from till duration ()

  let bitrate id limit compress from till duration _ _ () =
    respond_error ~status:`Not_implemented "FIXME" ()

  module T2MI = struct

    open Board_types.Streams.T2MI

    let sequence (api:api) id stream_ids (duration:Time.Relative.t option) _ _ () =
      let seconds =
        Option.flat_map Time.Relative.to_int_s duration
        |> Option.get_or ~default:5 in
      api.get_t2mi_seq { stream = Stream.id_of_int32 id; seconds }
      >|= (fun x -> List.filter (fun (x:sequence_item) ->
                        match stream_ids with
                        | [] -> true
                        | l  -> List.mem ~eq:(=) x.stream_id l) x
                    |> sequence_to_yojson
                    |> Result.return)
      >>= respond_result

    let structure db id limit from till duration _ _ () =
      let select from till =
        Db.Streams.select_t2mi_info
          ~with_pre:true ?limit ~ids:[id] ~from ~till db in
      get' db select structure_to_yojson from till duration ()

  end

  module Errors = struct

    open Errors

    let errors db streams errors priority pids
          limit compress from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok (`Range (from,till)) ->
         (match compress with
          | Some true ->
             (Db.Errors.select_errors_compressed
                db ~is_ts:true ~streams ~priority
                ~errors ~pids ~from ~till ())
          | _ -> Db.Errors.select_errors db ~is_ts:true ~streams
                   ~priority ~errors ~pids ?limit ~from ~till ())
         >>= fun v ->
         let r = Ok Db.Errors.(Api.Api_types.rows_to_yojson
                                 raw_to_yojson compressed_to_yojson v) in
         respond_result r
      | _ -> respond_error ~status:`Not_implemented "not implemented" ()

    let percent db streams errors priority pids from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok (`Range (from,till)) ->
         Db.Errors.select_percent db ~is_ts:true ~streams ~priority
           ~errors ~pids ~from ~till ()
         >>= fun v -> respond_result (Ok (`Float v))
      | _ -> respond_error ~status:`Not_implemented "not implemented" ()

    let has_any db streams errors priority pids from till duration _ _() =
      match Time.make_interval ?from ?till ?duration () with
      | Ok (`Range (from,till)) ->
         Db.Errors.select_has_any db ~is_ts:true ~streams ~priority
           ~errors ~pids ~from ~till ()
         >>= fun v -> respond_result (Ok (`Bool v))
      | _ -> respond_error ~status:`Not_implemented "not implemented" ()

  end

end

let handler db (api:api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "streams"
    [ create_ws_handler ~docstring:"Pushes available streams to the client"
        ~path:Path.Format.empty
        ~query:Query.[ "id",    (module List(Int32))
                     ; "input", (module List(Topology.Show_topo_input)) ]
        (WS.streams events)
    ; create_ws_handler ~docstring:"Pushes stream bitrate to the client"
        ~path:Path.Format.(Int32 ^/ "bitrate" @/ empty)
        ~query:Query.empty
        (WS.bitrate events)
    ; create_ws_handler ~docstring:"Pushes TS info to the client"
        ~path:Path.Format.(Int32 ^/ "info" @/ empty)
        ~query:Query.empty
        (WS.info events)
    ; create_ws_handler ~docstring:"Pushes TS services to the client"
        ~path:Path.Format.(Int32 ^/ "services" @/ empty)
        ~query:Query.empty
        (WS.services events)
    ; create_ws_handler ~docstring:"Pushes TS SI/PSI tables to the client"
        ~path:Path.Format.(Int32 ^/ "tables" @/ empty)
        ~query:Query.empty
        (WS.tables events)
    ; create_ws_handler ~docstring:"Pushes TS PIDs to the client"
        ~path:Path.Format.(Int32 ^/ "pids" @/ empty)
        ~query:Query.empty
        (WS.pids events)
    ; create_ws_handler ~docstring:"Pushes T2-MI structure to the client"
        ~path:Path.Format.(Int32 ^/ "t2mi/structure" @/ empty)
        ~query:Query.[ "t2mi-stream-id", (module List(Int)) ]
        (WS.T2MI.structure events)
    ; create_ws_handler ~docstring:"Pushes TS errors to the client"
        ~path:Path.Format.(Int32 ^/ "errors" @/ empty)
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))]
        (WS.Errors.errors events)
    ]
    [ `GET,
      [ create_handler ~docstring:"Returns streams states"
          ~path:Path.Format.empty
          ~query:Query.[ "id",       (module List(Int32))
                       ; "input",    (module List(Topology.Show_topo_input))
                       ; "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.streams db events)
      ; create_handler ~docstring:"Returns SI/PSI table section"
          ~path:Path.Format.(Int32 ^/ "section" @/ Int ^/ empty)
          ~query:Query.[ "section",        (module Option(Int))
                       ; "table-id-ext",   (module Option(Int))
                       ; "eit-ts-id",      (module Option(Int))
                       ; "eit-orig-nw-id", (module Option(Int)) ]
          (HTTP.si_psi_section api)
      ; create_handler ~docstring:"Returns TS bitrate"
          ~path:Path.Format.(Int32 ^/ "bitrate" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          HTTP.bitrate
      ; create_handler ~docstring:"Returns TS info"
          ~path:Path.Format.(Int32 ^/ "info" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.info db)
      ; create_handler ~docstring:"Returns TS services"
          ~path:Path.Format.(Int32 ^/ "services" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.services db)
      ; create_handler ~docstring:"Returns TS tables"
          ~path:Path.Format.(Int32 ^/ "tables" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.tables db)
      ; create_handler ~docstring:"Returns TS PIDs"
          ~path:Path.Format.(Int32 ^/ "pids" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.pids db)
      (* T2-MI*)
      ; create_handler ~docstring:"Returns T2-MI packet sequence"
          ~path:Path.Format.(Int32 ^/ "t2mi/sequence" @/ empty)
          ~query:Query.[ "t2mi-stream-id", (module List(Int))
                       ; "duration",       (module Option(Time.Relative)) ]
          (HTTP.T2MI.sequence api)
      ; create_handler ~docstring:"Returns T2-MI structure"
          ~path:Path.Format.(Int32 ^/ "t2mi/structure" @/ empty)
          ~query:Query.[ "limit",          (module Option(Int))
                       ; "from",           (module Option(Time.Show))
                       ; "to",             (module Option(Time.Show))
                       ; "duration",       (module Option(Time.Relative)) ]
          (HTTP.T2MI.structure db)
      (* Errors *)
      ; create_handler ~docstring:"Returns archived TS errors"
          ~path:Path.Format.(Int32 ^/ "errors" @/ empty)
          ~query:Query.[ "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))

                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (fun x -> HTTP.Errors.errors db [x])
      ; create_handler ~docstring:"Returns TS errors presence percentage"
          ~path:Path.Format.(Int32 ^/ "errors/percent" @/ empty)
          ~query:Query.[ "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (fun x -> HTTP.Errors.percent db [x])
      ; create_handler ~docstring:"Returns if TS errors were present for the requested period"
          ~path:Path.Format.(Int32 ^/ "errors/has-any" @/ empty)
          ~query:Query.[ "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (fun x -> HTTP.Errors.has_any db [x])
      ]
    ]
