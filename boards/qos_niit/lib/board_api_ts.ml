open Containers
open Board_types
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Common
open Types

module WS = struct

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

    let rec filter (acc:Stream.id * t list) = function
      | []    -> true
      | f::tl -> (match f acc with
                  | _,[] -> false
                  | acc  -> filter acc tl)

    let to_yojson = Json.(Pair.to_yojson Stream.id_to_yojson (List.to_yojson to_yojson))

    let flst fltr f = match fltr with
      | [] -> None | l  -> Some (f l)

    let errors (events:events) ids errors priority pids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let eq  = ( = ) in
      let f_stream =
        flst ids (fun l (s,e) ->
            if List.mem ~eq:Stream.equal_id s l then s,e else s,[]) in
      let f_errors =
        flst errors (fun l (s,e) ->
            s, List.filter (fun x -> List.mem ~eq x.err_code l) e) in
      let f_prior =
        flst priority (fun l (s,e) ->
            s, List.filter (fun x -> List.mem ~eq x.priority l) e) in
      let f_pids =
        flst pids (fun l (s,e) ->
            s, List.filter (fun x -> List.mem ~eq x.pid l) e) in
      let fns = List.filter_map (fun x -> x)
                  [f_stream;f_errors;f_prior;f_pids] in
      let e   = React.E.fmap (fun l ->
                    match List.filter (fun errs -> filter errs fns) l with
                    | [] -> None
                    | l  -> Some l) events.ts.errors
      in Api.Socket.handler socket_table sock_data e
           (Json.List.to_yojson to_yojson) body

  end

  open Board_types.Streams.TS

  let to_yojson f v = Json.(List.to_yojson
                              (Pair.to_yojson Stream.id_to_yojson f) v)


  let streams (events:events) ids _ body sock_data () =
    let ids = List.map Stream.id_of_int32 ids in
    let e = match ids with
      | [] -> React.S.changes events.streams
      | l  ->
         React.E.fmap (fun streams ->
             List.filter (fun (s:Stream.t) ->
                 match s.id with
                 | `Ts id -> List.mem ~eq:(Stream.equal_id) id l
                 | _      -> false) streams
             |> function [] -> None | l -> Some l) (React.S.changes events.streams)
    in Api.Socket.handler socket_table sock_data e
         (Json.List.to_yojson Stream.to_yojson) body

  let bitrate (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let e  = React.E.fmap (List.Assoc.get ~eq:Stream.equal_id id)
               events.ts.bitrates in
    Api.Socket.handler socket_table sock_data e
      bitrate_to_yojson body

  let structure (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let e  = React.E.fmap (List.Assoc.get ~eq:Stream.equal_id id) events.ts.structures in
    Api.Socket.handler socket_table sock_data e structure_to_yojson body

end

module HTTP = struct

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

  end

  open Board_types.Streams.TS

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
    >|= (function Ok x    -> Ok    (section_to_yojson x)
                | Error e -> Error (section_error_to_yojson e))
    >>= respond_result

  let streams (events:events) ids _ _ () =
    let ids = List.map Stream.id_of_int32 ids in
    let streams = match ids with
      | [] -> React.S.value events.streams
      | l  -> List.filter (fun (s:Stream.t) ->
                  match s.id with
                  | `Ts id -> List.mem ~eq:Stream.equal_id id l
                  | _      -> false)
              @@ React.S.value events.streams
    in (Json.List.to_yojson Stream.to_yojson) streams
       |> Result.return |> respond_result

  module Archive = struct

    module T2MI = struct

      open Board_types.Streams.T2MI

      type struct_t2 = (Stream.id * Board_types.Streams.T2MI.structure * Time.t)
                         list [@@deriving yojson]

      let structure db id ids limit from till duration _ _ () =
        match Time.make_interval ?from ?till ?duration () with
        | Ok `Range (from,till) ->
           Db.Streams.select_structs_t2 db ~with_pre:true
             ?limit ~ids:[id] ~from ~till
           |> Lwt_result.map (fun d ->
                  Api.Api_types.rows_to_yojson struct_t2_to_yojson
                    (fun () -> `Null) d)
           |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
           >>= fun x -> respond_result x
        | _ -> respond_error ~status:`Not_implemented "FIXME" ()

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

    (* merge ordered descending lists of streams and board states *)
    let merge_streams_state
          (streams : (Common.Stream.t list * Time.t) list)
          (state : (Common.Topology.state * Time.t * Time.t) list) =
      (* TODO consider ord checks *)
      let pair_streams streams =
        let rec pair' acc = function
          | (sx,tx)::(sy,ty)::tl -> pair' ((sy, ty, tx)::acc) ((sy,ty)::tl)
          | [_] | [] -> List.rev acc
        in
        let (s,t) = List.hd streams in
        (s,t,Time.max)::(pair' [] streams)
      in
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
      join (pair_streams streams) state
      |> List.map (fun (s, f, t) -> { streams = s; period = f, t })

    let streams db ids input limit from till duration _ _ () =
      let open Api.Api_types in
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from,till) ->
         (* TODO make it more sound *)
         Db.Streams.select_streams db ~with_pre:true ?limit ~from ~till
         >>= fun (Ok Raw { data = streams; has_more; order = `Desc }) ->
         let Some (_,oldest) = List.last_opt streams in
         Db.Device.select_state db ?limit ~from:oldest ~till
         >>= fun (Raw { data = states; has_more = _; order = `Desc }) ->
         let v = merge_streams_state streams states in
         let rval = Raw { data = v; has_more; order = `Desc } in
         respond_result (Ok (rows_to_yojson archived_list_to_yojson  (fun () -> `Null) rval))
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

    let struct_to_yojson =
      Json.(List.to_yojson (Pair.to_yojson
                              Stream.id_to_yojson
                              structure_to_yojson))

    let structure db id limit from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from,till) ->
         Db.Streams.select_structs_ts db ~with_pre:true
           ?limit ~ids:[id] ~from ~till
         |> Lwt_result.map (fun d -> Api.Api_types.rows_to_yojson
                                       struct_to_yojson (fun () -> `Null) d)
         |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
         >>= fun x -> respond_result x
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

    let bitrate id limit compress from till duration _ _ () =
      respond_error ~status:`Not_implemented "FIXME" ()


  end

end

let handler db (api:api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "streams"
    [ create_ws_handler ~docstring:"Pushes available streams to the client"
        ~path:Path.Format.empty
        ~query:Query.[ "id", (module List(Int32)) ]
        (WS.streams events)
    ; create_ws_handler ~docstring:"Pushes stream bitrate to the client"
        ~path:Path.Format.(Int32 ^/ "bitrate" @/ empty)
        ~query:Query.empty
        (WS.bitrate events)
    ; create_ws_handler ~docstring:"Pushes stream structure to the client"
        ~path:Path.Format.(Int32 ^/ "structure" @/ empty)
        ~query:Query.empty
        (WS.structure events)
    ; create_ws_handler ~docstring:"Pushes T2-MI structure to the client"
        ~path:Path.Format.(Int32 ^/ "t2mi/structure" @/ empty)
        ~query:Query.[ "t2mi-stream-id", (module List(Int)) ]
        (WS.T2MI.structure events)
    ; create_ws_handler ~docstring:"Pushes TS errors to the client"
        ~path:Path.Format.(Int32 ^/ "errors" @/ empty)
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))]
        (fun x -> WS.Errors.errors events [x])
    ]
    [ `GET,
      [ create_handler ~docstring:"Returns streams list"
          ~path:Path.Format.empty
          ~query:Query.[ "id",       (module List(Int32))
                       ; "input",    (module List(Topology.Show_topo_input))
                       ; "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Archive.streams db)
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
          HTTP.Archive.bitrate
      ; create_handler ~docstring:"Returns TS structure"
          ~path:Path.Format.(Int32 ^/ "structure" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Archive.structure db)
      (* T2-MI*)
      ; create_handler ~docstring:"Returns T2-MI packet sequence"
          ~path:Path.Format.(Int32 ^/ "t2mi/sequence" @/ empty)
          ~query:Query.[ "t2mi-stream-id", (module List(Int))
                       ; "duration",       (module Option(Time.Relative)) ]
          (HTTP.T2MI.sequence api)
      ; create_handler ~docstring:"Returns T2-MI structure"
          ~path:Path.Format.(Int32 ^/ "t2mi/structure" @/ empty)
          ~query:Query.[ "t2mi-stream-id", (module List(Int))
                       ; "limit",          (module Option(Int))
                       ; "from",           (module Option(Time.Show))
                       ; "to",             (module Option(Time.Show))
                       ; "duration",       (module Option(Time.Relative)) ]
          (HTTP.Archive.T2MI.structure db)
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
          (fun x -> HTTP.Archive.Errors.errors db [x])
      ; create_handler ~docstring:"Returns TS errors presence percentage"
          ~path:Path.Format.(Int32 ^/ "errors/percent" @/ empty)
          ~query:Query.[ "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (fun x -> HTTP.Archive.Errors.percent db [x])
      ; create_handler ~docstring:"Returns if TS errors were present for the requested period"
          ~path:Path.Format.(Int32 ^/ "errors/has-any" @/ empty)
          ~query:Query.[ "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (fun x -> HTTP.Archive.Errors.has_any db [x])
      ]
    ]
