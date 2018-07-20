open Containers
open Board_types
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Common
open Types

module WS = struct

  open Board_types.Streams.TS

  let to_yojson f v = Json.(List.to_yojson (Pair.to_yojson Stream.id_to_yojson f) v)

  let bitrate (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let e  = React.E.fmap (List.Assoc.get ~eq:Stream.equal_id id) events.streams.ts_bitrates in
    Api.Socket.handler socket_table sock_data e bitrate_to_yojson body

  let structure (events:events) id _ body sock_data () =
    let id = Stream.id_of_int32 id in
    let e  = React.E.fmap (List.Assoc.get ~eq:Stream.equal_id id) events.streams.ts_structures in
    Api.Socket.handler socket_table sock_data e structure_to_yojson body

  open Errors

  let rec filter (acc:Stream.id * t list) = function
    | []    -> true
    | f::tl -> (match f acc with
                | _,[] -> false
                | acc  -> filter acc tl)

  let to_yojson = Json.(Pair.to_yojson Stream.id_to_yojson (List.to_yojson to_yojson))

  let flst fltr f = match fltr with
    | [] -> None
    | l  -> Some (f l)

  let errors (events:events) ids errors priority pids _ body sock_data () =
    let ids = List.map Stream.id_of_int32 ids in
    let f_stream = flst ids      (fun l (s,e) -> if List.mem ~eq:Stream.equal_id s l then s,e else s,[]) in
    let f_errors = flst errors   (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.err_code l) e) in
    let f_prior  = flst priority (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.err_code l) e) in
    let f_pids   = flst pids     (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.pid l) e) in
    let fns = List.filter_map (fun x -> x) [f_stream;f_errors;f_prior;f_pids] in
    let e   = React.E.fmap (fun (l:errors) ->
                  match List.filter (fun errs -> filter errs fns) l with
                  | [] -> None
                  | l  -> Some l) events.errors.ts_errors
    in Api.Socket.handler socket_table sock_data e (Json.List.to_yojson to_yojson) body

end

module HTTP = struct

  open Board_types.Streams.TS

  let si_psi_section (api:api) id table_id section table_id_ext eit_ts_id eit_orig_nw_id _ _ () =
    let stream_id = Stream.id_of_int32 id in
    let req = { stream_id; table_id; section; table_id_ext; eit_ts_id; eit_orig_nw_id } in
    api.get_section req
    >|= (function Ok x    -> Ok    (section_to_yojson x)
                | Error e -> Error (section_error_to_yojson e))
    >>= respond_result

  let to_yojson f v = Json.(List.to_yojson (Pair.to_yojson Stream.id_to_yojson f) v)

  let bitrate (api:api) id _ _ () =
    let id = Stream.id_of_int32 id in
    match List.Assoc.get ~eq:(Stream.equal_id) id @@ api.get_ts_bitrates () with
    | None   -> `String "stream not found" |> Result.fail |> respond_result ~err_status:`Not_found
    | Some b -> bitrate_to_yojson b |> Result.return |> respond_result

  let structure (api:api) id _ _ () =
    let id = Stream.id_of_int32 id in
    match List.Assoc.get ~eq:(Stream.equal_id) id @@ api.get_ts_structures () with
    | None   -> `String "stream not found" |> Result.fail |> respond_result ~err_status:`Not_found
    | Some s -> structure_to_yojson s |> Result.return |> respond_result

  module Archive = struct

    let struct_to_yojson =
      Json.(List.to_yojson (Pair.to_yojson
                              Stream.id_to_yojson
                              structure_to_yojson))

    let structure db id limit from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from,till) ->
         Db.Streams.select_structs_ts db ~with_pre:true ?limit ~ids:[id] ~from ~till
         |> Lwt_result.map (fun d -> Api.Api_types.rows_to_yojson struct_to_yojson (fun () -> `Null) d)
         |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
         >>= fun x -> respond_result x
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

    let bitrate id limit compress from till duration _ _ () =
      respond_error ~status:`Not_implemented "FIXME" ()

    open Errors

    let errors db streams errors priority pids limit compress from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok (`Range (from,till)) ->
         (match compress with
          | Some true ->
             (Db.Errors.select_errors_compressed
                db ~is_ts:true ~streams ~priority ~errors ~pids ~from ~till ())
          | _ -> Db.Errors.select_errors db ~is_ts:true ~streams ~priority ~errors ~pids ?limit ~from ~till ())
         >>= fun v ->
         respond_result (Ok Db.Errors.(Api.Api_types.rows_to_yojson raw_to_yojson compressed_to_yojson v))
      | _ -> respond_error ~status:`Not_implemented "not implemented" ()

    let percent db streams errors priority pids from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok (`Range (from,till)) ->
         Db.Errors.select_percent db ~is_ts:true ~streams ~priority ~errors ~pids ~from ~till ()
         >>= fun v -> respond_result (Ok (`Float v))
      | _ -> respond_error ~status:`Not_implemented "not implemented" ()

    let has_any db streams errors priority pids from till duration _ _() =
      match Time.make_interval ?from ?till ?duration () with
      | Ok (`Range (from,till)) ->
         Db.Errors.select_has_any db ~is_ts:true ~streams ~priority ~errors ~pids ~from ~till ()
         >>= fun v -> respond_result (Ok (`Bool v))
      | _ -> respond_error ~status:`Not_implemented "not implemented" ()

  end

end

let handler db (api:api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "ts"
    [ create_ws_handler ~docstring:"Pushes stream bitrate to the client"
        ~path:Path.Format.(Int32 ^/ "bitrate" @/ empty)
        ~query:Query.empty
        (WS.bitrate events)
    ; create_ws_handler ~docstring:"Pushes stream structure to the client"
        ~path:Path.Format.(Int32 ^/ "structure" @/ empty)
        ~query:Query.empty
        (WS.structure events)
    ; create_ws_handler ~docstring:"Pushes TS errors to the client"
        ~path:Path.Format.(Int32 ^/ "errors" @/ empty)
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))]
        (fun x -> WS.errors events [x])
    ]
    [ `GET,
      [ create_handler ~docstring:"Returns stream bitrate"
          ~path:Path.Format.(Int32 ^/ "bitrate" @/ empty)
          ~query:Query.empty
          (HTTP.bitrate api)
      ; create_handler ~docstring:"Returns stream structure"
          ~path:Path.Format.(Int32 ^/ "structure" @/  empty)
          ~query:Query.empty
          (HTTP.structure api)
      (* Archive *)
      ; create_handler ~docstring:"Retunrs archived stream bitrate"
          ~path:Path.Format.(Int32 ^/ "bitrate/archive" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          HTTP.Archive.bitrate
      ; create_handler ~docstring:"Retunrs archived stream structure"
          ~path:Path.Format.(Int32 ^/ "structure/archive" @/ empty)
          ~query:Query.[ "limit",    (module Option(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Archive.structure db)
      ; create_handler ~docstring:"Returns archived TS errors"
          ~path:Path.Format.(Int32 ^/ "errors/archive" @/ empty)
          ~query:Query.[ "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (fun x -> HTTP.Archive.errors db [x])
      ; create_handler ~docstring:"Returns TS errors presence percentage"
          ~path:Path.Format.(Int32 ^/ "errors/archive/percent" @/ empty)
          ~query:Query.[ "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (fun x -> HTTP.Archive.percent db [x])
      ; create_handler ~docstring:"Returns if TS errors were present for the requested period"
          ~path:Path.Format.(Int32 ^/ "errors/archive has-any" @/ empty)
          ~query:Query.[ "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (fun x -> HTTP.Archive.has_any db [x])
      ]
    ]
