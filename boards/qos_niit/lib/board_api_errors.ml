open Containers
open Board_types
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Common
open Types

(* module WS = struct
 * 
 *   open Errors
 * 
 *   let rec filter (acc:Stream.id * t list) = function
 *     | []    -> true
 *     | f::tl -> (match f acc with
 *                 | _,[] -> false
 *                 | acc  -> filter acc tl)
 * 
 *   let to_yojson = Json.(Pair.to_yojson Stream.id_to_yojson (List.to_yojson to_yojson))
 * 
 *   let flst fltr f = match fltr with
 *     | [] -> None
 *     | l  -> Some (f l)
 * 
 *   module TS = struct
 * 
 *     let errors (events:events) ids errors priority pids _ body sock_data () =
 *       let ids = List.map Stream.id_of_int32 ids in
 *       let f_stream = flst ids      (fun l (s,e) -> if List.mem ~eq:Stream.equal_id s l then s,e else s,[]) in
 *       let f_errors = flst errors   (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.err_code l) e) in
 *       let f_prior  = flst priority (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.err_code l) e) in
 *       let f_pids   = flst pids     (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.pid l) e) in
 *       let fns = List.filter_map (fun x -> x) [f_stream;f_errors;f_prior;f_pids] in
 *       let e   = React.E.fmap (fun (l:errors) ->
 *                     match List.filter (fun errs -> filter errs fns) l with
 *                     | [] -> None
 *                     | l  -> Some l) events.ts_errors
 *       in Api.Socket.handler socket_table sock_data e (Json.List.to_yojson to_yojson) body
 * 
 *   end
 * 
 *   module T2MI = struct
 * 
 *     let errors (events:events) ids t2mi_ids errors pids _ body sock_data () =
 *       let ids = List.map Stream.id_of_int32 ids in
 *       let f_stream = flst ids      (fun l (s,e) -> if List.mem ~eq:Stream.equal_id s l then s,e else s,[]) in
 *       let f_t2mi   = flst t2mi_ids (fun l (s,e) -> s,List.filter (fun x -> let id = Int32.to_int x.param_2 in
 *                                                                            List.mem ~eq:(=) id l) e) in
 *       let f_errors = flst errors   (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.err_code l) e) in
 *       let f_pids   = flst pids     (fun l (s,e) -> s,List.filter (fun x -> List.mem ~eq:(=) x.pid l) e) in
 *       let fns = List.filter_map (fun x -> x) [f_stream;f_t2mi;f_errors;f_pids] in
 *       let e   = React.E.fmap (fun (l:errors) ->
 *                     match List.filter (fun errs -> filter errs fns) l with
 *                     | [] -> None
 *                     | l  -> Some l) events.t2mi_errors
 *       in Api.Socket.handler socket_table sock_data e (Json.List.to_yojson to_yojson) body
 * 
 *   end
 * 
 * end
 * 
 * module HTTP = struct
 * 
 *   module TS = struct
 * 
 *     module Archive = struct
 * 
 *       open Errors
 * 
 *       type err = (Common.Stream.id * Errors.t) list [@@deriving yojson]
 * 
 *       let errors db streams errors priority pids limit compress from till duration _ _ () =
 *         match Time.make_interval ?from ?till ?duration () with
 *         | Ok (`Range (from,till)) ->
 *            (match compress with
 *             | Some true ->
 *                (Db.Errors.select_errors_compressed
 *                   db ~is_ts:true ~streams ~priority ~errors ~pids ~from ~till ())
 *             | _ -> Db.Errors.select_errors db ~is_ts:true ~streams ~priority ~errors ~pids ?limit ~from ~till ())
 *            >>= fun v ->
 *            respond_result (Ok Db.Errors.(Api.Api_types.rows_to_yojson raw_to_yojson compressed_to_yojson v))
 *         | _ -> respond_error ~status:`Not_implemented "not implemented" ()
 * 
 *       let percent db streams errors priority pids from till duration _ _ () =
 *         match Time.make_interval ?from ?till ?duration () with
 *         | Ok (`Range (from,till)) ->
 *            Db.Errors.select_percent db ~is_ts:true ~streams ~priority ~errors ~pids ~from ~till ()
 *            >>= fun v -> respond_result (Ok (`Float v))
 *         | _ -> respond_error ~status:`Not_implemented "not implemented" ()
 * 
 *       let has_any db streams errors priority pids from till duration _ _() =
 *         match Time.make_interval ?from ?till ?duration () with
 *         | Ok (`Range (from,till)) ->
 *            Db.Errors.select_has_any db ~is_ts:true ~streams ~priority ~errors ~pids ~from ~till ()
 *            >>= fun v -> respond_result (Ok (`Bool v))
 *         | _ -> respond_error ~status:`Not_implemented "not implemented" ()
 * 
 *     end
 *   end
 * 
 *   module T2MI = struct
 * 
 *     module Archive = struct
 * 
 *       open Errors
 *          
 *       let errors db streams t2mi_id errors pids limit compress from till duration _ _ () =
 *         match Time.make_interval ?from ?till ?duration () with
 *         | Ok (`Range (from,till)) -> begin
 *             match compress with
 *             | Some true -> Db.Errors.select_errors_compressed db ~is_ts:false ~streams ~errors ~pids ~from ~till ()
 *             | _ -> Db.Errors.select_errors db ~is_ts:true ~streams ~errors ~pids ?limit ~from ~till ()
 *           end >>= fun v ->
 *                                      respond_result (Ok Db.Errors.(Api.Api_types.rows_to_yojson raw_to_yojson compressed_to_yojson v))
 *         | _ -> respond_error ~status:`Not_implemented "not implemented" ()
 *              
 *       let percent db streams t2mi_id errors pids from till duration _ _ () =
 *         match Time.make_interval ?from ?till ?duration () with
 *         | Ok (`Range (from,till)) ->
 *            Db.Errors.select_percent db ~is_ts:true ~streams ~errors ~pids ~from ~till ()
 *            >>= fun v -> respond_result (Ok (`Float v))
 *         | _ -> respond_error ~status:`Not_implemented "not implemented" ()
 *              
 *       let has_any db streams t2mi_id errors pids from till duration _ _() =
 *         match Time.make_interval ?from ?till ?duration () with
 *         | Ok (`Range (from,till)) ->
 *            Db.Errors.select_has_any db ~is_ts:true ~streams ~errors ~pids ~from ~till ()
 *            >>= fun v -> respond_result (Ok (`Bool v))
 *         | _ -> respond_error ~status:`Not_implemented "not implemented" ()
 * 
 *     end
 * 
 *   end
 * end
 * 
 * let ts_handler db events =
 *   let open Uri in
 *   let open Boards.Board.Api_handler in
 *   create_dispatcher
 *     "ts"
 *     [ create_ws_handler ~docstring:"Pushes TS errors to the client"
 *         ~path:Path.Format.empty
 *         ~query:Query.[ "id",       (module List(Int32))
 *                      ; "errors",   (module List(Int))
 *                      ; "priority", (module List(Int))
 *                      ; "pid",      (module List(Int)) ]
 *         (WS.TS.errors events)
 *     ]
 *     [ `GET, [ create_handler ~docstring:"Returns archived TS errors"
 *                 ~path:Path.Format.("archive" @/ empty)
 *                 ~query:Query.[ "stream-id",(module List(Int32))
 *                              ; "errors",   (module List(Int))
 *                              ; "priority", (module List(Int))
 *                              ; "pid",      (module List(Int))
 *                              ; "limit",    (module Option(Int))
 *                              ; "compress", (module Option(Bool))
 *                              ; "from",     (module Option(Time.Show))
 *                              ; "to",       (module Option(Time.Show))
 *                              ; "duration", (module Option(Time.Relative)) ]
 *                 (HTTP.TS.Archive.errors db)
 *             ; create_handler ~docstring:"Returns TS errors presence percentage"
 *                 ~path:Path.Format.("archive/percent" @/ empty)
 *                 ~query:Query.[ "stream-id",(module List(Int32))
 *                              ; "errors",   (module List(Int))
 *                              ; "priority", (module List(Int))
 *                              ; "pid",      (module List(Int))
 *                              ; "from",     (module Option(Time.Show))
 *                              ; "to",       (module Option(Time.Show))
 *                              ; "duration", (module Option(Time.Relative)) ]
 *                 (HTTP.TS.Archive.percent db)
 *             ; create_handler ~docstring:"Returns if TS errors were present for the requested period"
 *                 ~path:Path.Format.("archive/has-any" @/ empty)
 *                 ~query:Query.[ "stream-id",(module List(Int32))
 *                              ; "errors",   (module List(Int))
 *                              ; "priority", (module List(Int))
 *                              ; "pid",      (module List(Int))
 *                              ; "from",     (module Option(Time.Show))
 *                              ; "to",       (module Option(Time.Show))
 *                              ; "duration", (module Option(Time.Relative)) ]
 *                 (HTTP.TS.Archive.has_any db)
 *             ]
 *     ]
 * 
 * let t2mi_handler db events =
 *   let open Uri in
 *   let open Boards.Board.Api_handler in
 *   create_dispatcher
 *     "t2mi"
 *     [ create_ws_handler ~docstring:"Pushes T2-MI errors to the client"
 *         ~path:Path.Format.empty
 *         ~query:Query.[ "stream-id",      (module List(Int32))
 *                      ; "t2mi-stream-id", (module List(Int))
 *                      ; "errors",         (module List(Int))
 *                      ; "pid",            (module List(Int)) ]
 *         (WS.T2MI.errors events)
 *     ]
 *     [ `GET, [ create_handler ~docstring:"Returns archived T2-MI errors"
 *                 ~path:Path.Format.("archive" @/ empty)
 *                 ~query:Query.[ "stream-id",      (module List(Int32))
 *                              ; "t2mi-stream-id", (module List(Int))
 *                              ; "errors",         (module List(Int))
 *                              ; "pid",            (module List(Int))
 *                              ; "limit",          (module Option(Int))
 *                              ; "compress",       (module Option(Bool))
 *                              ; "from",           (module Option(Time.Show))
 *                              ; "to",             (module Option(Time.Show))
 *                              ; "duration",       (module Option(Time.Relative)) ]
 *                 (HTTP.T2MI.Archive.errors db)
 *             ; create_handler ~docstring:"Returns T2-MI errors presense percentage"
 *                 ~path:Path.Format.("archive/percent" @/ empty)
 *                 ~query:Query.[ "stream-id",      (module List(Int32))
 *                              ; "t2mi-stream-id", (module List(Int))
 *                              ; "errors",         (module List(Int))
 *                              ; "pid",            (module List(Int))
 *                              ; "from",           (module Option(Time.Show))
 *                              ; "to",             (module Option(Time.Show))
 *                              ; "duration",       (module Option(Time.Relative)) ]
 *                 (HTTP.T2MI.Archive.percent db)
 *             ; create_handler ~docstring:"Returns if T2-MI errors were present for the requested period"
 *                 ~path:Path.Format.("archive/has-any" @/ empty)
 *                 ~query:Query.[ "stream-id",      (module List(Int32))
 *                              ; "t2mi-stream-id", (module List(Int))
 *                              ; "errors",         (module List(Int))
 *                              ; "pid",            (module List(Int))
 *                              ; "from",           (module Option(Time.Show))
 *                              ; "to",             (module Option(Time.Show))
 *                              ; "duration",       (module Option(Time.Relative)) ]
 *                 (HTTP.T2MI.Archive.has_any db) ]
 *     ]
 * 
 * let handlers db events =
 *   [ ts_handler db events
 *   ; t2mi_handler db events
 *   ] *)
