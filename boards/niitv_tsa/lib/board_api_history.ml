open Containers
open Board_qos_types
open Api.Interaction
open Api.Interaction.Json
open Board_api_common
open Common

module HTTP = struct

  module Streams = struct

    open Board_types.Streams

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

    let streams_unique db (events : events) ids inputs incoming
          from till duration () =
      let open Api.Api_types in
      let open Common.Stream in
      let open Lwt_result.Infix in
      let merge (cur : t list) streams =
        let filter (s, id, t) =
          if List.exists (fun s -> Stream.ID.equal id s.id) cur
          then None
          else Some (s,`Last t) in
        let streams = List.filter_map filter streams in
        (List.map (fun s -> s,`Now) cur) @ streams
      in
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from,till) ->
         Db.Streams.select_stream_unique db ~ids ~inputs ?incoming ~from ~till ()
         >>= (function
              | Raw _ -> assert false
              | Compressed { data } ->
                 let current =
                   React.S.value events.streams
                   |> List.filter (fun (s:Stream.t) ->
                          let input = Option.get_exn @@ Stream.get_input s in (* FIXME handle None *)
                          List.mem ~eq:Topology.equal_topo_input input inputs) in
                 Lwt_result.return (Compressed { data = merge current data }))
         |> Lwt_result.map (fun x ->
                rows_to_yojson
                  (fun _ -> assert false)
                  streams_unique_to_yojson x)
         |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
         |> fun t -> Lwt.bind t respond_result
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

    let streams_states db ids inputs limit incoming from till duration () =
      let open Api.Api_types in
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from, till) ->
         (* TODO make it more sound *)
         Db.Streams.select_streams ?limit ?incoming ~ids ~inputs ~from ~till db
         |> Lwt_result.map (fun x ->
                rows_to_yojson
                  streams_states_to_yojson
                  (fun _ -> assert false) x)
         |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
         >>= respond_result
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

    let streams db events ids inputs limit incoming compress
          from till duration _ _ () =
      match compress with
      | Some false | None ->
         streams_states db ids inputs limit incoming from till duration ()
      | Some true ->
         streams_unique db events ids inputs incoming from till duration ()

  end

  module Ts_info = struct

    let to_yojson =
      let f = Time.timespan_to_yojson Ts_info.to_yojson in
      Api.Api_types.rows_to_yojson
        (stream_assoc_to_yojson f)
        (fun () -> `Null)

    let get db ids limit from till duration _ _ () =
      match Time.make_interval ?from ?till ?duration () with
      | Ok `Range (from, till) ->
         Db.Ts_info.select ~with_pre:false ?limit ~ids ~from ~till db
         |> Lwt_result.map to_yojson
         |> Lwt_result.map_err (fun s -> `String s)
         >>= respond_result
      | Error e -> respond_error e ()
      | _ -> respond_error ~status:`Not_implemented "FIXME" ()

  end

  module Services = struct

  end

  module Tables = struct

  end

  module Pids = struct

  end

  module T2mi_info = struct

  end

  module Errors = struct

    open Error

    let raw_to_yojson =
      Json.(List.to_yojson
              (Pair.to_yojson Stream.ID.to_yojson
               @@ List.to_yojson Error.to_yojson))

    let errors db streams errors priority pids
          limit desc compress from till duration _ _ () =
      let order = Option.map (function
                      | true -> `Desc
                      | false -> `Asc) desc in
      match Time.make_interval ?from ?till ?duration () with
      | Ok (`Range (from, till)) ->
         begin match compress with
         | None | Some false ->
            Db.Errors.select_errors db ~is_ts:true ~streams
              ~priority ~errors ~pids ?order ?limit ~from ~till ()
         | Some true ->
            Db.Errors.select_errors_compressed
              db ~is_ts:true ~streams ~priority
              ~errors ~pids ~from ~till ()
         end
         >>= fun v ->
         let r = Ok (Api.Api_types.rows_to_yojson
                       raw_to_yojson
                       compressed_to_yojson
                       v) in
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

let handler db events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "history"
    []
    [ `GET,
      [ create_handler ~docstring:"Returns TS info"
          ~path:Path.Format.("ts-info" @/ empty)
          ~query:Query.[ "id", (module List(Stream.ID))
                       ; "limit", (module Option(Int))
                       ; "from", (module Option(Time.Show))
                       ; "to", (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Ts_info.get db)
      (* Streams *)
      ; create_handler ~docstring:"Returns streams"
          ~path:Path.Format.("streams" @/ empty)
          ~query:Query.[ "id", (module List(Stream.ID))
                       ; "input", (module List(Topology.Show_topo_input))
                       ; "limit", (module Option(Int))
                       ; "incoming", (module Option(Bool))
                       ; "compress", (module Option(Bool))
                       ; "from", (module Option(Time.Show))
                       ; "to", (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Streams.streams db events)
      (* Errors *)
      ; create_handler ~docstring:"Returns TS errors"
          ~path:Path.Format.("errors" @/ empty)
          ~query:Query.[ "id", (module List(Stream.ID))
                       ; "errors", (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid", (module List(Int))
                       ; "limit", (module Option(Int))
                       ; "desc", (module Option(Bool))
                       ; "compress", (module Option(Bool))
                       ; "from", (module Option(Time.Show))
                       ; "to", (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Errors.errors db)
      ; create_handler ~docstring:"Returns TS errors presence percentage"
          ~path:Path.Format.("errors/percent" @/ empty)
          ~query:Query.[ "id", (module List(Stream.ID))
                       ; "errors", (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid", (module List(Int))
                       ; "from", (module Option(Time.Show))
                       ; "to", (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Errors.percent db)
      ; create_handler ~docstring:"Returns if TS errors were present for the requested period"
          ~path:Path.Format.("errors/has-any" @/ empty)
          ~query:Query.[ "id", (module List(Stream.ID))
                       ; "errors", (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid", (module List(Int))
                       ; "from", (module Option(Time.Show))
                       ; "to", (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          (HTTP.Errors.has_any db)
      ]
    ]
