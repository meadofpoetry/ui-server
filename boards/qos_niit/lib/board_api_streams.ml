open Containers
open Board_types
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Common
open Types

type events = streams_events

module WS = struct

  module TS = struct

    open Board_types.Streams.TS

    let to_yojson f v = Json.(List.to_yojson (Pair.to_yojson Stream.id_to_yojson f) v)

    let streams (events:events) ids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let e = match ids with
        | [] -> React.S.changes events.streams
        | l  -> React.E.fmap (fun streams ->
                    List.filter (fun (s:Stream.t) -> match s.id with
                                                     | `Ts id -> List.mem ~eq:(Stream.equal_id) id l
                                                     | _      -> false) streams
                    |> function [] -> None | l -> Some l) (React.S.changes events.streams)
      in Api.Socket.handler socket_table sock_data e (Json.List.to_yojson Stream.to_yojson) body

    let state (events:events) ids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let e   = match ids with
        | [] -> events.ts_states
        | l  -> React.E.fmap (fun states ->
                    List.filter (fun (id,_) -> List.mem ~eq:(Stream.equal_id) id l) states
                    |> function [] -> None | l -> Some l) events.ts_states
      in Api.Socket.handler socket_table sock_data e (to_yojson state_to_yojson) body

    let bitrate (events:events) ids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let e   = match ids with
        | [] -> events.ts_bitrates
        | l  -> React.E.fmap (fun bitrates ->
                    List.filter (fun (id,_) -> List.mem ~eq:(Stream.equal_id) id l) bitrates
                    |> function [] -> None | l -> Some l)
                  events.ts_bitrates
      in Api.Socket.handler socket_table sock_data e (to_yojson bitrate_to_yojson) body

    let structure (events:events) ids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let e   = match ids with
        | [] -> events.ts_structures
        | l  -> React.E.fmap (fun structures ->
                    List.filter (fun (id,_) -> List.mem ~eq:(Stream.equal_id) id l) structures
                    |> function [] -> None | l -> Some l)
                  events.ts_structures
      in Api.Socket.handler socket_table sock_data e (to_yojson structure_to_yojson) body

  end

  module T2MI = struct

    open Board_types.Streams.T2MI

    let to_yojson f v = Json.(List.to_yojson (Pair.to_yojson Int.to_yojson f) v)

    let state (events:events) ids _ body sock_data () =
      let e   = match ids with
        | [] -> events.t2mi_states
        | l  -> React.E.fmap (fun states ->
                    List.filter (fun (id,_) -> List.mem ~eq:(=) id l) states
                    |> function [] -> None | l -> Some l) events.t2mi_states
      in Api.Socket.handler socket_table sock_data e (to_yojson state_to_yojson) body

    let structure (events:events) ids _ body sock_data () =
      let e   = match ids with
        | [] -> events.t2mi_structures
        | l  -> React.E.fmap (fun structures ->
                    List.filter (fun (id,_) -> List.mem ~eq:(=) id l) structures
                    |> function [] -> None | l -> Some l)
                events.t2mi_structures
      in Api.Socket.handler socket_table sock_data e (to_yojson structure_to_yojson) body

  end

end

module HTTP = struct

  module TS = struct

    open Board_types.Streams.TS

    let to_yojson f v = Json.(List.to_yojson (Pair.to_yojson Stream.id_to_yojson f) v)
                      
    let streams (events:events) ids _ _ () =
      let ids = List.map Stream.id_of_int32 ids in
      let streams = match ids with
        | [] -> React.S.value events.streams
        | l  -> List.filter (fun (s:Stream.t) -> match s.id with
                                                 | `Ts id -> List.mem ~eq:Stream.equal_id id l
                                                 | _      -> false)
                @@ React.S.value events.streams
      in (Json.List.to_yojson Stream.to_yojson) streams |> Result.return |> respond_result

    let state (api:api) ids _ _ () =
      let ids = List.map Stream.id_of_int32 ids in
      let states = match ids with
        | [] -> api.get_ts_states ()
        | l  -> List.filter (fun (id,_) -> List.mem ~eq:Stream.equal_id id l)
                  (api.get_ts_states ())
      in (to_yojson state_to_yojson) states |> Result.return |> respond_result

    let bitrate (api:api) ids _ _ () =
      let ids = List.map Stream.id_of_int32 ids in
      let bitrates = match ids with
        | [] -> api.get_ts_bitrates ()
        | l  -> List.filter (fun (id,_) -> List.mem ~eq:Stream.equal_id id l)
                  (api.get_ts_bitrates ())
      in (to_yojson bitrate_to_yojson) bitrates |> Result.return |> respond_result

    let structure (api:api) ids _ _ () =
      let ids = List.map Stream.id_of_int32 ids in
      let structs = match ids with
        | [] -> api.get_ts_structures ()
        | l  -> List.filter (fun (id,_) -> List.mem ~eq:Stream.equal_id id l)
                  (api.get_ts_structures ())
      in (to_yojson structure_to_yojson) structs |> Result.return |> respond_result

    module Archive = struct

      type strms = (Common.Stream.t list * Time.t) list [@@deriving yojson]

      type struct_ts = (Common.Stream.id * Board_types.Streams.TS.structure * Time.t) list [@@deriving yojson]

      type strms_state = (Common.Stream.t * Time.t * Time.t) list [@@deriving yojson]

      type stream_ids = Common.Stream.stream_id list [@@deriving yojson]

      type stream_uniq = Common.Stream.t list [@@deriving yojson]

      (* merge ordered descending lists of streams and board states *)
      let merge_streams_state
            (streams : (Common.Stream.t list * Time.t * Time.t) list)
            (state : (Common.Topology.state * Time.t * Time.t) list) =
        (* TODO consider ord checks *)
        let (<=) l r = Time.compare l r <= 0 in
        let (>=) l r = Time.compare l r >= 0 in
        let join streams states = List.fold_left (fun acc (s,f,t) ->
                                      List.append acc @@ List.filter_map
                                                           (function (`Fine,ff,tt) ->
                                                                      if f <= ff && t >= tt then Some (s,ff,tt)
                                                                      else if f <=ff && t >= ff then Some (s, ff, t)
                                                                      else if f <= tt && t >= tt then Some (s, f, tt)
                                                                      else if f >= ff && t <= tt then Some (s, f, t)
                                                                      else None
                                                                   | _ -> None) states) [] streams
        in (* TODO add compress *) 
        join streams state

      let stream_unique db (events:events) limit from till duration _ _ () =
        let open Api.Api_types in
        let open Common.Stream in
        let open Lwt_result.Infix in
        let merge (cur:t list) streams =
          let cur = List.filter (fun s -> List.exists (equal s) streams) cur in
          cur @ streams
        in
        match Time.make_interval ?from ?till ?duration () with
        | Ok `Range (from,till) ->
           Db.Streams.select_stream_unique db ?limit ~from ~till ()
           >>= (fun (Raw { data; has_more; order }) ->
            let current = React.S.value events.streams in
            Lwt_result.return (Raw { data = merge current data; has_more; order }))
           |> Lwt_result.map (fun x -> rows_to_yojson stream_uniq_to_yojson (fun () -> `Null) x)
           |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
           |> fun t -> Lwt.bind t respond_result
        | _ -> respond_error ~status:`Not_implemented "FIXME" ()
        

      let stream_ids db (events:events) limit from till duration _ _ () =
        let open Api.Api_types in
        let open Common.Stream in
        let merge (cur:stream_id list) ids =
          let cur = List.filter (function `Ip _ -> true
                                        | `Ts id -> not @@ List.exists (Int32.equal (id_to_int32 id)) ids)
                      cur
          in cur @ List.map (fun x -> `Ts (id_of_int32 x)) ids
        in
        match Time.make_interval ?from ?till ?duration () with
        | Ok `Range (from,till) ->
           Db.Streams.select_stream_ids db ?limit ~from ~till ()
           >>= fun (Raw { data; has_more; order }) ->
           let current = List.map (fun s -> s.id) @@ React.S.value events.streams in
           let rval = Raw { data = merge current data; has_more; order } in
           respond_result (Ok (rows_to_yojson stream_ids_to_yojson (fun () -> `Null) rval))
        | _ -> respond_error ~status:`Not_implemented "FIXME" ()
      
      let streams db limit ids from till duration _ _ () =
        let open Api.Api_types in
        match Time.make_interval ?from ?till ?duration () with
        | Ok `Range (from,till) ->
           (* TODO make it more sound *)
           Db.Streams.select_streams db ?limit ~ids ~from ~till ()
           |> Lwt_result.map (fun x -> rows_to_yojson strms_state_to_yojson (fun () -> `Null) x)
           |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
           >>= respond_result 
           (*|> Lwt_result.map (fun d -> Api.Api_types.rows_to_yojson strms_to_yojson (fun () -> `Null) d)
           |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
           >>= fun x -> respond_result x*)
        | _ -> respond_error ~status:`Not_implemented "FIXME" ()

        (*
      let state ids limit compress from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()
         *)
      let structure db ids limit from till duration _ _ () =
        match Time.make_interval ?from ?till ?duration () with
        | Ok `Range (from,till) ->
           Db.Streams.select_structs_ts db ~with_pre:true ?limit ~ids ~from ~till          
           |> Lwt_result.map (fun d -> Api.Api_types.rows_to_yojson struct_ts_to_yojson (fun () -> `Null) d)
           |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
           >>= fun x -> respond_result x
        | _ -> respond_error ~status:`Not_implemented "FIXME" ()

      let bitrate ids limit compress from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()

    end

  end

  module T2MI = struct

    open Board_types.Streams.T2MI

    let to_yojson f v = Json.(List.to_yojson (Pair.to_yojson Int.to_yojson f) v)

    let state (api:api) ids _ _ () =
      let states = match ids with
        | [] -> api.get_t2mi_states ()
        | l  -> List.filter (fun (id,_) -> List.mem ~eq:(=) id l)
                  (api.get_t2mi_states ())
      in (to_yojson state_to_yojson) states |> Result.return |> respond_result

    let structure (api:api) ids _ _ () =
      let structs = match ids with
        | [] -> api.get_t2mi_structures ()
        | l  -> List.filter (fun (id,_) -> List.mem ~eq:(=) id l)
                  (api.get_t2mi_structures ())
      in (to_yojson structure_to_yojson) structs |> Result.return |> respond_result

    let sequence (api:api) ids duration _ _ () =
      let seconds = Option.flat_map Time.Relative.to_int_s duration in
      api.get_t2mi_seq seconds
      >|= (fun x -> let seq = match ids with
                      | [] -> x
                      | l  -> List.filter (fun (x:sequence_item) -> List.mem ~eq:(=) x.stream_id l) x
                    in sequence_to_yojson seq |> Result.return)
      >>= respond_result

    module Archive = struct

      open Board_types.Streams.T2MI

      type struct_t2 = (int * Board_types.Streams.T2MI.structure * Time.t) list [@@deriving yojson]
         (*
      let state ids limit compress from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()
          *)
      let structure db ids limit from till duration _ _ () =
        match Time.make_interval ?from ?till ?duration () with
        | Ok `Range (from,till) ->
           Db.Streams.select_structs_t2 db ~with_pre:true ?limit ~ids ~from ~till
           |> Lwt_result.map (fun d -> Api.Api_types.rows_to_yojson struct_t2_to_yojson (fun () -> `Null) d)
           |> Lwt_result.map_err (fun s -> (`String s : Yojson.Safe.json))
           >>= fun x -> respond_result x
        | _ -> respond_error ~status:`Not_implemented "FIXME" ()
             
    end

  end

end

let ts_handler db (api:api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "ts"
    [ create_ws_handler ~docstring:"Pushes available TS to the client"
        ~path:Path.Format.empty
        ~query:Query.[ "id", (module List(Int32)) ]
        (WS.TS.streams events)
    ; create_ws_handler ~docstring:"Pushes TS state change to the client"
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.[ "id", (module List(Int32)) ]
        (WS.TS.state events)
    ; create_ws_handler ~docstring:"Pushes TS bitrates to the client"
        ~path:Path.Format.("bitrate" @/ empty)
        ~query:Query.[ "id", (module List(Int32)) ]
        (WS.TS.bitrate events)
    ; create_ws_handler ~docstring:"Pushes TS structure to the client"
        ~path:Path.Format.("structure" @/ empty)
        ~query:Query.[ "id", (module List(Int32)) ]
        (WS.TS.structure events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns current TS list"
                ~path:Path.Format.empty
                ~query:Query.[ "id", (module List(Int32)) ]
                (HTTP.TS.streams events)
            ; create_handler ~docstring:"Returns current TS states"
                ~path:Path.Format.("state" @/ empty)
                ~query:Query.[ "id", (module List(Int32)) ]
                (HTTP.TS.state api)
            ; create_handler ~docstring:"Returns current TS bitrate"
                ~path:Path.Format.("bitrate" @/ empty)
                ~query:Query.[ "id", (module List(Int32)) ]
                (HTTP.TS.bitrate api)
            ; create_handler ~docstring:"Returns current TS structure"
                ~path:Path.Format.("structure" @/ empty)
                ~query:Query.[ "id", (module List(Int32)) ]
                (HTTP.TS.structure api)
            (* Archive *)
            ; create_handler ~docstring:"Returns archived streams"
                ~path:Path.Format.("archive" @/ empty)
                ~query:Query.[ "limit",    (module Option(Int))
                             ; "ids",      (module List(Int32))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                (HTTP.TS.Archive.streams db)
            ; create_handler ~docstring:"Returns archived streams"
                ~path:Path.Format.("archive/ids" @/ empty)
                ~query:Query.[ "limit",    (module Option(Int))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                (HTTP.TS.Archive.stream_ids db events)
            (*  ; create_handler ~docstring:"Retunrs archived stream state"
                ~path:Path.Format.("state/archive" @/ empty)
                ~query:Query.[ "id",       (module List(Int32))
                             ; "limit",    (module Option(Int))
                             ; "compress", (module Option(Bool))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.TS.Archive.state;*)
            ; create_handler ~docstring:"Retunrs archived stream bitrate"
                ~path:Path.Format.("bitrate/archive" @/ empty)
                ~query:Query.[ "id",       (module List(Int32))
                             ; "limit",    (module Option(Int))
                             ; "compress", (module Option(Bool))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.TS.Archive.bitrate
            ; create_handler ~docstring:"Retunrs archived stream structure"
                ~path:Path.Format.("structure/archive" @/ empty)
                ~query:Query.[ "id",       (module List(Int32))
                             ; "limit",    (module Option(Int))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                (HTTP.TS.Archive.structure db)
            ]
    ]

let t2mi_handler db (api:api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "t2mi"
    [ create_ws_handler ~docstring:"Pushes stream state to the client"
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.["id", (module List(Int))]
        (WS.T2MI.state events)
    ; create_ws_handler ~docstring:"Pushes stream structure to the client"
        ~path:Path.Format.("structure" @/ empty)
        ~query:Query.["id", (module List(Int))]
        (WS.T2MI.structure events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns stream state"
                ~path:Path.Format.("state" @/ empty)
                ~query:Query.[ "id", (module List(Int)) ]
                (HTTP.T2MI.state api)
            ; create_handler ~docstring:"Returns T2-MI stream structure (L1 signalling)"
                ~path:Path.Format.("structure" @/ empty)
                ~query:Query.[ "id", (module List(Int)) ]
                (HTTP.T2MI.structure api)
            ; create_handler ~docstring:"Returns T2-MI packet sequence"
                ~path:Path.Format.("sequence" @/ empty)
                ~query:Query.[ "id",       (module List(Int))
                             ; "duration", (module Option(Time.Relative)) ]
                (HTTP.T2MI.sequence api)
            (* Archive *)
           (* ; create_handler ~docstring:"Returns archived stream state"
                ~path:Path.Format.("state/archive" @/ empty)
                ~query:Query.[ "id",       (module List(Int))
                             ; "limit",    (module Option(Int))
                             ; "compress", (module Option(Bool))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.T2MI.Archive.state *)
            ; create_handler ~docstring:"Returns archived stream structure"
                ~path:Path.Format.("structure/archive" @/ empty)
                ~query:Query.[ "id",       (module List(Int))
                             ; "limit",    (module Option(Int))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                (HTTP.T2MI.Archive.structure db)
            ]
    ]

let handlers db api events =
  [ ts_handler db api events
  ; t2mi_handler db api events
  ]
