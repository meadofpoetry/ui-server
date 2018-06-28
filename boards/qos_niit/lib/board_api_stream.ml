open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect
open Common

type events = streams_events

module WS = struct

  module TS = struct

    open Board_types.Streams.TS

    let stream (events:events) id _ body sock_data () =
      let id = Stream.id_of_int32 id in
      let e  = React.E.map
                 (fun streams ->
                   List.find_opt (fun (s:Stream.t) -> match s.id with
                                                      | `Ts x -> Stream.equal_id id x
                                                      | _ -> false) streams)
               @@ React.S.changes events.streams
               |> React.E.changes ~eq:(Equal.option Stream.equal)
      in sock_handler sock_data e (Json.Option.to_yojson Stream.to_yojson) body

    let state (events:events) id _ body sock_data () =
      let id = Stream.id_of_int32 id in
      let e  = React.E.fmap (List.Assoc.get ~eq:Stream.equal_id id) events.ts_states in
      sock_handler sock_data e state_to_yojson body

    let bitrate (events:events) id _ body sock_data () =
      let id = Stream.id_of_int32 id in
      let e  = React.E.fmap (List.Assoc.get ~eq:Stream.equal_id id) events.ts_bitrates in
      sock_handler sock_data e bitrate_to_yojson body

    let structure (events:events) id _ body sock_data () =
      let id = Stream.id_of_int32 id in
      let e  = React.E.fmap (List.Assoc.get ~eq:Stream.equal_id id) events.ts_structures in
      sock_handler sock_data e structure_to_yojson body

  end

  module T2MI = struct

    open Board_types.Streams.T2MI

    let state (events:events) id _ body sock_data () =
      let e = React.E.fmap (List.Assoc.get ~eq:(=) id) events.t2mi_states in
      sock_handler sock_data e state_to_yojson body

    let structure (events:events) id _ body sock_data () =
      let e = React.E.fmap (List.Assoc.get ~eq:(=) id) events.t2mi_structures in
      sock_handler sock_data e structure_to_yojson body

  end

end

module HTTP = struct

  module TS = struct

    open Board_types.Streams.TS

    let stream (events:events) id _ _ () =
      let id = Stream.id_of_int32 id in
      let streams = React.S.value events.streams in
      match List.find_opt (fun (s:Stream.t) -> match s.id with
                                               | `Ts x -> Stream.equal_id id x
                                               | _ -> false) streams with
      | None   -> `String "not found" |> Result.fail |> respond_result
      | Some s -> Stream.to_yojson s |> Result.return |> respond_result

    let state (api:api) id _ _ () =
      let id = Stream.id_of_int32 id in
      match List.Assoc.get ~eq:(Stream.equal_id) id @@ api.get_ts_states () with
      | None   -> `String "not found" |> Result.fail |> respond_result
      | Some s -> state_to_yojson s |> Result.return |> respond_result

    let bitrate (api:api) id _ _ () =
      let id = Stream.id_of_int32 id in
      match List.Assoc.get ~eq:(Stream.equal_id) id @@ api.get_ts_bitrates () with
      | None   -> `String "not found" |> Result.fail |> respond_result
      | Some b -> bitrate_to_yojson b |> Result.return |> respond_result

    let structure (api:api) id _ _ () =
      let id = Stream.id_of_int32 id in
      match List.Assoc.get ~eq:(Stream.equal_id) id @@ api.get_ts_structures () with
      | None   -> `String "not found" |> Result.fail |> respond_result
      | Some s -> structure_to_yojson s |> Result.return |> respond_result

    let si_psi_section (api:api) id table_id section table_id_ext eit_ts_id eit_orig_nw_id _ _ () =
      let stream_id = Stream.id_of_int32 id in
      let req = { stream_id; table_id; section; table_id_ext; eit_ts_id; eit_orig_nw_id } in
      api.get_section req
      >|= (function Ok x    -> Ok    (section_to_yojson x)
                  | Error e -> Error (section_error_to_yojson e))
      >>= respond_result

    module Archive = struct

      let stream id limit from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()

      let state id filter limit compress from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()

      let structure id limit from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()

      let bitrate id limit compress from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()

    end

  end

  module T2MI = struct

    open Board_types.Streams.T2MI

    let state (api:api) id _ _ () =
      match List.Assoc.get ~eq:(=) id @@ api.get_t2mi_states () with
      | None   -> `String "not found" |> Result.fail |> respond_result
      | Some s -> state_to_yojson s |> Result.return |> respond_result

    let structure (api:api) id _ _ () =
      match List.Assoc.get ~eq:(=) id @@ api.get_t2mi_structures () with
      | None   -> `String "not found" |> Result.fail |> respond_result
      | Some s -> structure_to_yojson s |> Result.return |> respond_result

    let sequence (api:api) id seconds _ _ () =
      api.get_t2mi_seq seconds
      >|= (fun x -> List.filter (fun (x:sequence_item) -> id = x.stream_id) x
                    |> sequence_to_yojson
                    |> Result.return)
      >>= respond_result

    module Archive = struct

      open Board_types.Streams.T2MI

      let state id filter limit compress from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()

      let structure id limit from till duration _ _ () =
        respond_error ~status:`Not_implemented "FIXME" ()

    end

  end

end

let ts_handler (api:api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "ts"
    [ create_ws_handler ~docstring:"Pushes selected stream to the client"
        ~path:Path.Format.(Int32 ^/ empty)
        ~query:Query.empty
        (WS.TS.stream events)
    ; create_ws_handler ~docstring:"Pushes stream state to the client"
        ~path:Path.Format.(Int32 ^/ "state" @/ empty)
        ~query:Query.empty
        (WS.TS.state events)
    ; create_ws_handler ~docstring:"Pushes stream bitrate to the client"
        ~path:Path.Format.(Int32 ^/ "bitrate" @/ empty)
        ~query:Query.empty
        (WS.TS.bitrate events)
    ; create_ws_handler ~docstring:"Pushes stream structure to the client"
        ~path:Path.Format.(Int32 ^/ "structure" @/ empty)
        ~query:Query.empty
        (WS.TS.structure events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns stream info"
                ~path:Path.Format.(Int32 ^/ empty)
                ~query:Query.empty
                (HTTP.TS.stream events)
            ; create_handler ~docstring:"Returns stream state"
                ~path:Path.Format.(Int32 ^/ "state" @/ empty)
                ~query:Query.empty
                (HTTP.TS.state api)
            ; create_handler ~docstring:"Returns stream bitrate"
                ~path:Path.Format.(Int32 ^/ "bitrate" @/ empty)
                ~query:Query.empty
                (HTTP.TS.bitrate api)
            ; create_handler ~docstring:"Returns stream structure"
                ~path:Path.Format.(Int32 ^/ "structure" @/ empty)
                ~query:Query.empty
                (HTTP.TS.structure api)
            ; create_handler ~docstring:"Returns SI/PSI table section"
                ~path:Path.Format.(Int32 ^/ "section" @/ Int ^/ empty)
                ~query:Query.[ "section",        (module Option(Int))
                             ; "table-id-ext",   (module Option(Int))
                             ; "eit-ts-id",      (module Option(Int))
                             ; "eit-orig-nw-id", (module Option(Int)) ]
                (HTTP.TS.si_psi_section api)
            (* Archive *)
            ; create_handler ~docstring:"Returns archived streams"
                ~path:Path.Format.(Int32 ^/ "archive" @/ empty)
                ~query:Query.[ "limit",    (module Option(Int))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.TS.Archive.stream
            ; create_handler ~docstring:"Retunrs archived stream state"
                ~path:Path.Format.(Int32 ^/ "state/archive" @/ empty)
                ~query:Query.[ "filter",   (module Option(Bool))
                             ; "limit",    (module Option(Int))
                             ; "compress", (module Option(Bool))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.TS.Archive.state;
            create_handler ~docstring:"Retunrs archived stream bitrate"
              ~path:Path.Format.(Int32 ^/ "bitrate/archive" @/ empty)
              ~query:Query.[ "limit",    (module Option(Int))
                           ; "compress", (module Option(Bool))
                           ; "from",     (module Option(Time.Show))
                           ; "to",       (module Option(Time.Show))
                           ; "duration", (module Option(Time.Relative)) ]
              HTTP.TS.Archive.bitrate;
            create_handler ~docstring:"Retunrs archived stream structure"
              ~path:Path.Format.(Int32 ^/ "structure/archive" @/ empty)
              ~query:Query.[ "limit",    (module Option(Int))
                           ; "from",     (module Option(Time.Show))
                           ; "to",       (module Option(Time.Show))
                           ; "duration", (module Option(Time.Relative)) ]
              HTTP.TS.Archive.structure
            ]
    ]

let t2mi_handler (api:api) events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "t2mi"
    [ create_ws_handler ~docstring:"Pushes stream state to the client"
        ~path:Path.Format.(Int ^/ "state" @/ empty)
        ~query:Query.empty
        (WS.T2MI.state events)
    ; create_ws_handler ~docstring:"Pushes stream structure to the client"
        ~path:Path.Format.(Int ^/ "structure" @/ empty)
        ~query:Query.empty
        (WS.T2MI.structure events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns stream state"
                ~path:Path.Format.(Int ^/ "state" @/ empty)
                ~query:Query.empty
                (HTTP.T2MI.state api)
            ; create_handler ~docstring:"Returns T2-MI stream structure (L1 signalling)"
                ~path:Path.Format.(Int ^/ "structure" @/ empty)
                ~query:Query.empty
                (HTTP.T2MI.structure api)
            ; create_handler ~docstring:"Returns T2-MI packet sequence"
                ~path:Path.Format.(Int ^/ "sequence" @/ empty)
                ~query:Query.[ "duration", (module Option(Int)) ]
                (HTTP.T2MI.sequence api)
            (* Archive *)
            ; create_handler ~docstring:"Returns archived stream state"
                ~path:Path.Format.(Int ^/ "state/archive" @/ empty)
                ~query:Query.[ "filter",   (module Option(Bool))
                             ; "limit",    (module Option(Int))
                             ; "compress", (module Option(Bool))
                             ; "from",     (module Option(Time.Show))
                             ; "to",       (module Option(Time.Show))
                             ; "duration", (module Option(Time.Relative)) ]
                HTTP.T2MI.Archive.state;
            create_handler ~docstring:"Returns archived stream structure"
              ~path:Path.Format.(Int ^/ "structure/archive" @/ empty)
              ~query:Query.[ "limit",    (module Option(Int))
                           ; "from",     (module Option(Time.Show))
                           ; "to",       (module Option(Time.Show))
                           ; "duration", (module Option(Time.Relative)) ]
              HTTP.T2MI.Archive.structure
            ]
    ]

let handlers api events =
  [ ts_handler api events
  ; t2mi_handler api events
  ]