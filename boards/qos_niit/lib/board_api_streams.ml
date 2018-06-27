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

    let streams (events:events) ids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let e = match ids with
        | [] -> React.S.changes events.streams
        | l  -> React.E.fmap (fun streams ->
                    List.filter (fun (s:Stream.t) -> match s.id with
                                                     | `Ts id -> List.mem ~eq:(Stream.equal_id) id l
                                                     | _      -> false) streams
                    |> function [] -> None | l -> Some l) (React.S.changes events.streams)
      in sock_handler sock_data e (Json.List.to_yojson Stream.to_yojson) body

    let state (events:events) ids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let e   = match ids with
        | [] -> events.ts_states
        | l  -> React.E.fmap (fun states ->
                    List.filter (fun (s:state) -> List.mem ~eq:(Stream.equal_id) s.stream l) states
                    |> function [] -> None | l -> Some l) events.ts_states
      in sock_handler sock_data e (Json.List.to_yojson state_to_yojson) body

    let bitrate (events:events) ids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let e   = match ids with
        | [] -> React.S.changes events.ts_bitrates
        | l  -> React.E.fmap (fun bitrates ->
                    List.filter (fun (b:bitrate) -> List.mem ~eq:(Stream.equal_id) b.stream l) bitrates
                    |> function [] -> None | l -> Some l)
                @@ React.S.changes events.ts_bitrates
      in sock_handler sock_data e (Json.List.to_yojson bitrate_to_yojson) body

    let structure (events:events) ids _ body sock_data () =
      let ids = List.map Stream.id_of_int32 ids in
      let e   = match ids with
        | [] -> React.S.changes events.ts_structures
        | l  -> React.E.fmap (fun structures ->
                    List.filter (fun (s:structure) -> List.mem ~eq:(Stream.equal_id) s.stream l) structures
                    |> function [] -> None | l -> Some l)
                @@ React.S.changes events.ts_structures
      in sock_handler sock_data e (Json.List.to_yojson structure_to_yojson) body

  end

  module T2MI = struct

    open Board_types.Streams.T2MI

    (* let fmap f_id f_time stream e = fmap ~eq:Int.equal f_id f_time stream e *)

    (* let state sock_data (ev:events) body query () =
     *   match get_t2mi_id query with
     *   | Ok id ->
     *      let f e j = sock_handler sock_data e j body in
     *      let e     = ev.t2mi_states in
     *      (match id with
     *       | Some id -> let e = fmap (fun (x:state) -> x.stream_id) (fun x -> x.timestamp) id e in
     *                    f e state_to_yojson
     *       | None    -> f e states_to_yojson)
     *   | Error e -> respond_bad_query e
     * 
     * let structure sock_data (ev:events) body query () =
     *   match get_t2mi_id query with
     *   | Ok id ->
     *      let f e j = sock_handler sock_data e j body in
     *      let e     = React.S.changes ev.t2mi_structures in
     *      (match id with
     *       | Some id -> let e = fmap (fun (x:structure) -> x.stream_id) (fun x -> x.timestamp) id e in
     *                    f e structure_to_yojson
     *       | None    -> f e structures_to_yojson)
     *   | Error e -> respond_bad_query e *)

  end

end

module HTTP = struct

  module TS = struct

    open Board_types.Streams.TS

    let streams (events:events) ids _ _ () =
      let ids = List.map Stream.id_of_int32 ids in
      let streams = match ids with
        | [] -> React.S.value events.streams
        | l  -> List.filter (fun (s:Stream.t) -> match s.id with
                                                 | `Ts id -> List.mem ~eq:Stream.equal_id id l
                                                 | _      -> false)
                @@ React.S.value events.streams
      in (Json.List.to_yojson Stream.to_yojson) streams |> Result.return |> respond_result

    let bitrate (events:events) ids _ _ () =
      let ids = List.map Stream.id_of_int32 ids in
      let bitrates = match ids with
        | [] -> React.S.value events.ts_bitrates
        | l  -> List.filter (fun (b:bitrate) -> List.mem ~eq:Stream.equal_id b.stream l)
                @@ React.S.value events.ts_bitrates
      in (Json.List.to_yojson bitrate_to_yojson) bitrates |> Result.return |> respond_result

    let structure (events:events) ids _ _ () =
      let ids = List.map Stream.id_of_int32 ids in
      let structs = match ids with
        | [] -> React.S.value events.ts_structures
        | l  -> List.filter (fun (s:structure) -> List.mem ~eq:Stream.equal_id s.stream l)
                @@ React.S.value events.ts_structures
      in (Json.List.to_yojson structure_to_yojson) structs |> Result.return |> respond_result

    let si_psi_section (api:api) stream table_id section table_id_ext
                       eit_ts_id eit_orig_nw_id _ _ () =
      let stream_id = Stream.id_of_int32 stream in
      let req = { stream_id; table_id; section; table_id_ext; eit_ts_id; eit_orig_nw_id } in
      api.get_section req
      >|= (function Ok x    -> Ok    (section_to_yojson x)
                  | Error e -> Error (section_error_to_yojson e))
      >>= respond_result

    module Archive = struct

      open Api.Query.Collection

      let streams (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "limit",  (module Option(Int))
                              ; "total",  (module Option(Bool))
                              ; "id",     (module Option(Api.Query.Stream.Show)) ]
                              (fun l t s -> l,t,s) query
        in
        match res with
        | Ok (limit,total,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

      let state (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "state", (module Option(Bool))
                              ; "limit", (module Option(Int))
                              ; "total", (module Option(Bool))
                              ; "thin",  (module Option(Bool))
                              ; "id",    (module Option(Api.Query.Stream.Show)) ]
                              (fun f l t thn s -> f,l,t,thn,s) query
        in
        match res with
        | Ok (fltr,limit,total,thin,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

      let structure (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "limit", (module Option(Int))
                              ; "total", (module Option(Bool))
                              ; "id",    (module Option(Api.Query.Stream.Show)) ]
                              (fun l t s -> l,t,s) query
        in
        match res with
        | Ok (limit,total,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

      let bitrate (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "limit",  (module Option(Int))
                              ; "total",  (module Option(Bool))
                              ; "thin",   (module Option(Bool))
                              ; "id",     (module Option(Api.Query.Stream.Show)) ]
                              (fun l t thn s -> l,t,thn,s) query
        in
        match res with
        | Ok (limit,total,thin,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

    end

  end

  module T2MI = struct

    open Board_types.Streams.T2MI

    (* let structure_now (e:events) query () =
     *   match get_t2mi_id query with
     *   | Ok id ->
     *      let v = React.S.value e.t2mi_structures in
     *      (match id with
     *       | Some id -> List.find_opt (fun (x:structure) -> Int.equal id x.stream_id) v
     *                    |> structure_opt_to_yojson
     *       | None    -> structures_to_yojson v)
     *      |> Result.return
     *      |> respond_result
     *   | Error e -> respond_bad_query e *)

    let sequence_now (api:api) (query:Uri.Query.t) () =
      let res = let open Uri.Query in
                parse_query [ "seconds", (module Option(Int))
                            ; "id",      (module Option(Int)) ] (fun s id -> s,id) query
      in
      match res with
      | Ok (s,id) ->
         api.get_t2mi_seq (Option.get_or ~default:5 s)
         >|= (fun x -> let seq = match id with
                         | Some id -> List.filter (fun (x:sequence_item) -> id = x.stream_id) x
                         | None    -> x
                       in Ok (sequence_to_yojson seq))
         >>= respond_result
      | Error e -> respond_bad_query e

    module Archive = struct

      open Board_types.Streams.T2MI

      let state (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "state", (module Option(Bool))
                              ; "limit", (module Option(Int))
                              ; "total", (module Option(Bool))
                              ; "thin",  (module Option(Bool))
                              ; "id",    (module Option(Api.Query.Stream.Show)) ]
                              (fun f l t thn s -> f,l,t,thn,s) query
        in
        match res with
        | Ok (fltr,limit,total,thin,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

      let structure (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "limit", (module Option(Int))
                              ; "total", (module Option(Bool))
                              ; "id",    (module Option(Api.Query.Stream.Show)) ]
                              (fun l t s -> l,t,s) query
        in
        match res with
        | Ok (limit,total,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

    end

  end

end

let ts_handler api events =
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
            ; create_handler ~docstring:"Returns current TS bitrate"
                             ~path:Path.Format.("bitrate" @/ empty)
                             ~query:Query.[ "id", (module List(Int32)) ]
                             (HTTP.TS.bitrate events)
            ; create_handler ~docstring:"Returns current TS structure"
                             ~path:Path.Format.("structure" @/ empty)
                             ~query:Query.[ "id", (module List(Int32)) ]
                             (HTTP.TS.structure events)
            ; create_handler ~docstring:"Returns requested SI/PSI table content"
                             ~path:Path.Format.("section" @/ Int32 ^/ Int ^/ empty)
                             ~query:Query.[ "section",        (module Option(Int))
                                          ; "table-id-ext",   (module Option(Int))
                                          ; "eit-ts-id",      (module Option(Int))
                                          ; "eit-orig-nw-id", (module Option(Int)) ]
                             (HTTP.TS.si_psi_section api)
            ]
    ]

let t2mi_handler api events ({scheme;path;query}:Uri.uri) _ meth _ body sock_data =
  match Uri.Scheme.is_ws scheme,meth,path with
  (* WS *)
  (* | true, `GET,["state"]     -> WS.T2MI.state sock_data events body query ()
   * | true, `GET,["structure"] -> WS.T2MI.structure sock_data events body query ()
   * (\* HTTP *\)
   * | false,`GET,["state"]     -> HTTP.T2MI.state query ()
   * | false,`GET,["structure"] -> HTTP.T2MI.structure events query ()
   * | false,`GET,["sequence"]  -> HTTP.T2MI.sequence api query () *)
  | _ -> not_found ()

let handlers api events =
  [ ts_handler api events
  ; (module struct
       let domain = "t2mi"
       let handle = t2mi_handler api events
     end : Api_handler.HANDLER)
  ]
