open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(** API
    GET /streams/ts
    GET /streams/[ts|t2mi]/state
    GET /streams/[ts|t2mi]/structure
    GET /streams/ts/bitrate
    GET /streams/t2mi/sequence
    GET /streams/ts/section/{stream}/{table-id} *)

type events = streams_events

module Show_stream_id = struct
  type t = Stream.id
  let of_string s = Int32.of_string_opt s
                    |> Option.map Stream.id_of_int32
                    |> Option.get_exn
  let to_string x = Stream.id_to_int32 x |> Int32.to_string
end

let get_ts_id' q =
  Uri.Query.(parse_query' ["id", (module Option(Show_stream_id))] (fun x -> x) q)
let get_ts_id q =
  get_ts_id' q |> Result.map fst

let get_t2mi_id q =
  Uri.Query.(parse_query ["id", (module Option(Int))] (fun x -> x) q)

module WS = struct

  open Common

  let streams sock_data (events:events) body () =
    sock_handler sock_data (React.S.changes events.streams) Stream.t_list_to_yojson body

  let fmap ~(eq:'b -> 'b -> bool) f_id f_time stream (e:'a list React.event) =
    React.E.fmap (fun x ->
        List.filter (fun s -> eq stream (f_id s)) x
        |> (function
            | [] -> None
            | l  -> List.sort (fun x y -> Time.compare (f_time x) (f_time y)) l
                    |> List.rev |> List.head_opt)) e

  module TS = struct

    open Board_types.Streams.TS

    let fmap f_id f_time stream e = fmap ~eq:Common.Stream.equal_id f_id f_time stream e

    let state sock_data (ev:events) body query () =
      match get_ts_id query with
      | Ok id ->
         let f e j = sock_handler sock_data e j body in
         let e     = ev.ts_states in
         (match id with
          | Some id -> let e = fmap (fun (x:state) -> x.stream) (fun x -> x.timestamp) id e in
                       f e state_to_yojson
          | None    -> f e states_to_yojson)
      | Error e -> respond_bad_query e

    let bitrate sock_data (ev:events) body query () =
      match get_ts_id query with
      | Ok id ->
         let f e j = sock_handler sock_data e j body in
         let e     = React.S.changes ev.ts_bitrates in
         (match id with
          | Some id -> let e = fmap (fun x -> x.stream) (fun x -> x.timestamp) id e in
                       f e structure_to_yojson
          | None    -> f e structures_to_yojson)
      | Error e -> respond_bad_query e

    let structure sock_data (ev:events) body query () =
      match get_ts_id query with
      | Ok id ->
         let f e j = sock_handler sock_data e j body in
         let e     = React.S.changes ev.ts_structures in
         (match id with
          | Some id -> let e = fmap (fun x -> x.stream) (fun x -> x.timestamp) id e in
                       f e structure_to_yojson
          | None    -> f e structures_to_yojson)
      | Error e -> respond_bad_query e


  end

  module T2MI = struct

    open Board_types.Streams.T2MI

    let fmap f_id f_time stream e = fmap ~eq:Int.equal f_id f_time stream e

    let state sock_data (ev:events) body query () =
      match get_t2mi_id query with
      | Ok id ->
         let f e j = sock_handler sock_data e j body in
         let e     = ev.t2mi_states in
         (match id with
          | Some id -> let e = fmap (fun (x:state) -> x.stream_id) (fun x -> x.timestamp) id e in
                       f e state_to_yojson
          | None    -> f e states_to_yojson)
      | Error e -> respond_bad_query e

    let structure sock_data (ev:events) body query () =
      match get_t2mi_id query with
      | Ok id ->
         let f e j = sock_handler sock_data e j body in
         let e     = React.S.changes ev.t2mi_structures in
         (match id with
          | Some id -> let e = fmap (fun (x:structure) -> x.stream_id) (fun x -> x.timestamp) id e in
                       f e structure_to_yojson
          | None    -> f e structures_to_yojson)
      | Error e -> respond_bad_query e

  end

end

module REST = struct

  (** Real-time GET requests **)
  module RT = struct

    let to_json ?stream s =
      (match stream with
       | Some id -> List.find_opt (fun (x:Common.Stream.t) -> match x.id with
                                                              | `Ts x -> Common.Stream.equal_id id x
                                                              | _     -> false) s
                    |> Common.Stream.t_opt_to_yojson
       | None    -> Common.Stream.t_list_to_yojson s)
      |> Result.return
      |> Json.respond_result

    let streams (ev:events) query () =
      match get_ts_id query with
      | Ok id   -> let s = React.S.value ev.streams in to_json ?stream:id s
      | Error e -> respond_bad_query e

    module TS = struct

      open Board_types.Streams.TS

      let to_json ?stream s =
        (match stream with
         | Some id -> List.find_opt (fun (x:structure) -> Common.Stream.equal_id id x.stream) s
                      |> structure_opt_to_yojson
         | None    -> structures_to_yojson s)
        |> Result.return
        |> Json.respond_result

      let structure (ev:events) query () =
        match get_ts_id query with
        | Ok id   -> let v = React.S.value ev.ts_structures in to_json ?stream:id v
        | Error e -> respond_bad_query e

      let bitrate (ev:events) query () =
        match get_ts_id query with
        | Ok id   -> let v = React.S.value ev.ts_bitrates in to_json ?stream:id v
        | Error e -> respond_bad_query e

      let si_psi_section stream_id table_id (api:api) (q:Uri.Query.t) () =
        let f = fun s t ts nw -> { stream_id
                                 ; table_id
                                 ; section=s
                                 ; table_id_ext=t
                                 ; eit_ts_id=ts
                                 ; eit_orig_nw_id=nw
                                 }
        in
        let res = let open Uri.Query in
                  parse_query [ "section",        (module Option(Int))
                              ; "table_id_ext",   (module Option(Int))
                              ; "eit_ts_id",      (module Option(Int))
                              ; "eit_orig_nw_id", (module Option(Int))
                              ] f q
        in
        match res with
        | Ok x -> api.get_section x
                  >|= (function
                       | Ok x    -> Ok (section_to_yojson x)
                       | Error e -> Error (section_error_to_yojson e))
                  >>= Json.respond_result
        | Error e  -> respond_bad_query e

    end

    module T2MI = struct

      open Board_types.Streams.T2MI

      let structure (e:events) query () =
        match get_t2mi_id query with
        | Ok id ->
           let v = React.S.value e.t2mi_structures in
           (match id with
            | Some id -> List.find_opt (fun (x:structure) -> Int.equal id x.stream_id) v
                         |> structure_opt_to_yojson
            | None    -> structures_to_yojson v)
           |> Result.return
           |> Json.respond_result
        | Error e -> respond_bad_query e

      let sequence (api:api) (query:Uri.Query.t) () =
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
           >>= Json.respond_result
        | Error e -> respond_bad_query e

    end

  end

  (** Archive GET requests **)
  module AR = struct

    open Api.Query.Collection

    let streams (query:Uri.Query.t) time () =
      let res = let open Uri.Query in
                parse_query [ "limit",  (module Option(Int))
                            ; "total",  (module Option(Bool))
                            ; "id",     (module Option(Show_stream_id)) ]
                            (fun l t s -> l,t,s) query
      in
      match res with
      | Ok (limit,total,id) -> respond_error ~status:`Not_implemented "FIXME" ()
      | Error e -> respond_bad_query e


    module TS = struct

      open Board_types.Streams.TS

      let state ?stream (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "state", (module Option(Bool))
                              ; "limit", (module Option(Int))
                              ; "total", (module Option(Bool))
                              ; "thin",  (module Option(Bool))
                              ; "id",    (module Option(Show_stream_id)) ]
                              (fun f l t thn s -> f,l,t,thn,s) query
        in
        match res with
        | Ok (fltr,limit,total,thin,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

      let structure (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "limit", (module Option(Int))
                              ; "total", (module Option(Bool))
                              ; "id",    (module Option(Show_stream_id)) ]
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
                              ; "id",     (module Option(Show_stream_id)) ]
                              (fun l t thn s -> l,t,thn,s) query
        in
        match res with
        | Ok (limit,total,thin,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e
    end

    module T2MI = struct

      open Board_types.Streams.T2MI

      let state (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "state", (module Option(Bool))
                              ; "limit", (module Option(Int))
                              ; "total", (module Option(Bool))
                              ; "thin",  (module Option(Bool))
                              ; "id",    (module Option(Show_stream_id)) ]
                              (fun f l t thn s -> f,l,t,thn,s) query
        in
        match res with
        | Ok (fltr,limit,total,thin,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

      let structure (query:Uri.Query.t) time () =
        let res = let open Uri.Query in
                  parse_query [ "limit", (module Option(Int))
                              ; "total", (module Option(Bool))
                              ; "id",    (module Option(Show_stream_id)) ]
                              (fun l t s -> l,t,s) query
        in
        match res with
        | Ok (limit,total,id) -> respond_error ~status:`Not_implemented "FIXME" ()
        | Error e -> respond_bad_query e

    end

  end

end

let ts_handler events _ meth ({scheme;path;query}:Uri.sep) sock_data _ body =
  match Uri.Scheme.is_ws scheme,meth,Uri.Path.to_string path with
  | _ -> not_found ()

let t2mi_handler events _ meth ({scheme;path;query}:Uri.sep) sock_data _ body =
  match Uri.Scheme.is_ws scheme,meth,Uri.Path.to_string path with
  | _ -> not_found ()

let handlers events =
  [ (module struct
       let domain = "ts"
       let handle = ts_handler events
     end : Api_handler.HANDLER)
  ; (module struct
       let domain = "t2mi"
       let handle = t2mi_handler events
     end : Api_handler.HANDLER)
  ]
