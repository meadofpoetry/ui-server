open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(** API
    GET /errors/[ts|t2mi]
    GET /errors/[ts|t2mi]/percent
    GET /errors/[ts|t2mi]/has-any *)

type events = errors_events

let not_implemented = respond_error ~status:`Not_implemented

module WS = struct

  module TS = struct

    let errors sock_data (events:events) body query () =
      let res = Api.Query.Stream.get query in
      match res with
      | Ok id ->
         let open Errors in
         let e = match id with
           | Some id -> let map = List.filter (fun (x:t) -> Common.Stream.equal_id id x.stream) in
                        React.E.map map events.ts_errors
                        |> React.E.filter Fun.(not % List.is_empty)
           | None    -> events.ts_errors
         in sock_handler sock_data e t_list_to_yojson body
      | Error e -> respond_error (Uri.Query.err_to_string e) ()

  end

  module T2MI = struct

    let errors sock_data (events:events) body query () =
      let res = Api.Query.Stream.get' query
                |> Result.flat_map (fun (id,query) ->
                       Uri.Query.(parse_query ["t2mi-stream-id", (module Option(Int))]
                                              (fun x -> id,x) query))
      in
      match res with
      | Ok (stream_id,t2mi_stream_id) ->
         let open Errors in
         let e = match stream_id,t2mi_stream_id with
           | Some sid,Some tid ->
              let map = List.filter (fun (x:t) -> Stream.equal_id sid x.stream
                                                  && Int.equal tid (Int32.to_int x.param_2)) in
              React.E.map map events.t2mi_errors
              |> React.E.filter Fun.(not % List.is_empty)
           | Some sid,None ->
              let map = List.filter (fun (x:t) -> Stream.equal_id sid x.stream) in
              React.E.map map events.t2mi_errors
              |> React.E.filter Fun.(not % List.is_empty)
           | None, Some tid ->
              let map = List.filter (fun (x:t) -> Int.equal tid (Int32.to_int x.param_2)) in
              React.E.map map events.t2mi_errors
              |> React.E.filter Fun.(not % List.is_empty)
           | None,None -> events.t2mi_errors
         in sock_handler sock_data e t_list_to_yojson body
      | Error e -> respond_error (Uri.Query.err_to_string e) ()

  end

end

let rest_now_ni = "This REST real-time REQ is not implemented"

module HTTP = struct

  module TS = struct

    module Archive = struct

      let errors time query () =
        (* TODO IMPLEMENT *)
        not_found ()
      let percent time query () =
        (* TODO IMPLEMENT *)
        not_found ()
      let has_any time query () =
        (* TODO IMPLEMENT *)
        not_found ()

    end

    let errors query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> Archive.errors t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

    let percent query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> Archive.percent t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

    let has_any query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> Archive.has_any t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()
  end

  module T2MI = struct

    module Archive = struct

      let errors time query () =
        (* TODO IMPLEMENT *)
        not_found ()
      let percent time query () =
        (* TODO IMPLEMENT *)
        not_found ()
      let has_any time query () =
        (* TODO IMPLEMENT *)
        not_found ()

    end

    let errors query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> Archive.errors t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

    let percent query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> Archive.percent t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

    let has_any query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> Archive.has_any t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

  end
end

let ts_handler events _ meth ({path;query;_}:Uri.sep) sock_data headers body =
  match Api.Headers.is_ws headers,meth,Uri.Path.to_string path with
  (* WS *)
  | true, `GET,""        -> WS.TS.errors sock_data events body query ()
  (* HTTP *)
  | false,`GET,""        -> HTTP.TS.errors query ()
  | false,`GET,"percent" -> HTTP.TS.percent query ()
  | false,`GET,"has-any" -> HTTP.TS.has_any query ()
  | _                    -> not_found ()

let t2mi_handler events _ meth ({path;query;_}:Uri.sep) sock_data headers body =
  match Api.Headers.is_ws headers,meth,Uri.Path.to_string path with
  (* WS *)
  | true, `GET,""        -> WS.T2MI.errors sock_data events body query ()
  (* HTTP *)
  | false,`GET,""        -> HTTP.T2MI.errors query ()
  | false,`GET,"percent" -> HTTP.T2MI.percent query ()
  | false,`GET,"has-any" -> HTTP.T2MI.has_any query ()
  | _                    -> not_found ()

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
