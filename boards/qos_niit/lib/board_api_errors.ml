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
      let res = Uri.Query.(parse_query ["id", (module Option(Caml.Int32))]
                                       (fun x -> CCOpt.map Stream.id_of_int32 x) query) in
      match res with
      | Ok id ->
         let open Errors.TS in
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
      let res = Uri.Query.(parse_query ["id", (module Option(Int))] (fun x -> x) query) in
      match res with
      | Ok id ->
         let open Errors.T2MI in
         let e = match id with
           | Some id -> let map = List.filter (fun (x:t) -> Int.equal id x.stream_id) in
                        React.E.map map events.t2mi_errors
                        |> React.E.filter Fun.(not % List.is_empty)
           | None    -> events.t2mi_errors
         in sock_handler sock_data e t_list_to_yojson body
      | Error e -> respond_error (Uri.Query.err_to_string e) ()

  end

end

let rest_now_ni = "This REST real-time REQ is not implemented"

module REST = struct

  module TS = struct

    module AR = struct

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
      | Ok (Some t,q) -> AR.errors t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

    let percent query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> AR.percent t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

    let has_any query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> AR.has_any t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()
  end

  module T2MI = struct

    module AR = struct

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
      | Ok (Some t,q) -> AR.errors t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

    let percent query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> AR.percent t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

    let has_any query () =
      match Api.Query.Time.get' query with
      | Ok (None,_)   -> not_implemented rest_now_ni ()
      | Ok (Some t,q) -> AR.has_any t q ()
      | Error e       -> respond_error (Uri.Query.err_to_string e) ()

  end
end

let ts_handler events _ meth ({path;query;_}:Uri.sep) sock_data headers body =
  match Api.Headers.is_ws headers,meth,Uri.Path.to_string path with
  (* WS *)
  | true, `GET,""        -> WS.TS.errors sock_data events body query ()
  (* REST *)
  | false,`GET,""        -> REST.TS.errors query ()
  | false,`GET,"percent" -> REST.TS.percent query ()
  | false,`GET,"has-any" -> REST.TS.has_any query ()
  | _                    -> not_found ()

let t2mi_handler events _ meth ({path;query;_}:Uri.sep) sock_data headers body =
  match Api.Headers.is_ws headers,meth,Uri.Path.to_string path with
  (* WS *)
  | true, `GET,""        -> WS.T2MI.errors sock_data events body query ()
  (* REST *)
  | false,`GET,""        -> REST.T2MI.errors query ()
  | false,`GET,"percent" -> REST.T2MI.percent query ()
  | false,`GET,"has-any" -> REST.T2MI.has_any query ()
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
