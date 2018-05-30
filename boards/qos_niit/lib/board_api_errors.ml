open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect
open Api.Query

(**
 ** API
 **
 ** GET  /errors/[ts|t2mi]/{?stream}
 ** GET  /errors/percent/[ts|t2mi]/{?stream}
 ** GET  /errors/has-any/[ts|t2mi]/{?stream}
 **
 ** QUERY PARAMETERS
 **
 ** [from][to]  - timestamps
 ** [f[errors]] - list of error codes to be filtered
 ** [f[level]]  - level of errors to be filtered. Priority for TS, ts-parser/t2mi-parser/errors for T2-MI
 ** [limit]     - maximum number of items in a response (default FIXME)
 ** [total]     - include [total] value into response to know how many collection items are available
 ** [thin]      - if true, decimate the number of items in a collection  (e.g. for charts)
 **
 **)

include Api_utils.Errors

let bad_request     = respond_error ~status:`Bad_request
let not_implemented = respond_error ~status:`Not_implemented

module WS = struct

  module TS = struct

    let errors ?stream sock_data (events:events) body () =
      let open Errors.TS in
      let e = match stream with
        | Some id -> let map = List.filter (fun (x:t) -> Common.Stream.equal_id id x.stream) in
                     React.E.map map events.ts_errors
                     |> React.E.filter Fun.(not % List.is_empty)
        | None    -> events.ts_errors
      in sock_handler sock_data e t_list_to_yojson body

  end

  module T2MI = struct

    let errors ?stream sock_data (events:events) body () =
      let open Errors.T2MI in
      let e = match stream with
        | Some id -> let map = List.filter (fun (x:t) -> Int.equal id x.stream_id) in
                     React.E.map map events.t2mi_errors
                     |> React.E.filter Fun.(not % List.is_empty)
        | None    -> events.t2mi_errors
      in sock_handler sock_data e t_list_to_yojson body

  end

end

module REST = struct

  (** Archive GET requests **)
  module AR = struct

    module TS = struct

      let errors ?stream time (q:Raw.t list) () =
        let r,_ = Validation.(
            get_errors_query q
            >>= fun (err,q) -> get_level_query q
            >>= fun (lev,q) -> get_limit_query q
            >>= fun (lim,q) -> get_thin_query q
            >>= fun (thn,q) -> get_total_query q
            >>| fun tot     -> err,lev,lim,thn,tot)
        in (fun _ -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

      let percent ?stream time (q:Api.Query.Raw.t list) () =
        let r,_ = Validation.(
            get_errors_query q
            >>= fun (err,q) -> get_level_query q
            >>| Pair.make err)
        in (fun _ -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

      let has_any ?stream time (q:Api.Query.Raw.t list) () =
        let r,_ = Validation.(
            get_errors_query q
            >>= fun (err,q) -> get_level_query q
            >>| Pair.make err)
        in (fun _ -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

    end

    module T2MI = struct

      let errors ?stream time (q:Api.Query.Raw.t list) () =
        let r,_ = Validation.(
            get_errors_query q
            >>= fun (err,q) -> get_level_query q
            >>= fun (lev,q) -> get_limit_query q
            >>= fun (lim,q) -> get_thin_query q
            >>= fun (thn,q) -> get_total_query q
            >>| fun tot     -> err,lev,lim,thn,tot)
        in (fun _ -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

      let percent ?stream time (q:Api.Query.Raw.t list) () =
        let r,_ = Validation.(
            get_errors_query q
            >>= fun (err,q) -> get_level_query q
            >>| Pair.make err)
        in (fun _ -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

      let has_any ?stream time (q:Api.Query.Raw.t list) () =
        let r,_ = Validation.(
            get_errors_query q
            >>= fun (err,q) -> get_level_query q
            >>| Pair.make err)
        in (fun _ -> (* TODO IMPLEMENT *)
            respond_error ~status:`Not_implemented "not impelemented" ())
           |> query_wrapper r

    end
  end
end

let ws_past_ni  = "This WS archive REQ is not implemented"
let rest_now_ni = "This REST real-time REQ is not implemented"

let handle_ok api events scheme meth req (q:Raw.t list) sock_data body time () =
  match scheme,meth,req,time with
  (* WS *)
  | `WS,  `GET,`Errors (`TS id),   `Now    -> WS.TS.errors   ?stream:id sock_data events body ()
  | `WS,  `GET,`Errors (`T2MI id), `Now    -> WS.T2MI.errors ?stream:id sock_data events body ()
  | `WS,  `GET,_,                  `Past _ -> not_implemented ws_past_ni ()
  (* REST *)
  | `REST,`GET,_,                  `Now    -> not_implemented rest_now_ni ()
  | `REST,`GET,`Errors  (`TS id),  `Past t -> REST.AR.TS.errors    ?stream:id t q ()
  | `REST,`GET,`Percent (`TS id),  `Past t -> REST.AR.TS.percent   ?stream:id t q ()
  | `REST,`GET,`Has_any (`TS id),  `Past t -> REST.AR.TS.has_any   ?stream:id t q ()
  | `REST,`GET,`Errors  (`T2MI id),`Past t -> REST.AR.T2MI.errors  ?stream:id t q ()
  | `REST,`GET,`Percent (`T2MI id),`Past t -> REST.AR.T2MI.percent ?stream:id t q ()
  | `REST,`GET,`Has_any (`T2MI id),`Past t -> REST.AR.T2MI.has_any ?stream:id t q ()
  | _ -> not_found ()

let handle api events scheme meth req uri sock_data body () =
  match Validation.get_or ~default:`Now (Time ("from","to")) @@ Uri.query uri with
  | Error e,_ -> Json.respond_result (Error (Api_utils.err_to_yojson @@ Bad_query e))
  | Ok t,q    -> handle_ok api events scheme meth req q sock_data body t ()
