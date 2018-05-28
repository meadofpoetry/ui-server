open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect
open Common

(**
 ** API
 **
 ** GET  /errors/[ts|t2mi]/{?stream}
 ** GET  /errors/segmentation/[ts|t2mi]/{?stream}
 ** GET  /errors/has-errors/[ts|t2mi]/{?stream}
 **
 ** QUERY PARAMETERS
 **
 ** [from]      - timestamp (can be 'now', 'now' - timespan)
 ** [to]        - timestamp (can be 'now')
 ** [f[errors]] - list of error codes to be filtered
 ** [f[level]]  - level of errors to be filtered.
 **               Priority for TS; TS parser, T2-MI parser or T2-MI errors for T2-MI
 ** [limit] - maximum number of items in a response (default FIXME)
 ** [total] - include [total] value into response to know how many collection items are available
 ** [thin]  - if true, decimate the number of items in a collection  (e.g. for charts)
 **
 **)

include Api_utils.Errors

let bad_request     = respond_error ~status:`Bad_request
let not_implemented = respond_error ~status:`Not_implemented

module WS = struct

  let ts_errors ?stream sock_data (events:events) body () =
    let open Errors.TS in
    let e = match stream with
      | Some id -> let map = List.filter (fun (x:t) -> Stream.equal_id id x.stream) in
                   React.E.map map events.ts_errors
                   |> React.E.filter Fun.(not % List.is_empty)
      | None    -> events.ts_errors
    in sock_handler sock_data e t_list_to_yojson body

  let t2mi_errors ?stream sock_data (events:events) body () =
    let open Errors.T2MI in
    let e = match stream with
      | Some id -> let map = List.filter (fun (x:t) -> Int.equal id x.stream_id) in
                   React.E.map map events.t2mi_errors
                   |> React.E.filter Fun.(not % List.is_empty)
      | None    -> events.t2mi_errors
    in sock_handler sock_data e t_list_to_yojson body

end

module REST = struct

  (** Archive GET requests **)
  module AR = struct

    module TS = struct

      (** FIXME implement **)
      let errors ?stream time () =
        not_implemented "ts errors archive" ()

      let errors_compressed ?stream time () =
        not_implemented "ts errors archive compressed" ()

      (** FIXME implement **)
      let segmentation ?stream  time () =
        not_implemented "ts segmentation archive" ()

      (** FIXME implement **)
      let has_errors ?stream  time () =
        not_implemented "ts has errors arhcive" ()

    end

    module T2MI = struct

      (** FIXME implement **)
      let errors ?stream time () =
        not_implemented "t2mi errors archive" ()

      let errors_compressed ?stream time () =
        not_implemented "t2mi errors archive compressed" ()

      (** FIXME implement **)
      let segmentation ?stream time () =
        not_implemented "t2mi segmentation archive" ()

      (** FIXME implement **)
      let has_errors ?stream time () =
        not_implemented "t2mi has errors arhcive" ()

    end

  end

end

let ws_past_ni  = "WS archive REQ is not implemented"
let rest_now_ni = "REST real-time REQ is not implemented"

let handle api events scheme meth req uri sock_data body () =
  let open Api.Query in
  let q        = Uri.query uri in
  let from,q   = Validation.get (One ("from", Time)) q in
  let till,q   = Validation.get (One ("to", Time)) q   in
  let thin,q   = Validation.get (One ("thin", Bool)) q in
  let total,q  = Validation.get (One ("total", Bool)) q in
  let limit,q  = Validation.get (One ("limit", Int)) q in
  let level,q  = Validation.get (Filter ("level", Int)) q in
  let errors,q = Validation.get (Filter ("errors", Int)) q in
  let time     = Time.Range.of_time ~from ~till in
  match scheme,meth,req,time with
  (* Websockets real-time *)
  | `WS,`GET,`Errors (`TS id),  `Now -> WS.ts_errors   ?stream:id sock_data events body ()
  | `WS,`GET,`Errors (`T2MI id),`Now -> WS.t2mi_errors ?stream:id sock_data events body ()
  (* Websockets archive *)
  | `WS,  `GET,_,`Past _ -> not_implemented ws_past_ni ()
  (* RESTful GET real-time *)
  | `REST,`GET,_,`Now    -> not_implemented rest_now_ni ()
  (* RESTful GET archive*)
  | `REST,`GET,`Errors       (`TS id),  `Past t ->
     (match thin with
      | Some true -> REST.AR.TS.errors_compressed ?stream:id t ()
      | _         -> REST.AR.TS.errors ?stream:id t ())
  | `REST,`GET,`Segmentation (`TS id),  `Past t -> REST.AR.TS.segmentation ?stream:id t ()
  | `REST,`GET,`Has_errors   (`TS id),  `Past t -> REST.AR.TS.has_errors ?stream:id t ()
  | `REST,`GET,`Errors       (`T2MI id),`Past t ->
     (match thin with
      | Some true -> REST.AR.T2MI.errors_compressed ?stream:id t ()
      | _         -> REST.AR.T2MI.errors ?stream:id t ())
  | `REST,`GET,`Segmentation (`T2MI id),`Past t -> REST.AR.T2MI.segmentation ?stream:id t ()
  | `REST,`GET,`Has_errors   (`T2MI id),`Past t -> REST.AR.T2MI.has_errors ?stream:id t ()
  | _ -> not_found ()
