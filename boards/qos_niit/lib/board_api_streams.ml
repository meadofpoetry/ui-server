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
 ** BOTH GET  /streams/{?stream}
 ** BOTH GET  /streams/input/{?stream}
 ** BOTH GET  /streams/states/[ts|t2mi]/{?stream}
 ** BOTH GET  /streams/structures/[ts|t2mi]/{?stream}
 ** BOTH GET  /streams/bitrates/ts/{?stream}
 ** REST GET  /streams/sequence/t2mi
 ** REST GET  /streams/section/ts/{stream}/{table-id}/?[section; table-id-ext; eit-ts-id; eit-orig-nw-id]
 **
 ** QUERY PARAMETERS
 **
 ** [from]      - timestamp (can be 'now', 'now' - timespan)
 ** [to]        - timestamp (can be 'now')
 ** [f[errors]] - list of error codes to be filtered
 ** [f[level]]  - level of errors to be filtered.
 **               Priority for TS; TS parser, T2-MI parser or T2-MI errors for T2-MI
 ** [limit]     - maximum number of items in a response (default FIXME)
 ** [total]     - include [total] value into response to know how many collection items are available
 ** [decimate]  - if true, decimate the number of items in a collection  (e.g. for charts).
 **               Possible values: 'off',FIXME mention available decimation algorithms here
 **
 **)

include Api_utils.Streams

module WS = struct

  let streams sock_data (events:events) body =
    sock_handler sock_data (React.S.changes events.streams) Stream.t_list_to_yojson body

  let input_streams sock_data (events:events) body =
    sock_handler sock_data (React.S.changes events.input_streams) Stream.t_list_to_yojson body

  let ts_bitrates sock_data (events:events) body =
    sock_handler sock_data (React.S.changes events.bitrates) Streams.TS.structures_to_yojson body

  let ts_structures sock_data (events:events) body =
    sock_handler sock_data (React.S.changes events.structs) Streams.TS.structures_to_yojson body

  let t2mi_structures sock_data (events:events) body =
    sock_handler sock_data events.t2mi_info Streams.T2MI.structures_to_yojson body

end

module REST = struct

  (** Real-time GET requests **)
  module RT = struct

    let input_streams (events:events) () =
      React.S.value events.input_streams
      |> Common.Stream.t_list_to_yojson
      |> Result.return
      |> Json.respond_result

    let ts_structures (e:events) () =
      let v = React.S.value e.structs in
      Json.respond_result @@ Ok (Streams.TS.structures_to_yojson v)

    let ts_bitrates (e:events) () =
      let v = React.S.value e.bitrates in
      Json.respond_result @@ Ok (Streams.TS.structures_to_yojson v)

    let si_psi_section ?section ?table_id_ext ?eit_ts_id ?eit_orig_nw_id
                       stream_id table_id (api:api) () =
      let req = { stream_id; table_id; section; table_id_ext; eit_ts_id; eit_orig_nw_id } in
      api.get_section req >|= (function
                               | Ok x    -> Ok (section_to_yojson x)
                               | Error e -> Error (section_error_to_yojson e))
      >>= Json.respond_result

    let t2mi_sequence ?seconds ?stream_id api () =
      let open Streams.T2MI in
      let sec = Option.get_or ~default:5 seconds in
      api.get_t2mi_seq sec
      >|= (fun x -> let seq = match stream_id with
                      | Some id -> List.filter (fun (x:sequence_item) -> id = x.stream_id) x
                      | None    -> x
                    in Ok (sequence_to_yojson seq))
      >>= Json.respond_result

  end

  (** Archive GET requests **)
  module AR = struct


  end

end

let handle api events scheme meth req uri sock_data body () = match scheme,meth,req with
  (* Websockets *)
  (* NOTE queries are ignored for websocket requests at the moment *)
  | `WS,`GET, `Streams (`All,id)   -> not_found ()
  | `WS,`GET, `Streams (`Input,id) -> not_found ()
  | `WS,`GET, `States x            -> not_found ()
  | `WS,`GET, `Structures x        -> not_found ()
  (* Restful GET *)
  | _  ,`GET, `Streams x           -> not_found ()
  | _  ,`GET, `States x            -> not_found ()
  | _  ,`GET, `Structures x        -> not_found ()
  | _  ,`GET, `Sequence id         -> REST.RT.t2mi_sequence ?stream_id:id api ()
  | _  ,`GET, `Section (sid,id)    -> REST.RT.si_psi_section sid id api ()
  | _ -> not_found ()
