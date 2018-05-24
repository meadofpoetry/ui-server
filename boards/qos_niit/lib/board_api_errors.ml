open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect
open Common

include Api_utils.Errors

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

end

let handle api events scheme meth req uri sock_data body () = match scheme,meth,req with
  (* Websockets *)
  (* NOTE queries are ignored for websocket requests at the moment *)
  | `WS,`GET, `Errors (`TS id)   -> WS.ts_errors   ?stream:id sock_data events body ()
  | `WS,`GET, `Errors (`T2MI id) -> WS.t2mi_errors ?stream:id sock_data events body ()
  (* Restful GET *)
  | _  ,`GET, `Errors _          -> not_found () (* FIXME implement *)
  | _  ,`GET, `Segmentation _    -> not_found () (* FIXME implement *)
  | _  ,`GET, `Has_errors _      -> not_found () (* FIXME implement *)
  | _ -> not_found ()
