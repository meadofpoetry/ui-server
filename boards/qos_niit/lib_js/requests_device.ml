open Containers
open Board_types
open Api_js.Requests.Json
open Lwt.Infix
open Common

open Api_utils.Device

let path_to_uri ?(uri=Uri.empty) control path : Uri.t =
  let path = ["api";"board";string_of_int control] @ path
             |> String.concat "/" in
  Uri.with_path uri path

module WS = struct

  let get_state control =
    let uri = path_to_uri control @@ req_to_path `State in
    WS.get (Uri.to_string uri) Common.Topology.state_of_yojson

  let get_status control =
    let uri = path_to_uri control @@ req_to_path `Status in
    WS.get (Uri.to_string uri) status_of_yojson

  let get_errors control =
    let uri = path_to_uri control @@ req_to_path `Errors in
    WS.get (Uri.to_string uri) board_errors_of_yojson

  let get_t2mi_mode control =
    let uri = path_to_uri control @@ req_to_path (`Mode `T2MI) in
    WS.get (Uri.to_string uri) t2mi_mode_request_of_yojson

  let get_jitter_mode control =
    let uri = path_to_uri control @@ req_to_path (`Mode `JITTER) in
    WS.get (Uri.to_string uri) jitter_mode_request_of_yojson

end

module REST = struct

  (** Resets the board **)
  let post_reset control =
    let uri = path_to_uri control @@ req_to_path `Reset in
    post_result_unit (Uri.to_string uri)

  (** Sets T2-MI analysis settings **)
  let post_t2mi_mode control mode =
    let uri      = path_to_uri control @@ req_to_path (`Mode `T2MI) in
    let contents = t2mi_mode_request_to_yojson mode in
    post_result_unit ~contents (Uri.to_string uri)

  (** Sets jitter measurements settings **)
  let post_jitter_mode control mode =
    let uri      = path_to_uri control @@ req_to_path (`Mode `JITTER) in
    let contents = jitter_mode_request_to_yojson mode in
    post_result_unit ~contents (Uri.to_string uri)

  (** Sets board port to listen **)
  let post_port control port state =
    let uri = path_to_uri control @@ req_to_path (`Port (port,state)) in
    post_result_unit (Uri.to_string uri)

  module RT = struct

    let get_state control =
      let uri = path_to_uri control @@ req_to_path `State in
      get_result Common.Topology.state_of_yojson (Uri.to_string uri)

    let get_devinfo control =
      let uri = path_to_uri control @@ req_to_path `Info in
      get_result devinfo_of_yojson (Uri.to_string uri)

    let get_t2mi_mode control =
      let uri = path_to_uri control @@ req_to_path (`Mode `T2MI) in
      get_result t2mi_mode_request_of_yojson (Uri.to_string uri)

    let get_jitter_mode control =
      let uri = path_to_uri control @@ req_to_path (`Mode `JITTER) in
      get_result jitter_mode_request_of_yojson (Uri.to_string uri)

  end

  module AR = struct

    let ( >>= ) (u,x) f = match x with
      | Some x -> f (u,x)
      | None   -> u,None

    let get_state ?filter ?limit ?total time control =
      let uri = (Uri.empty,filter)
                >>= (fun (u,fil) -> set_state_query fil u, limit)
                >>= (fun (u,lim) -> set_limit_query lim u, total)
                >>= (fun (u,tot) -> set_total_query tot u, None)
                |>  (fun (u,_)   -> set_time_query time u)
                |>  (fun uri     -> path_to_uri ~uri control @@ req_to_path `State)
      in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    let get_status ?limit ?total time control =
      let uri = (Uri.empty,limit)
                >>= (fun (u,lim) -> set_limit_query lim u, total)
                >>= (fun (u,tot) -> set_total_query tot u, None)
                |>  (fun (u,_)   -> set_time_query time u)
                |>  (fun uri     -> path_to_uri ~uri control @@ req_to_path `Status)
      in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    let get_errors ?filter ?limit ?thin ?total time control =
      let uri = (Uri.empty,filter)
                >>= (fun (u,fil) -> set_errors_query fil u, limit)
                >>= (fun (u,lim) -> set_limit_query  lim u, total)
                >>= (fun (u,tot) -> set_total_query  tot u, thin)
                >>= (fun (u,thn) -> set_thin_query   thn u, None)
                |>  (fun (u,_)   -> set_time_query time u)
                |>  (fun uri     -> path_to_uri ~uri control @@ req_to_path `Errors)
      in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

  end

end
