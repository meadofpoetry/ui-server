open Containers
open Board_types
open Api_js.Requests.Json
open Requests_common
open Common

open Api_utils.Device

let req_to_uri ?uri control req = req_to_uri ?uri control (`Device req)

module WS = struct

  include Boards_js.Requests.Device.WS

  let get_status control =
    let uri = req_to_uri control `Status in
    WS.get (Uri.to_string uri) status_of_yojson

  let get_errors control =
    let uri = req_to_uri control `Errors in
    WS.get (Uri.to_string uri) board_errors_of_yojson

  let get_t2mi_mode control =
    let uri = req_to_uri control (`Mode `T2MI) in
    WS.get (Uri.to_string uri) t2mi_mode_request_of_yojson

  let get_jitter_mode control =
    let uri = req_to_uri control (`Mode `JITTER) in
    WS.get (Uri.to_string uri) jitter_mode_request_of_yojson

end

module REST = struct

  include (Boards_js.Requests.Device.REST:
           module type of Boards_js.Requests.Device.REST
                          with module RT := Boards_js.Requests.Device.REST.RT
                          with module AR := Boards_js.Requests.Device.REST.AR)

  (** Resets the board **)
  let post_reset control =
    let uri = req_to_uri control `Reset in
    post_result_unit (Uri.to_string uri)

  (** Sets T2-MI analysis settings **)
  let post_t2mi_mode control mode =
    let uri      = req_to_uri control (`Mode `T2MI) in
    let contents = t2mi_mode_request_to_yojson mode in
    post_result_unit ~contents (Uri.to_string uri)

  (** Sets jitter measurements settings **)
  let post_jitter_mode control mode =
    let uri      = req_to_uri control (`Mode `JITTER) in
    let contents = jitter_mode_request_to_yojson mode in
    post_result_unit ~contents (Uri.to_string uri)

  module RT = struct

    include Boards_js.Requests.Device.REST.RT

    let get_devinfo control =
      let uri = req_to_uri control `Info in
      get_result devinfo_of_yojson (Uri.to_string uri)

    let get_t2mi_mode control =
      let uri = req_to_uri control (`Mode `T2MI) in
      get_result t2mi_mode_request_of_yojson (Uri.to_string uri)

    let get_jitter_mode control =
      let uri = req_to_uri control (`Mode `JITTER) in
      get_result jitter_mode_request_of_yojson (Uri.to_string uri)

  end

  module AR = struct

    include Boards_js.Requests.Device.REST.AR

    let get_status ?limit ?total time control =
      let uri = Query.(
          (Uri.empty,limit)
          >>* (fun (u,lim) -> set limit_query lim u, total)
          >>* (fun (u,tot) -> set total_query tot u, None)
          |>  (fun (u,_)   -> set_time_query time u)
          |>  (fun uri     -> req_to_uri ~uri control `Status))
      in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

    let get_errors ?filter ?limit ?thin ?total time control =
      let uri = Query.(
          (Uri.empty,filter)
          >>* (fun (u,fil) -> set errors_query fil u, limit)
          >>* (fun (u,lim) -> set limit_query  lim u, total)
          >>* (fun (u,tot) -> set total_query  tot u, thin)
          >>* (fun (u,thn) -> set thin_query   thn u, None)
          |>  (fun (u,_)   -> set_time_query time u)
          |>  (fun uri     -> req_to_uri ~uri control `Errors))
      in get_result (fun _ -> Error "not implemented") (Uri.to_string uri)

  end

end
