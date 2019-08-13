open Js_of_ocaml
open Netlib
open Components
open Pipeline_http_js
(* open Pipeline_widgets_js *)

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let () =
  let (_scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let _thread =
    (* let open React in *)
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket -> Http_measurements.Event.get_video socket
    >>=? fun (_v_id, _v_ev) -> Http_measurements.Event.get_audio socket
    >>=? fun (_a_id, _a_ev) -> Http_structure.get_annotated ()
    >>=? fun _structures ->
    Lwt.return_ok (Widget.create_div ()) in
  ()
