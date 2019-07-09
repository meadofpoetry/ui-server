open Js_of_ocaml
open Netlib
open Components
open Pipeline_types
open Pipeline_http_js

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let thread =
    Http_wm.get_layout ()
    >>= fun wm ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, event) ->
    let editor = Container_editor.make ~scaffold wm in
    let e = React.E.map (fun x -> editor#notify (`Layout x)) event in
    editor#set_on_destroy (fun () ->
        React.E.stop ~strong:true e;
        React.E.stop ~strong:true event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok editor in
  let body = Ui_templates.Loader.create_widget_loader thread in
  body#add_class "wm";
  scaffold#set_body body
