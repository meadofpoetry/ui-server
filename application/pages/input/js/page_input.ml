open Js_of_ocaml
open Components
open Netlib

let ( >>= ) = Lwt.bind

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Selector = struct
  let tab_bar = Printf.sprintf ".%s .%s" Top_app_bar.CSS.root Tab_bar.CSS.root
end

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let tab_bar = match Element.query_selector scaffold#root Selector.tab_bar with
    | None -> failwith "tab bar element not found"
    | Some x -> Tab_bar.attach x in
  let thread =
    Application_http_js.get_topology ()
    >>=? fun topo ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket -> Application_http_js.Event.get_topology socket
    >>=? fun (_, topo_event) ->
    let page = Widget.create_div () in
    page#set_on_destroy (fun () ->
        React.E.stop ~strong:true topo_event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok page in
  let body = Ui_templates.Loader.create_widget_loader thread in
  scaffold#set_on_destroy (fun () -> tab_bar#destroy ());
  scaffold#set_body body
