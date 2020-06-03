open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Netlib.Uri
module D =
  Page_timedate_settings_tyxml.Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( let* ) = Lwt.bind

let ( let*? ) = Lwt_result.bind

let on_loaded (scaffold : Scaffold.t) () =
  let thread =
    let open Lwt_react in
    let*? socket = Api_js.Websocket.JSON.open_socket ~path:(Path.Format.of_string "ws") () in
    let*? state = Pc_control_http_js.Timedate.get_config () in
    let*? (_, state_ev) = Pc_control_http_js.Timedate.Event.get_config socket in
    let ntp = Ntp_config.make state in
    let page =
      Widget.create
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ D.create ~children:[ ntp#markup ] ()
    in
    page#set_on_destroy (fun () ->
        ntp#destroy ();
        E.stop ~strong:true state_ev;
        Api_js.Websocket.close_socket socket);
    (* Setup *)
    Lwt.return_ok page
  in
  let (_ : Dom_html.element Js.t) =
    Components_lab.Loader.make_widget_loader
      ~elt:scaffold#app_content_inner
      thread
  in
  Lwt.return_unit

let () =
  Lwt.async (fun () ->
      let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
      let* () = scaffold#loaded in
      on_loaded scaffold ())
