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
    (* Getting the data *)
    let*? socket = Api_js.Websocket.JSON.open_socket ~path:(Path.Format.of_string "ws") () in
    let*? state = Pc_control_http_js.Timedate.get_config () in
    let*? (_, state_ev) = Pc_control_http_js.Timedate.Event.get_config socket in
    let*? zones = Pc_control_http_js.Timedate.get_timezones () in

    (* Creating monotonic time source *)
    let mcounter = Mtime_clock.counter () in
    let mtimer, mtimer_update =
      S.create
        ~eq:Mtime.Span.equal
        (Mtime_clock.count mcounter)
    in
    let time_pusher = Dom_html.window##setInterval
                        (Js.wrap_callback (fun () ->
                             mtimer_update (Mtime_clock.count mcounter);
                             Printf.printf "Mtimer updated %f\n" (Mtime.Span.to_s (React.S.value mtimer))))
                        60000.0
    in
    
    (* Creating widgets *)
    let ntp = Ntp_config.make state in
    let time = Time_config.make state mtimer ntp#disabled in
    let timezone = Timezone_config.make zones state in    
    let page =
      Widget.create
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ D.create ~children:[ ntp#markup; time#markup; timezone#markup ] ()
    in

    let update_state (state : Pc_control_types.Timedate_config.t) =
      let open Pc_control_types.Timedate_config in
      ntp#set_value (state.ntp, state.ntp_server, state.ntp_ip);
      time#set_value state.local_time
    in
    let event' = React.E.map update_state state_ev in

    page#set_on_destroy (fun () ->
        time#destroy ();
        ntp#destroy ();
        timezone#destroy ();
        E.stop ~strong:true state_ev;
        E.stop ~strong:true event';
        S.stop ~strong:true mtimer;
        Dom_html.window##clearInterval time_pusher;
        Api_js.Websocket.close_socket socket);      
    
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
