open React

let return = Lwt.return
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let (%) = CCFun.(%)

let button_type = Js.string "button"

let server =
  let protocol = (Js.to_string Dom_html.window##.location##.protocol) in
  protocol ^ "//" ^ (Js.to_string Dom_html.window##.location##.hostname) ^ ":8088/janus"

let plugin = "janus.plugin.streaming"

let log = Printf.printf
           
let make_struct () =
  let open Common in
  let tmp = Qoe_types.default in
  Qoe_types.State.to_yojson tmp
  |> Common.Qoe_types.filter_none
  |> Yojson.Safe.to_string

(* let attach_plugin (j:Janus.janus Js.t) = *)
(*   let thread, wakener = Lwt.wait() in *)
(*   let parameters = Janus.make_parameters *)
(*       ~plugin:plugin *)
(*       ~opaqueId:"webrtcstreamingocaml" *)
(*       ~success:(fun handler -> Lwt.wakeup wakener handler; *)
(*                  log "Plugin attached: (ID: %d, Name: %s)\n" *)
(*                    (int_of_float (Js.float_of_number (handler##getId ()))) *)
(*                    (Js.to_string (handler##getPlugin ())); *)
(*                  handler##send (Janus.make_plugin_msg *)
(*                                   ~request:`List *)
(*                                   ~success:(fun response -> *)
(*                                       log "Response: %s\n" (Js.to_string (Json.output response)); *)
(*                                       handler##send (Janus.make_plugin_msg *)
(*                                                        ~request:(`Watch (1, None)) *)
(*                                                        ~error:(fun cause -> *)
(*                                                            log "Watch request error: %s\n" (Js.to_string (Json.output cause))) *)
(*                                                        ());) *)
(*                                   ~error:(fun cause -> *)
(*                                       log "List request error: %s\n" (Js.to_string (Json.output cause))) *)
(*                                   ()); *)
(*                ) *)
(*       ~error:(fun cause -> log "Plugin NOT attached: %s\n" (Js.to_string cause)) *)
(*       ~onmessage:(fun msg jsep -> *)
(*           thread >>= (fun handler -> *)
(*               log "::: Got a message :::\n"; *)
(*               log "Message: %s\n" (Js.to_string (Json.output msg)); *)
(*               (let open CCOpt in *)
(*                Js.Optdef.to_option jsep *)
(*                >>= Js.Opt.to_option *)
(*                >>= (fun jsep' -> *)
(*                    log "Handling SDP as well...\n"; *)
(*                    return (handler##createAnswer (Janus.make_plugin_msg *)
(*                                                     ~jsep:jsep' *)
(*                                                     ~media:(false,false) *)
(*                                                     ~success:(fun jsep'' -> *)
(*                                                         log "Got SDP!\n"; *)
(*                                                         handler##send (Janus.make_plugin_msg *)
(*                                                                          ~jsep:jsep'' *)
(*                                                                          ~request:`Start *)
(*                                                                          ())) *)
(*                                                     ~error:(fun cause -> *)
(*                                                         log "Answer error: %s\n" (Js.to_string (Json.output cause))) *)
(*                                                     ())))) *)
(*               |> ignore; *)
(*               Lwt.return ()) *)
(*           |> ignore) *)
(*       ~onremotestream:(fun stream -> log "::: Got a remote stream :::\n"; *)
(*                         Janus.attachMediaStream (Dom_html.document##getElementById (Js.string "remotevideo")) stream) *)
(*       () in *)
(*   j##attach parameters *)

let onload _ =

  let e_err, push_err = Lwt_react.E.create () in
  let e_destr, push_destr = Lwt_react.E.create () in
  let _ = Lwt_react.E.map (fun s -> log "Janus session error: %s\n" s) e_err in
  let _ = Lwt_react.E.map (fun () -> log "Janus session destroyed") e_destr in

  let b2s b = if b then "true" else "false" in
  let e_consent, push_consent = React.E.create () in
  let _ = Lwt_react.E.map (fun x -> Printf.printf "ConsentDialog is %s\n" (b2s x)) e_consent in
  let e_webrtc, push_webrtc = React.E.create () in
  let _ = Lwt_react.E.map (fun x -> Printf.printf "WebRTC is %s\n" (b2s x)) e_webrtc in
  let e_icestate, push_icestate = React.E.create () in
  let _ = Lwt_react.E.map (fun x -> Printf.printf "IceState is %s\n" x) e_icestate in
  let e_mediastate, push_mediastate = React.E.create () in
  let _ = Lwt_react.E.map (fun (x,y) -> Printf.printf "Media State is %s %s\n" x (b2s y)) e_mediastate in
  let e_slowlink, push_slowlink = React.E.create () in
  let _ = Lwt_react.E.map (fun x -> Printf.printf "Slowlink is %s\n" (b2s x)) e_slowlink in
  let e_cleanup, push_cleanup = React.E.create () in
  let _ = Lwt_react.E.map (fun () -> Printf.printf "On cleanup\n") e_cleanup in
  let e_detached, push_detached = React.E.create () in
  let _ = Lwt_react.E.map (fun () -> Printf.printf "Detached") e_detached in

  Janus_static.init (`All true)
  >>= (fun () ->
      let j = Janus_static.create { server = `One server
                                  ; ice_servers = None
                                  ; ipv6 = None
                                  ; with_credentials = None
                                  ; max_poll_events = None
                                  ; destroy_on_unload = None
                                  ; token = None
                                  ; apisecret = None
                                  } in
      j.error >>= (fun s -> push_err s |> return) |> ignore;
      j.destroy >>= (fun () -> push_destr () |> return) |> ignore;
      j.success)
  >>= (fun j -> Janus_static.attach j { name = plugin
                                      ; opaque_id = None
                                      ; consent_dialog = Some push_consent
                                      ; webrtc_state = Some push_webrtc
                                      ; ice_state = Some push_icestate
                                      ; media_state = Some push_mediastate
                                      ; slow_link = Some push_slowlink
                                      ; on_cleanup = Some push_cleanup
                                      ; detached = Some push_detached
                                      }
              |> ignore ;
        Lwt.return @@ log "Session created!\n")
  |> ignore;

  let str = make_struct () in

  let ask_server push =
    let post_args = ["data", `String (Js.bytestring str)] in
    Lwt_xmlHttpRequest.perform_raw
      ~content_type:"application/json; charset=UTF-8"
      ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
                "X-Requested-With", "XMLHttpRequest"]
      ~override_method:`POST
      ~contents:(`POST_form post_args)
      ~response_type:XmlHttpRequest.Text
      "api/test"
    >|= (fun resp ->
        push @@ Js.some resp.content)
  in

  let v, push = S.create Js.null in

  let doc = Dom_html.document in
  let div = Dom_html.createDiv doc in
  let h2 = Dom_html.createH2 doc in
  let button_set   = Dom_html.createInput ~_type:button_type doc in
  let button_reset = Dom_html.createInput ~_type:button_type doc in
  button_set##.value := Js.string "Change Content";
  button_reset##.value := Js.string "Reset Content";

  let _ = S.map (fun text -> h2##.textContent := text; text) v in

  Lwt.ignore_result @@ Lwt_js_events.clicks button_set (fun _ _ -> ask_server push);
  Lwt.ignore_result @@ Lwt_js_events.clicks button_reset (fun _ _ -> return @@ push Js.null);

  Dom.appendChild div h2;
  Dom.appendChild doc##.body div;
  Dom.appendChild doc##.body button_set;
  Dom.appendChild doc##.body button_reset;
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload
