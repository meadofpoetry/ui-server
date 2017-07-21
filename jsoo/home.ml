open React

let return = Lwt.return
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let (%) = CCFun.(%)

let button_type = Js.string "button"

let server =
  let protocol = (Js.to_string Dom_html.window##.location##.protocol) in
  protocol ^ "//" ^ (Js.to_string Dom_html.window##.location##.hostname) ^ ":8088/janus"

let log = Printf.printf
           
let make_struct () =
  let open Common in
  let tmp = Qoe_types.default in
  Qoe_types.State.to_yojson tmp
  |> Common.Qoe_types.filter_none
  |> Yojson.Safe.to_string

let int_opt_to_string = function
  | Some x -> string_of_int x
  | None -> "unknown"

let str_opt_to_string = function
  | Some x -> x
  | None -> "unknown"

(* let info_to_string (i:Janus_static.Plugin.mp_info_ext) = *)
(*   Printf.printf "Id : %s\n\ *)
(*                  Type : %s\n\ *)
(*                  Description: %s\n\ *)
(*                  Video age ms: %s\n\ *)
(*                  Audio age ms: %s\n\ *)
(*                  Data age ms: %s\n\ *)
(*                  Audio: %s\n\ *)
(*                  Video: %s\n\ *)
(*                  Data: %s\n" *)
(*     (int_opt_to_string i.base.id) *)
(*     (str_opt_to_string i.base.type_) *)
(*     (str_opt_to_string i.base.description) *)
(*     (int_opt_to_string i.base.video_age_ms) *)
(*     (int_opt_to_string i.base.audio_age_ms) *)
(*     (int_opt_to_string i.data_age_ms) *)
(*     (str_opt_to_string i.audio) *)
(*     (str_opt_to_string i.video) *)
(*     (str_opt_to_string i.data) *)

let janus_pipe debug =
  let open Janus_static in
  let e_msg, push_msg   = Lwt_react.E.create () in
  let e_jsep, push_jsep = Lwt_react.E.create () in
  let e_rs, push_rs     = Lwt_react.E.create () in
  let _ = Lwt_react.E.map (fun (_,x)-> Printf.printf "Got a message: %s\n"
                              (Js.to_string @@ Json.output x)) e_msg in
  let _ = Lwt_react.E.map (fun (handle,x) ->
      match x with
      | Session.Offer x ->
        Plugin.create_answer handle Janus_streaming.default_media_props None x
        >>= (function
            | Ok jsep -> Lwt.return @@ (Janus_streaming.send ~jsep:jsep handle Start |> ignore)
            | Error e -> Lwt.return @@ Printf.printf "Error creating answer: %s\n" e) |> ignore
      | Answer x        -> Plugin.handle_remote_jsep handle x |> ignore
      | Unknown _       -> Printf.printf "Unknown jsep received\n" |> ignore) e_jsep in
  let _ = Lwt_react.E.map (fun stream -> Janus.attachMediaStream "remotevideo" stream) e_rs in

  init debug
  >>= (fun () -> let res = create { server = `One server
                                  ; ice_servers = None
                                  ; ipv6 = None
                                  ; with_credentials = None
                                  ; max_poll_events = None
                                  ; destroy_on_unload = None
                                  ; token = None
                                  ; apisecret = None
                                  } in
        (* FIXME do something useful in case of error*)
        res.error >>= (fun s -> Printf.printf "Error in session handle %s\n" s |> return) |> ignore;
        (* FIXME do something useful in case of destroy*)
        res.destroy >>= (fun () -> Printf.printf "Session handle destroyed\n" |> return) |> ignore;
        res.success)
  >>= (fun session -> let res = Session.attach session { name = Plugin.Streaming
                                                       ; opaque_id = None
                                                       ; on_local_stream = None
                                                       ; on_remote_stream = Some push_rs
                                                       ; on_message = Some push_msg
                                                       ; on_jsep = Some push_jsep
                                                       ; consent_dialog = None
                                                       ; webrtc_state = None
                                                       ; ice_state = None
                                                       ; media_state = None
                                                       ; slow_link = None
                                                       ; on_cleanup = None
                                                       ; detached = None
                                                       } in
        res.error >>= (fun s -> Printf.printf "Error in plugin handle %s\n" s |> return) |> ignore;
        res.success)
  >>= (fun plugin -> Janus_streaming.send plugin (Watch { id = 1; secret = None }) |> ignore ; Lwt.return plugin)
  >>= (fun plugin ->
      let doc = Dom_html.document in
      let div = Dom_html.createDiv doc in
      let h3 = Dom_html.createH3 doc in
      h3##.textContent := Js.Opt.return @@ Js.string "Bitrate:";
      Dom_html.window##setInterval
        (Js.wrap_callback (fun () ->
             h3##.textContent := Js.Opt.return @@ Js.string @@ "Bitrate: " ^ (Plugin.get_bitrate plugin)))
        300.0 |> ignore;
      Dom.appendChild div h3;
      Dom.appendChild doc##.body div;
      Lwt.return ())

let onload _ =

  let () = (Lwt.catch
              (fun () -> (janus_pipe (`All false)))
              (function
                | Failure e -> Lwt.return @@ Printf.printf "Exception in janus pipe: %s\n" e
                | _ -> Lwt.return @@ Printf.printf "Unknown exception in janus pipe\n"))
           |> ignore in

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
