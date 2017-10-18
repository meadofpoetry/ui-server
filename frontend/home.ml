open Lwt_react
open Board_ip_dektec_js.Requests
open Pipeline_js.Requests
   
let return = Lwt.return
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let (%) = CCFun.(%)

let button_type = Js.string "button"

let server =
  let protocol = (Js.to_string Dom_html.window##.location##.protocol) in
  protocol ^ "//" ^ (Js.to_string Dom_html.window##.location##.hostname) ^ ":8088/janus"

let janus_pipe debug =
  let open Janus_static in
  let e_msg, push_msg   = Lwt_react.E.create () in
  let e_jsep, push_jsep = Lwt_react.E.create () in
  let e_rs, push_rs     = Lwt_react.E.create () in

  init debug
  >>= (fun () -> let res = create ~server:(`One server) () in
        (* FIXME do something useful in case of error*)
        res.error >>= (fun s -> Printf.printf "Error in session handle %s\n" s |> return) |> ignore;
        (* FIXME do something useful in case of destroy*)
        res.destroy >>= (fun () -> Printf.printf "Session handle destroyed\n" |> return) |> ignore;
        res.success)
  >>= (fun session -> Session.attach
          ~session:session
          ~plugin_type:Plugin.Streaming
          ~on_remote_stream:push_rs
          ~on_message:push_msg
          ~on_jsep:push_jsep
          ())
  >>= (fun plugin ->
      let _ = Lwt_react.E.map (fun x-> Printf.printf "Got a message: %s\n"
                                  (Js.to_string @@ Json.output x)) e_msg in
      let _ = Lwt_react.E.map (fun stream -> Janus.attachMediaStream "remotevideo" stream) e_rs in
      let _ = Lwt_react.E.map (function
          | Session.Offer x ->
            Plugin.create_answer plugin Janus_streaming.default_media_props None x
            >>= (function
                | Ok jsep -> return @@ (Janus_streaming.send ~jsep:jsep plugin Start |> ignore)
                | Error e -> return @@ Printf.printf "Error creating answer: %s\n" e) |> ignore
          | Answer x        -> Plugin.handle_remote_jsep plugin x |> ignore
          | Unknown _       -> Printf.printf "Unknown jsep received\n" |> ignore) e_jsep in
      return plugin)
  >>= (fun plugin -> Janus_streaming.send plugin (Watch { id = 1; secret = None }) |> ignore ; return ())

let onload _ =

  (*let streams, push_streams = S.create Js.null in*)
  
  let () = (Lwt.catch
              (fun () -> (janus_pipe (`All false)))
              (function
                | e -> return @@ Printf.printf "Exception in janus pipe: %s\n" (Printexc.to_string e)))
           |> ignore in
  
  let doc = Dom_html.document in

  let label    = Dom_html.createH2 doc in
  let button_set = Dom_html.createButton ~_type:(Js.string "button") doc in
  let button_reset = Dom_html.createButton ~_type:(Js.string "button") doc in
  button_set##.value := (Js.string "set");
  button_reset##.value := (Js.string "reset");
  let ev_label = Dom_html.createH2 doc in

  (* test *)

  Lwt.ignore_result @@ Lwt_js_events.clicks button_set (fun _ _ ->
                           let data = Board_ip_dektec_js.Requests.post_delay 5 101 in
                           data >>= function
                           | Error e -> Lwt.return @@ (label##.textContent := Js.some @@ Js.string e)
                           | Ok devi -> Lwt.return @@ (label##.textContent := Js.some @@ Js.string
                                                                              @@ Yojson.Safe.to_string
                                                                              @@ Board_ip_dektec_js.Board_types.delay_to_yojson devi));

  Lwt.ignore_result @@ Lwt_js_events.clicks button_reset (fun _ _ -> Lwt.return @@ (label##.textContent := Js.some @@ Js.string ""));

  let _ = React.E.map (fun x -> ev_label##.textContent := Js.some @@ Js.string (Yojson.Safe.to_string @@ Board_ip_dektec_js.Board_types.board_status_to_yojson x)) (Board_ip_dektec_js.Requests.get_status_socket 5) in
  
  Dom.appendChild (Dom_html.getElementById "arbitrary-content") label;
  Dom.appendChild (Dom_html.getElementById "arbitrary-content") button_set;
  Dom.appendChild (Dom_html.getElementById "arbitrary-content") button_reset;
  Dom.appendChild (Dom_html.getElementById "arbitrary-content") ev_label;
    
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload
