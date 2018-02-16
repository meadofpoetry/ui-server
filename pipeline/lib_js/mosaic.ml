open Lwt_react
open Requests
open Components

open Lwt.Infix

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
                 res.error >>= (fun s -> Printf.printf "Error in session handle %s\n" s |> Lwt.return) |> ignore;
                 (* FIXME do something useful in case of destroy*)
                 res.destroy >>= (fun () -> Printf.printf "Session handle destroyed\n" |> Lwt.return) |> ignore;
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
                                     | Ok jsep -> Lwt.return @@ (Janus_streaming.send ~jsep:jsep plugin Start
                                                                 |> ignore)
                                     | Error e -> Lwt.return @@ Printf.printf "Error creating answer: %s\n" e)
                                |> ignore
                             | Answer x        -> Plugin.handle_remote_jsep plugin x |> ignore
                             | Unknown _       -> Printf.printf "Unknown jsep received\n" |> ignore) e_jsep in
    Lwt.return plugin)
  >>= (fun plugin -> Janus_streaming.send plugin (Watch { id = 1; secret = None }) |> ignore ; Lwt.return ())

let make_video () =
  let video   = Dom_html.createVideo Dom_html.document in
  video##setAttribute (Js.string "playsinline") (Js.string "true");
  video##setAttribute (Js.string "autoplay") (Js.string "true");
  video##setAttribute (Js.string "controls") (Js.string "true");
  (* video##.autoplay    := Js._true;
   * video##.controls    := Js._true; *)
  video##.id          := Js.string "remotevideo";
  video##.style##.backgroundColor := Js.string "rgba(0,0,0,1)";
  video##setAttribute (Js.string "width") (Js.string "100%");
  video

let page () =
  let video = make_video () in
  let cell  = new Layout_grid.Cell.t ~widgets:[Widget.create video] () in
  let () = (Lwt.catch
              (fun () -> (janus_pipe (`All false)))
              (function
               | e -> Lwt.return @@ Printf.printf "Exception in janus pipe: %s\n" (Printexc.to_string e)))
           |> ignore in
  cell#set_span 12;
  new Layout_grid.t ~cells:[cell] ()
