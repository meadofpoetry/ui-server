open Lwt_react
open Requests
open Components
   
let server =
  let protocol = (Js.to_string Dom_html.window##.location##.protocol) in
  protocol ^ "//" ^ (Js.to_string Dom_html.window##.location##.hostname) ^ ":8088/janus"

let janus_pipe debug =
  let open Lwt.Infix in
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
                        | Ok jsep -> Lwt.return @@ (Janus_streaming.send ~jsep:jsep plugin Start |> ignore)
                        | Error e -> Lwt.return @@ Printf.printf "Error creating answer: %s\n" e) |> ignore
                | Answer x        -> Plugin.handle_remote_jsep plugin x |> ignore
                | Unknown _       -> Printf.printf "Unknown jsep received\n" |> ignore) e_jsep in
    Lwt.return plugin)
  >>= (fun plugin -> Janus_streaming.send plugin (Watch { id = 1; secret = None }) |> ignore ; Lwt.return ())

let load () =
  let () = (Lwt.catch
              (fun () -> (janus_pipe (`All false)))
              (function
               | e -> Lwt.return @@ Printf.printf "Exception in janus pipe: %s\n" (Printexc.to_string e)))
           |> ignore in

  let str = Requests.get_structure_socket () in
  let wm  = Requests.get_wm_socket () in
  
  let doc = Dom_html.document in

  let container = Dom_html.getElementById "pipeline_container" in
  
  let text    = Dom_html.createP doc in
  text##.textContent := Js.some @@ Js.string "Pipeline widget";
  text##.classList##add (Js.string Components.Typography.display1_class);

  let video   = Dom_html.createVideo doc in
  video##setAttribute (Js.string "id") (Js.string "remotevideo");
  video##setAttribute (Js.string "width") (Js.string "640");
  video##setAttribute (Js.string "autoplay") (Js.string "");
  
  let _ = React.E.map (fun s ->
              print_endline "event";
              (try Dom.removeChild container (Dom_html.getElementById Widg.WStructure.id)
               with _ -> print_endline "No el");
              let e = Widg.WStructure.create s (fun s ->
                          let open Lwt.Infix in
                          (Requests.post_structure s
                           >|= function
                           | Ok () -> ()
                           | Error e -> print_endline e)
                          |> ignore)
              in Dom.appendChild container e.div) str
  in

  let _ = React.E.map (fun s ->
              print_endline "wm event";
              (try Dom.removeChild container (Dom_html.getElementById Widg.WLayout.id)
               with _ -> print_endline "No el");
              let e = Widg.WLayout.create s (fun s ->
                          let open Lwt.Infix in
                          (Requests.post_wm s
                           >|= function
                           | Ok () -> ()
                           | Error e -> print_endline e)
                          |> ignore)
              in Dom.appendChild container e.div) wm
  in
      
  Dom.appendChild container text;
  Dom.appendChild container video
