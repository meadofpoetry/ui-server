open Js_of_ocaml
open Containers
open Components
open Common
open Lwt.Infix

module CSS = struct

  let root = "webrtc-mosaic"

  let video = CSS.add_element root "video"
  let audio = CSS.add_element root "audio"
  let video_container = root ^ "-container"
  let theater_container = CSS.add_modifier video_container "theater"

end

module Janus = struct

  type track =
    { id : int
    ; description : string
    ; video : Janus_streaming.Mp_rtp.video option
    ; audio : Janus_streaming.Mp_rtp.audio option
    }

  type media =
    { main : track
    ; opt : track list
    }

  let server =
    let location = Dom_html.window##.location in
    let protocol = Js.to_string location##.protocol in
    let hostname = Js.to_string location##.hostname in
    protocol ^ "//" ^ hostname ^ ":8088/janus"

  let session ?(debug = `All false) () =
    Janus_static.init debug
    >>= (fun () ->
      let t, _ = Janus_static.create ~server:(`One server) () in
      t)

  let handle_plugin ~selected ~tracks ~target e_rs e_jsep plugin =
    let open Janus_static in
    React.E.map (Janus.attachMediaStream target) e_rs |> React.E.keep;
    React.E.map (function
        | Session.Offer x ->
           Plugin.create_answer plugin Janus_streaming.default_media_props None x
           >>= (function
                | Ok jsep -> Janus_streaming.send ~jsep plugin Start
                | Error e -> Printf.printf "Error creating answer: %s\n" e;
                             Lwt.return_ok ())
           |> Lwt.ignore_result
        | Answer x -> Plugin.handle_remote_jsep plugin x |> Lwt.ignore_result
        | Unknown _ -> Printf.printf "Unknown jsep received\n") e_jsep
    |> React.E.keep;
    List.iter (fun x ->
        let req =
          ({ type_ = Rtp { base = ({ id = Some x.id
                                   ; name = None
                                   ; description = Some x.description
                                   ; is_private = false
                                   ; audio = Option.is_some x.audio
                                   ; video = Option.is_some x.video
                                   ; data = false
                                   } : Janus_streaming.Mp_base.t)
                         ; audio = x.audio
                         ; video = x.video
                         ; data = None
                       }
           ; admin_key = None
           ; secret = None
           ; pin = None
           ; permanent = true
           } : Janus_streaming.Mp_create.t)
        in
        Janus_streaming.send plugin (Create req)
        >>= (function
             | Ok _ -> Printf.printf "created mp!\n"; Lwt.return_unit
             | Error e -> Printf.printf "failure creating mp: %s\n" e; Lwt.return_unit)
        |> Lwt.ignore_result) tracks;
    React.S.changes selected
    |> React.E.map_s (fun x -> Janus_streaming.send plugin (Switch x.id))
    |> React.E.keep;
    let init = React.S.value selected in
    Janus_streaming.send plugin (Watch { id = init.id; secret = None }) |> ignore;
    Lwt.return_unit

  let plugin ~(tracks : track list)
        ~(selected : track React.signal)
        ~(target : #Dom_html.element Js.t)
        session =
    let open Janus_static in
    let e_jsep, on_jsep = React.E.create () in
    let e_rs, on_remote_stream = React.E.create () in
    Session.attach ~session
      ~plugin_type:Plugin.Streaming
      ~on_remote_stream
      ~on_jsep
      ()
    >>= handle_plugin ~tracks ~selected ~target e_rs e_jsep

  let main =
    { id = 1
    ; description = "Video plus alarm audio"
    ; video =
        Some { videomcast = None
             ; videoport = 5004
             ; videopt = 100
             ; videortpmap = "VP8/90000" (* FIXME should be configurable *)
             ; videofmtp = None
             ; videoiface = None
             ; videobufferkf = None }
    ; audio =
        Some { audiomcast = None
             ; audioport = 5005
             ; audiopt = 111
             ; audiortpmap = "opus/48000/2" (* FIXME should be configurable *)
             ; audiofmtp = None
             ; audioiface = None }
    }

  let opt =
    { id = 2
    ; description = "Program 1 audio"
    ; video = None
    ; audio =
        Some { audiomcast = None
             ; audioport = 5006
             ; audiopt = 111
             ; audiortpmap = "opus/48000/2"
             ; audiofmtp = None
             ; audioiface = None }
    }

end

let make_video () =
  let video = Dom_html.(createVideo document) in
  video##setAttribute (Js.string "playsinline") (Js.string "true");
  video##setAttribute (Js.string "autoplay") (Js.string "true");
  video##setAttribute (Js.string "controls") (Js.string "false");
  video##.classList##add (Js.string CSS.video);
  video

let make_audio () =
  let audio = Dom_html.(createAudio document) in
  audio##setAttribute (Js.string "playsinline") (Js.string "true");
  audio##setAttribute (Js.string "autoplay") (Js.string "true");
  audio##setAttribute (Js.string "controls") (Js.string "true");
  audio##.classList##add (Js.string CSS.audio);
  audio

let page () =
  let video = make_video () in
  let audio = make_audio () in
  let janus_lwt =
    Lwt.catch
      (fun () ->
        Janus.session ()
        >>= (fun s ->
         Janus.plugin ~tracks:[Janus.main]
           ~selected:(React.S.const Janus.main)
           ~target:video
           s
         |> ignore;
         Janus.plugin ~tracks:[Janus.opt]
           ~selected:(React.S.const Janus.opt)
           ~target:audio
           s
         |> ignore;
         Lwt.return_ok ()))
      (fun exn ->
        let err = match exn with
          | Janus_static.Not_created s ->
             Printf.sprintf "WebRTC session not created:\n %s" s
          | e -> Printexc.to_string e in
       Lwt.return_error err) in
  Ui_templates.Loader.create_widget_loader
    Lwt_result.(
    janus_lwt
    >|= (fun () ->
      let v = Widget.create video in
      let a = Widget.create audio in
      let root = Widget.create_div ~widgets:[v; a] () in
      root#add_class CSS.theater_container;
      root#add_class CSS.root;
      root))
