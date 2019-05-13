open Js_of_ocaml
open Containers
open Components
open Util_react

open Lwt.Infix

type track =
  { id : int
  ; description : string
  ; video : Janus_streaming.Mp_rtp.video option
  ; audio : Janus_streaming.Mp_rtp.audio option
  }

type media =
  { main : track
  ; opt  : track list
  }

let server =
  let protocol = (Js.to_string Dom_html.window##.location##.protocol) in
  protocol ^ "//" ^ (Js.to_string Dom_html.window##.location##.hostname) ^ ":8088/janus"

let janus_session ?(debug=`All false) () =
  Janus_static.init debug
  >>= (fun () ->
    let res = Janus_static.create ~server:(`One server) () in
    res.error >>= (fun s  -> Printf.printf "Error in session handle %s\n" s |> Lwt.return) |> ignore;
    res.destroy >>= (fun () -> Printf.printf "Session handle destroyed\n" |> Lwt.return) |> ignore;
    res.success)

let janus_plugin ~(tracks: track list)
      ~(selected: track React.signal)
      ~(target:   #Dom_html.element Js.t)
      session =
  let open Janus_static in
  let e_jsep, on_jsep        = React.E.create () in
  let e_rs, on_remote_stream = React.E.create () in
  Session.attach ~session ~plugin_type:Plugin.Streaming ~on_remote_stream ~on_jsep ()
  >>= (fun plugin ->
    E.map (fun stream -> Janus.attachMediaStream target stream) e_rs
    |> E.keep;
    React.E.map (function
        | Session.Offer x ->
           Plugin.create_answer plugin Janus_streaming.default_media_props None x
           >>= (function
                | Ok jsep -> Janus_streaming.send ~jsep plugin Start
                | Error e -> Printf.printf "Error creating answer: %s\n" e;
                             Lwt.return_ok ())
           |> Lwt.ignore_result
        | Answer x  -> Plugin.handle_remote_jsep plugin x |> Lwt.ignore_result
        | Unknown _ -> Printf.printf "Unknown jsep received\n") e_jsep
    |> E.keep;
    List.iter (fun x ->
        let req =
          ({ type_ = Rtp { base = ({ id          = Some x.id
                                   ; name        = None
                                   ; description = Some x.description
                                   ; is_private  = false
                                   ; audio       = Option.is_some x.audio
                                   ; video       = Option.is_some x.video
                                   ; data        = false
                                   }:Janus_streaming.Mp_base.t)
                         ; audio = x.audio
                         ; video = x.video
                         ; data  = None
                       }
           ; admin_key = None
           ; secret    = None
           ; pin       = None
           ; permanent = true
           } : Janus_streaming.Mp_create.t)
        in
        Janus_streaming.send plugin (Create req)
        >>= (function
             | Ok _    -> Printf.printf "created mp!\n"; Lwt.return_unit
             | Error e -> Printf.printf "failure creating mp: %s\n" e; Lwt.return_unit)
        |> Lwt.ignore_result) tracks;
    React.S.changes selected
    |> E.map_s (fun x -> Janus_streaming.send plugin (Switch x.id))
    |> E.keep;
    let init = React.S.value selected in
    Janus_streaming.send plugin (Watch { id = init.id; secret = None }) |> ignore;
    Lwt.return_unit)

let make_video () =
  let video = Dom_html.createVideo Dom_html.document in
  video##setAttribute (Js.string "playsinline") (Js.string "true");
  video##setAttribute (Js.string "autoplay") (Js.string "true");
  video##setAttribute (Js.string "controls") (Js.string "true");
  video##.id := Js.string "remotevideo";
  video##.style##.backgroundColor := Js.string "rgba(0,0,0,1)";
  video##setAttribute (Js.string "data-setup") (Js.string "{language: \"ru\"}");
  video##.classList##add (Js.string "video-js");
  video

let make_audio () =
  let audio = Dom_html.createAudio Dom_html.document in
  audio##setAttribute (Js.string "playsinline") (Js.string "true");
  audio##setAttribute (Js.string "autoplay") (Js.string "true");
  audio##setAttribute (Js.string "controls") (Js.string "true");
  audio##.id := Js.string "remoteaudio";
  audio##.style##.display := Js.string "none";
  audio

let main =
  { id = 1
  ; description = "Video plus alarm audio"
  ; video = Some { videomcast    = None
                 ; videoport     = 5004
                 ; videopt       = 100
                 ; videortpmap   = "VP8/90000"
                 ; videofmtp     = None
                 ; videoiface    = None
                 ; videobufferkf = None
              }
  ; audio = Some { audiomcast    = None
                 ; audioport     = 5005
                 ; audiopt       = 111
                 ; audiortpmap   = "opus/48000/2"
                 ; audiofmtp     = None
                 ; audioiface    = None
              }
  }

let opt =
  { id = 2
  ; description = "Program 1 audio"
  ; video = None
  ; audio = Some { audiomcast    = None
                 ; audioport     = 5006
                 ; audiopt       = 111
                 ; audiortpmap   = "opus/48000/2"
                 ; audiofmtp     = None
                 ; audioiface    = None
              }
  }


let page () =
  let video = make_video () in
  let audio = make_audio () in
  let () =
    Lwt.catch
      (fun () ->
        janus_session ()
        >>= (fun s -> janus_plugin ~tracks:[main]
                        ~selected:(React.S.const main)
                        ~target:video
                        s
                      |> ignore;
                      janus_plugin ~tracks:[opt]
                        ~selected:(React.S.const opt)
                        ~target:audio
                        s
                      |> ignore;
                      Lwt.return_ok ()))
      (function
       | e -> Printf.printf "Exception in janus pipe: %s\n" (Printexc.to_string e);
              Lwt.return_error @@ Printexc.to_string e;)
    |> Lwt.ignore_result in
  let cell =
    new Layout_grid.Cell.t
      ~span:12
      ~widgets:[ Widget.create video
               ; Widget.create audio ]
      ()
  in
  new Layout_grid.t ~cells:[cell] ()
