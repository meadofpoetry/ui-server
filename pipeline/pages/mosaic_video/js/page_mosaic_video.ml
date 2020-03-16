open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pipeline_http_js
include Page_mosaic_video_tyxml.Page
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

module Selector = struct
  let overflow_menu = "." ^ Components_lab.Overflow_menu.CSS.root

  let side_sheet_icon = "." ^ CSS.side_sheet_icon
end

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

module RTC = struct
  open Janus_js

  type t = { session : Session.t; video : Plugin.t; audio : Plugin.t }

  type track = {
    id : int;
    description : string;
    video : Streaming.Mp_rtp.video option;
    audio : Streaming.Mp_rtp.audio option;
  }

  type media = { main : track; opt : track list }

  let server =
    let location = Dom_html.window##.location in
    let protocol = Js.to_string location##.protocol in
    let hostname = Js.to_string location##.hostname in
    let port = match protocol with "https:" -> 8089 | _ -> 8088 in
    Printf.sprintf "%s//%s:%d/janus" protocol hostname port

  let main =
    {
      id = 1;
      description = "Video plus alarm audio";
      video =
        Some
          {
            videomcast = None;
            videoport = 5004;
            videopt = 100;
            videortpmap = "VP9/90000" (* FIXME should be configurable *);
            videofmtp = None;
            videoiface = None;
            videobufferkf = None;
          };
      audio =
        Some
          {
            audiomcast = None;
            audioport = 5005;
            audiopt = 111;
            audiortpmap = "opus/48000/2" (* FIXME should be configurable *);
            audiofmtp = None;
            audioiface = None;
          };
    }

  let opt =
    {
      id = 2;
      description = "Program 1 audio";
      video = None;
      audio =
        Some
          {
            audiomcast = None;
            audioport = 5006;
            audiopt = 111;
            audiortpmap = "opus/48000/2";
            audiofmtp = None;
            audioiface = None;
          };
    }

  module MP = struct
    open Streaming

    let track_to_create_req (track : track) : Mp_create.t =
      let (base : Mp_base.t) =
        {
          id = Some track.id;
          name = None;
          description = Some track.description;
          is_private = false;
          audio = Option.is_some track.audio;
          video = Option.is_some track.video;
          data = false;
        }
      in
      let (rtp : Mp_rtp.t) =
        { base; audio = track.audio; video = track.video; data = None }
      in
      {
        type_ = Rtp rtp;
        admin_key = None;
        secret = None;
        pin = None;
        permanent = true;
      }

    let create (plugin : Plugin.t) (req : Mp_create.t) :
        (Mp_create.r, string) Lwt_result.t =
      Plugin.send_message
        ~message:(Js.Unsafe.obj @@ request_to_obj (Create req))
        plugin
      >>= function
      | Ok None -> Lwt.return_error "empty response"
      | Ok (Some d) -> Lwt.return @@ Mp_create.of_js_obj d
      | Error _ as e -> Lwt.return e

    let watch ?secret (plugin : Plugin.t) (id : int) :
        (unit, string) Lwt_result.t =
      let req = Mp_watch.{ id; secret } in
      Plugin.send_message
        ~message:(Js.Unsafe.obj @@ request_to_obj (Watch req))
        plugin
      >>= function
      | Ok _ -> Lwt.return_ok ()
      | Error _ as e -> Lwt.return e

    let start ?jsep (plugin : Plugin.t) =
      Plugin.send_message ?jsep
        ~message:(Js.Unsafe.obj @@ request_to_obj Start)
        plugin
      >>= function
      | Ok _ -> Lwt.return_ok ()
      | Error _ as e -> Lwt.return e
  end

  let handle_jsep (jsep : Webrtc._RTCSessionDescriptionInit Js.t)
      (plugin : Plugin.t) =
    match Js.to_string jsep##._type with
    | "answer" -> Plugin.handle_remote_jsep jsep plugin
    | "offer" -> (
        let video = Media.make_video ~source:(`Bool false) () in
        let audio = Media.make_audio ~source:(`Bool false) () in
        let media = Media.make ~audio ~video () in
        Plugin.create_answer ~jsep ~data:(`Bool true) (`Media media) plugin
        >>= function
        | Ok jsep -> MP.start ~jsep plugin
        | Error e -> Lwt.return_error e )
    | s -> Lwt.return_error @@ Printf.sprintf "Unknown jsep received: %s" s

  let start_webrtc (player : Player.t) : (t, string) Lwt_result.t =
    Lwt.Infix.(
      create_session ~server
        ~on_error:(fun s ->
          let ph = Components_lab.Placeholder.make_error ~text:(`Text s) () in
          player#set_overlay ph)
        (create ~log_level:Error ())
      >>= function
      | Error e -> Lwt_result.fail e
      | Ok session -> (
          Session.attach_plugin ~typ:Streaming
            ~on_message:(fun ?jsep _ (plugin : Plugin.t) ->
              match jsep with
              | None -> ()
              | Some jsep ->
                  handle_jsep jsep plugin
                  >|= (function
                        | Error e ->
                            let ph =
                              Components_lab.Placeholder.make_error
                                ~text:(`Text e) ()
                            in
                            player#set_overlay ph
                        | Ok _ -> ())
                  |> Lwt.ignore_result)
            ~on_remote_stream:(fun stream (plugin : Plugin.t) ->
              Plugin.Stats.start_bitrate_loop
                (fun { audio; video } ->
                  let audio = match audio with None -> 0 | Some x -> x in
                  let video = match video with None -> 0 | Some x -> x in
                  Printf.printf "video: %d, audio: %d\n" video audio)
                plugin
              >|= (function
                    | Ok _ -> print_endline "Bitrate loop started"
                    | Error e ->
                        Lwt_log_js.ign_error_f ~logger:Lwt_log_js.console
                          "Bitrate loop failed: %s\n" e)
              |> Lwt.ignore_result;
              attach_media_stream player#video_element stream)
            session
          >>= function
          | Ok (plugin : Plugin.t) ->
              List.map
                (fun (x : track) ->
                  MP.create plugin (MP.track_to_create_req x)
                  |> Lwt_result.map_err
                       (Printf.printf "failure creating mp: %s\n")
                  |> Lwt.map (fun _ -> ()))
                [ main ]
              |> Lwt.join
              >>= fun () ->
              MP.watch plugin main.id >>= fun _ ->
              Lwt.return_ok { session; video = plugin; audio = plugin }
          | Error e -> Lwt_result.fail e ))
end

let tie_side_sheet_with_toggle (scaffold : Scaffold.t) =
  match
    ( Element.query_selector scaffold#root Selector.side_sheet_icon,
      scaffold#side_sheet )
  with
  | Some i, Some side_sheet ->
      Lwt.async (fun () ->
          Js_of_ocaml_lwt.Lwt_js_events.clicks i (fun _ _ ->
              side_sheet#toggle ()))
  | _ -> ()

let make_hotkeys_dialog () =
  let open Dialog.D in
  let hotkeys =
    Widget.create @@ Tyxml_js.To_dom.of_element @@ D.create_hotkeys ()
  in
  let title = dialog_title ~title:"Горячие клавиши" () in
  let cancel = dialog_action ~label:"Закрыть" ~action:Close () in
  let content = dialog_content ~children:[ hotkeys#markup ] () in
  Dialog.make ~title ~content ~actions:[ cancel ] ()

let tie_menu_with_toggle (scaffold : Scaffold.t) (wizard_dialog, show_wizard) =
  match Element.query_selector scaffold#root Selector.overflow_menu with
  | None -> None
  | Some elt ->
      let menu = Components_lab.Overflow_menu.attach elt in
      let hotkeys = Dom_html.getElementById "hotkeys" in
      let wizard = Dom_html.getElementById "wizard" in
      let hotkeys_dialog = make_hotkeys_dialog () in
      let listeners =
        Js_of_ocaml_lwt.Lwt_js_events.
          [
            clicks hotkeys (fun _ _ ->
                hotkeys_dialog#open_await () >>= fun _ -> Lwt.return_unit);
            clicks wizard (fun _ _ -> show_wizard ());
          ]
      in
      Dom.appendChild Dom_html.document##.body hotkeys_dialog#root;
      Dom.appendChild Dom_html.document##.body wizard_dialog#root;
      menu#set_on_destroy (fun () ->
          Dom.removeChild Dom_html.document##.body hotkeys_dialog#root;
          List.iter Lwt.cancel listeners);
      menu#layout ();
      Some menu

let submit_wizard (scaffold : Scaffold.t) value =
  Pipeline_http_js.Http_wm.set_layout value >>= function
  | Ok _ ->
      let label = "Мозаика сохранена" in
      let snackbar = Snackbar.make ~dismiss:`True ~label:(`Text label) () in
      snackbar#set_timeout 4.;
      scaffold#show_snackbar ~on_close:(fun _ -> snackbar#destroy ()) snackbar
  | Error (`Msg e) ->
      let label = Printf.sprintf "Ошибка. %s" e in
      let snackbar = Snackbar.make ~label:(`Text label) () in
      scaffold#show_snackbar ~on_close:(fun _ -> snackbar#destroy ()) snackbar

let make_wizard (scaffold : Scaffold.t) =
  let thread =
    let open React in
    Http_wm.get_layout () >>=? fun wm ->
    Http_structure.get_streams () >>=? fun streams ->
    Http_structure.get_annotated () >>=? fun structures ->
    Api_js.Websocket.JSON.open_socket
      ~path:(Netlib.Uri.Path.Format.of_string "ws")
      ()
    >>=? fun socket ->
    Http_wm.Event.get socket >>=? fun (_, wm_event) ->
    Http_structure.Event.get_annotated socket >>=? fun (_, streams_event) ->
    let streams = List.map snd streams in
    let wizard = Pipeline_widgets.Wizard.make streams structures wm in
    let notif =
      E.merge
        (fun _ -> wizard#notify)
        ()
        [
          E.map (fun x -> `Layout x) wm_event;
          E.map (fun x -> `Streams x) streams_event;
        ]
    in
    wizard#set_on_destroy (fun () ->
        E.stop ~strong:true notif;
        E.stop ~strong:true wm_event;
        E.stop ~strong:true streams_event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok wizard
  in
  let title = Dialog.D.dialog_title ~title:Pipeline_widgets.Wizard.title () in
  let loader = Components_lab.Loader.make_widget_loader thread in
  let content =
    Dialog.D.dialog_content ~children:[ Tyxml_js.Of_dom.of_element loader ] ()
  in
  let cancel = Dialog.D.dialog_action ~action:Close ~label:"Отмена" () in
  let accept =
    Button.attach
    @@ Tyxml_js.To_dom.of_element
    @@ Dialog.D.dialog_action ~disabled:true ~action:Accept
         ~label:"Применить" ()
  in
  let actions = [ cancel; accept#markup ] in
  let dialog = Dialog.make ~title ~content ~actions () in
  Lwt.on_success thread (fun _ -> accept#set_disabled false);
  let show () =
    dialog#open_await () >>= function
    | Close | Destroy | Custom _ -> Lwt.return_unit
    | Accept -> (
        thread >>= function
        | Error _ -> Lwt.return_unit
        | Ok wizard ->
            submit_wizard scaffold wizard#value >>= fun _ -> Lwt.return_unit )
  in
  let listeners =
    Js_of_ocaml_lwt.Lwt_js_events.
      [
        seq_loop (make_event Treeview.Event.action) dialog#root (fun _ _ ->
            dialog#layout ();
            Lwt.return_unit);
      ]
  in
  dialog#set_on_destroy (fun () ->
      List.iter Lwt.cancel listeners;
      accept#destroy ());
  (dialog, show)

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let player =
    match scaffold#body with
    | None -> failwith "no video player element found"
    | Some x -> Player.attach x
  in
  tie_side_sheet_with_toggle scaffold;
  let wizard = make_wizard scaffold in
  let menu = tie_menu_with_toggle scaffold wizard in
  scaffold#set_on_destroy (fun () -> Option.iter Widget.destroy menu);
  Lwt.async (fun () ->
      RTC.start_webrtc player >>= function
      | Ok (_ : RTC.t) -> Lwt.return player#root##focus
      | Error e ->
          (* Show error overlay in case of failure while starting webrtc session *)
          let ph = Components_lab.Placeholder.make_error ~text:(`Text e) () in
          ph#add_class Player.CSS.overlay;
          player#append_child ph;
          Lwt.return_unit)
