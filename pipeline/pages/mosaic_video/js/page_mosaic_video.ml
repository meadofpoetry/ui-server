open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js
open Components

(* TODO
   - add hotkeys legend
   - add stats inside the side sheet
   - add settings inside the side sheet
   - add switch to the editor mode
*)

let ( >>= ) = Lwt.bind

module CSS = Page_mosaic_video_tyxml.CSS
module Markup = Page_mosaic_video_tyxml.Make(Xml)(Svg)(Html)
module Hotkeys = Page_mosaic_video_tyxml.Hotkeys.Make(Xml)(Svg)(Html)

module Selectors = struct
  let menu_icon = "." ^ CSS.menu_icon
  let edit = "." ^ CSS.edit
  let side_sheet_icon = "." ^ CSS.side_sheet_icon
end

module RTC = struct
  open Janus_js

  type t =
    { session : Session.t
    ; video : Plugin.t
    ; audio : Plugin.t
    }

  type track =
    { id : int
    ; description : string
    ; video : Streaming.Mp_rtp.video option
    ; audio : Streaming.Mp_rtp.audio option
    }

  type media =
    { main : track
    ; opt : track list
    }

  let server =
    let location = Dom_html.window##.location in
    let protocol = Js.to_string location##.protocol in
    let hostname = Js.to_string location##.hostname in
    let port = match protocol with
      | "https:" -> 8089
      | _ -> 8088 in
    Printf.sprintf "%s//%s:%d/janus" protocol hostname port

  let main =
    { id = 1
    ; description = "Video plus alarm audio"
    ; video =
        Some { videomcast = None
             ; videoport = 5004
             ; videopt = 100
             ; videortpmap = "VP9/90000" (* FIXME should be configurable *)
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

  module MP = struct
    open Streaming

    let track_to_create_req (track : track) : Mp_create.t =
      let (base : Mp_base.t) =
        { id = Some track.id
        ; name = None
        ; description = Some track.description
        ; is_private = false
        ; audio = Utils.Option.is_some track.audio
        ; video = Utils.Option.is_some track.video
        ; data = false
        } in
      let (rtp : Mp_rtp.t) =
        { base
        ; audio = track.audio
        ; video = track.video
        ; data = None
        } in
      { type_ = Rtp rtp
      ; admin_key = None
      ; secret = None
      ; pin = None
      ; permanent = true
      }

    let create (plugin : Plugin.t) (req : Mp_create.t)
      : (Mp_create.r, string) Lwt_result.t =
      Plugin.send_message
        ~message:(Js.Unsafe.obj @@ request_to_obj (Create req))
        plugin
      >>= function
      | Ok None -> Lwt.return_error "empty response"
      | Ok Some d -> Lwt.return @@ Mp_create.of_js_obj d
      | Error _ as e -> Lwt.return e

    let watch ?secret (plugin : Plugin.t) (id : int)
        : (unit, string) Lwt_result.t =
      let req = Mp_watch.{ id; secret } in
      Plugin.send_message
        ~message:(Js.Unsafe.obj @@ request_to_obj (Watch req))
        plugin
      >>= function
      | Ok _ -> Lwt.return_ok ()
      | Error _ as e -> Lwt.return e

    let start ?jsep (plugin : Plugin.t) =
      Plugin.send_message
        ?jsep
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
    | "offer" ->
       (let video = Media.make_video ~source:(`Bool false) () in
        let audio = Media.make_audio ~source:(`Bool false) () in
        let media = Media.make ~audio ~video () in
        Plugin.create_answer ~jsep ~data:(`Bool true) (`Media media) plugin
        >>= function
        | Ok jsep -> MP.start ~jsep plugin
        | Error e -> Lwt.return_error e)
    | s -> Lwt.return_error @@ Printf.sprintf "Unknown jsep received: %s" s

  let start_webrtc (player : Player.t) : (t, string) Lwt_result.t =
    Lwt.Infix.(
      create_session
        ~server
        ~on_error:(fun s ->
          let ph = Ui_templates.Placeholder.Err.make ~text:s () in
          player#set_overlay ph)
        (create ~log_level:Error ())
      >>= function
      | Error e -> Lwt_result.fail e
      | Ok session ->
         Session.attach_plugin
           ~typ:Streaming
           ~on_message:(fun ?jsep _ (plugin : Plugin.t) ->
             match jsep with
             | None -> ()
             | Some jsep ->
                (handle_jsep jsep plugin
                 >|= function
                 | Error e ->
                    let ph = Ui_templates.Placeholder.Err.make ~text:e () in
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
                     Lwt_log_js.ign_error_f
                       ~logger:Lwt_log_js.console
                       "Bitrate loop failed: %s\n"
                       e)
             |> Lwt.ignore_result;
             attach_media_stream player#video_element stream)
           session
         >>= (function
              | Ok (plugin : Plugin.t) ->
                 List.map (fun (x : track) ->
                     MP.create plugin (MP.track_to_create_req x)
                     |> Lwt_result.map_err (Printf.printf "failure creating mp: %s\n")
                     |> Lwt.map (fun _ -> ()))
                   [main]
                 |> Lwt.join
                 >>= fun () -> MP.watch plugin main.id
                 >>= (fun _ ->
                   Lwt.return_ok { session
                                 ; video = plugin
                                 ; audio = plugin })
              | Error e -> Lwt_result.fail e))

end

let tie_side_sheet_with_toggle (scaffold : Scaffold.t) =
  match Element.query_selector scaffold#root Selectors.side_sheet_icon,
        scaffold#side_sheet with
  | Some i, Some side_sheet ->
    Lwt.async (fun () -> Events.clicks i (fun _ _ -> side_sheet#toggle ()))
  | _ -> ()

let make_hotkeys_dialog () =
  let hotkeys =
    Widget.create
    @@ To_dom.of_element
    @@ Markup.create_hotkeys () in
  let title =
    To_dom.of_element
    @@ Dialog.Markup.create_title_simple ~title:"Быстрые клавиши" () in
  let cancel =
    To_dom.of_element
    @@ Dialog.Markup.create_action ~label:"Закрыть" ~action:Close () in
  let content =
    To_dom.of_element
    @@ Dialog.Markup.create_content ~content:[hotkeys#markup] () in
  Dialog.make ~title ~content ~actions:[cancel] ()

let make_menu ?body ?viewport () =
  let hotkeys_item =
    let graphic = Icon.SVG.(make_simple Path.keyboard) in
    Item_list.Item.make ~role:"menuitem" ~graphic "Горячие клавиши" in
  let items =
    [ hotkeys_item
    ] in
  Menu.make_of_item_list ?body ?viewport (Item_list.make ~role:"menu" items)

let tie_menu_with_toggle (scaffold : Scaffold.t) =
  match Element.query_selector scaffold#root Selectors.menu_icon with
  | None -> ()
  | Some i ->
    let icon = Icon_button.attach i in
    let menu = make_menu
        ~body:scaffold#app_content_inner
        ~viewport:(Element scaffold#app_content_inner)
        () in
    menu#set_quick_open true;
    icon#append_child menu;
    menu#set_anchor_element icon#root;
    menu#set_anchor_corner Bottom_left;
    let click = Events.clicks icon#root (fun e _ ->
        let target = Dom.eventTarget e in
        if not @@ Element.contains menu#root target
        && not @@ Element.equal menu#root target
        then menu#reveal ()
        else Lwt.return_unit) in
    let selected = Lwt_js_events.(
        seq_loop (make_event Menu.Event.selected)
          menu#root  (fun e _ ->
              let detail = Js.Opt.get e##.detail (fun () -> failwith "No detail in event") in
              match detail##.index with
              | 0 ->
                let dialog = make_hotkeys_dialog () in
                Dom.appendChild Dom_html.document##.body dialog#root;
                dialog#open_await ()
                >>= fun _ ->
                dialog#destroy ();
                Element.remove_child_safe Dom_html.document##.body dialog#root;
                Lwt.return_unit
              | _ -> Lwt.return_unit)) in
    menu#set_on_destroy (fun () ->
        icon#destroy ();
        Lwt.cancel click;
        Lwt.cancel selected)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let player = match scaffold#body with
    | None -> failwith "no video player element found"
    | Some x -> Player.attach x#root in
  tie_side_sheet_with_toggle scaffold;
  tie_menu_with_toggle scaffold;
  Lwt.async (fun () ->
      RTC.start_webrtc player
      >>= function
      | Ok (_ : RTC.t) -> Lwt.return player#root##focus
      | Error e ->
        (* Show error overlay in case of failure while starting webrtc session *)
        let ph = Ui_templates.Placeholder.Err.make ~text:e () in
        ph#add_class Player.CSS.overlay;
        player#append_child ph;
        Lwt.return_unit)
