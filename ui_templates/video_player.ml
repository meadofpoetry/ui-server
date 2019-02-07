open Js_of_ocaml
open Containers
open Components

(* TODO
 * add orientation change handling - open video in fullscreen
 *)

let get_boolean_attr ?(default = false)
      (elt : #Dom_html.element Js.t)
      (attr : string) : bool =
  match Js.Opt.to_option (elt##getAttribute (Js.string attr)) with
  | None -> default
  | Some x -> bool_of_string @@ Js.to_string x

let set_boolean_attr (elt : #Dom_html.element Js.t)
      (attr : string) (v : bool) : unit =
  elt##setAttribute (Js.string attr) (Js.string @@ string_of_bool v)

let set_fullscreen (elt : #Dom.node Js.t) : unit =
  let test = Js.Optdef.test in
  let elt = Js.Unsafe.coerce elt in
  if test elt##.requestFullscreen
  then elt##requestFullscreen
  else if test elt##.mozRequestFullScreen
  then elt##mozRequestFullScreen
  else if test elt##.webkitRequestFullScreen
  then elt##webkitRequestFullScreen
  else if test elt##.msRequestFullscreen
  then elt##msRequestFullscreen

let exit_fullscreen (elt : #Dom.node Js.t) : unit =
  let test = Js.Optdef.test in
  let elt = Js.Unsafe.coerce elt in
  if test elt##.exitFullscreen
  then elt##exitFullscreen
  else if test elt##.mozCancelFullScreen
  then elt##mozCancelFullScreen
  else if test elt##.webkitExitFullScreen
  then elt##webkitExitFullScreen
  else if test elt##.msExitFullscreen
  then elt##msExitFullscreen

module Markup = struct
  open Tyxml_js.Html
  open Components_tyxml.Utils

  module CSS = struct

    let root = "mdc-video-player"
    let video = CSS.add_element root "video"
    let gradient = root ^ "-controls-gradient"

    let theater = CSS.add_modifier root "theater"

    module Controls = struct
      let root = root ^ "-controls"
      let action = CSS.add_element root "action"
      let section = CSS.add_element root "section"

      let section_start = CSS.add_modifier section "align-start"
      let section_end = CSS.add_modifier section "align-end"
      let action_play = CSS.add_modifier action "play"
      let action_fullscreen = CSS.add_modifier action "fullscreen"
    end

  end

  let ( ^:: ) = List.cons_maybe

  module Controls = struct

    type align = [`Start | `End]

    let create_action ?(classes = []) ?attrs ?on_icon icon () : 'a elt =
      let classes = CSS.Controls.action :: classes in
      Icon_button.Markup.create ?attrs ?on_icon ~classes icon ()

    let create_section ?(classes = []) ?attrs ?(align : align option)
          content () : 'a elt =
      let classes =
        classes
        |> map_cons_option (function
               | `End -> CSS.Controls.section_end
               | `Start -> CSS.Controls.section_start) align
        |> List.cons CSS.Controls.section in
      div ~a:([a_class classes] <@> attrs) content

    let create ?(classes = []) ?attrs sections () : 'a elt =
      let classes = CSS.Controls.root :: classes in
      div ~a:([a_class classes] <@> attrs) sections

  end

  let create_video ?(classes = []) ?attrs
        ?(autoplay = false)
        ?(playsinline = false)
        ?(controls = true)
        () : 'a elt =
    let classes = CSS.video :: classes in
    video ~a:([a_class classes]
              |> cons_if_lazy controls a_controls
              |> cons_if_lazy autoplay a_autoplay
              |> cons_if_lazy playsinline (fun () ->
                     Unsafe.string_attrib "playsinline" "true")
              <@> attrs) []

  let create_gradient ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.gradient :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create ?(classes = []) ?attrs ?(theater_mode = false)
        ?controls ?gradient ~video () : 'a elt =
    let classes =
      classes
      |> cons_if theater_mode CSS.theater
      |> List.cons CSS.root in
    div ~a:([a_class classes] <@> attrs)
      (video :: (gradient ^:: controls ^:: []))

end

module Controls = struct

  class t (elt : #Dom_html.element Js.t) () =
  object

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ()

    method play_button : Icon_button.t =
      play

    method fullscreen_button : Icon_button.t =
      fullscreen

  end

  let make () : t =
    new t ()

end

class t (elt : #Dom_html.element Js.t) () =
  let (video_elt : Dom_html.videoElement Js.t) =
    match Element.query_selector elt ("." ^ Markup.CSS.video) with
    | Some x -> Js.Unsafe.coerce x
    | None -> failwith "no video element found" in
  let video = Widget.create video_elt in
  object(self)

    val mutable fullscreen_handler = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      self#set_controls false;
      let doc = Dom_html.document in
      (* Attach DOM event listeners *)
      controls#play_button#set_on_change (function
          | true -> self#play ()
          | false -> self#pause ());
      controls#fullscreen_button#set_on_change (function
          | true -> set_fullscreen super#root
          | false -> exit_fullscreen doc);
      video#listen_lwt' Widget.Event.play (fun _ _ ->
          controls#play_button#set_on true;
          Lwt.return_unit);
      video#listen_lwt' Widget.Event.pause (fun _ _ ->
          controls#play_button#set_on false;
          Lwt.return_unit);
      let fs_handler =
        Dom_events.listen doc Widget.Event.fullscreenchange
          self#handle_fullscreenchange in
      fullscreen_handler <- Some fs_handler;
      video#listen_lwt' Widget.Event.playing (fun _ _ ->
          print_endline "playing";
          Lwt.return_unit);

    method! destroy () : unit =
      super#destroy ();
      (* Detach DOM event listeners *)
      Option.iter Dom_events.stop_listen fullscreen_handler;
      fullscreen_handler <- None;

    method video_element : Dom_html.videoElement Js.t =
      video_elt

    method theater_mode : bool =
      super#has_class Markup.CSS.theater
    method set_theater_mode (x : bool) : unit =
      super#toggle_class ~force:x Markup.CSS.theater

    method play () : unit =
      self#video_element##play

    method pause () : unit =
      self#video_element##pause

    method set_fullscreen : bool -> unit = function
      | true -> (Js.Unsafe.coerce self#video_element)##requestFullscreen
      | false -> (Js.Unsafe.coerce self#video_element)##exitFullscreen

    method autoplay : bool =
      Js.to_bool (self#video_element##.autoplay)
    method set_autoplay (x : bool) : unit =
      self#video_element##.autoplay := Js.bool x

    method playsinline : bool =
      get_boolean_attr self#video_element "playsinline"
    method set_playsinline (x : bool) : unit =
      set_boolean_attr self#video_element "playsinline" x

    method muted : bool =
      Js.to_bool (self#video_element##.muted)
    method set_muted (x : bool) =
      self#video_element##.muted := Js.bool x

    method paused : bool =
      Js.to_bool (self#video_element##.paused)

    method ended : bool =
      Js.to_bool (self#video_element##.ended)

    method duration : float =
      self#video_element##.duration

    method current_time : float =
      self#video_element##.currentTime

    method volume : float =
      self#video_element##.volume
    method set_volume (v : float) : unit =
      self#video_element##.volume := v

    (* Private methods *)

    method private set_controls (x : bool) : unit =
      self#video_element##.controls := Js.bool x

    method private handle_fullscreenchange _ _ : bool =
      let (elt : Dom_html.element Js.t option) =
        Js.Opt.to_option
          (Js.Unsafe.coerce Dom_html.document)##.fullscreenElement in
      begin match elt with
      | None -> controls#fullscreen_button#set_on false
      | Some (elt : Dom_html.element Js.t) ->
         let eq = Element.equal elt super#root in
         controls#fullscreen_button#set_on eq
      end;
      true

  end

let make ?theater_mode ?autoplay ?gradient ?controls () : t =
  let gradient = match gradient, controls with
    | Some x, _ -> Some x
    | None, None -> None
    | None, Some _ -> Some (Markup.create_gradient ()) in
  let video = Markup.create_video ?autoplay () in
  let (elt : Element.t) =
    Markup.create ?theater_mode ?gradient ?controls ~video ()
    |> Tyxml_js.To_dom.of_element in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
