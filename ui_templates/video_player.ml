open Js_of_ocaml
open Components

let get_boolean_attr ?(default = false)
      (elt : #Dom_html.element Js.t)
      (attr : string) : bool =
  match Js.Opt.to_option (elt##getAttribute (Js.string attr)) with
  | None -> default
  | Some x -> bool_of_string @@ Js.to_string x

let set_boolean_attr (elt : #Dom_html.element Js.t)
      (attr : string) (v : bool) : unit =
  elt##setAttribute (Js.string attr) (Js.string @@ string_of_bool v)

module Markup = struct
  open Tyxml_js.Html
  open Components_tyxml.Utils

  module CSS = struct

    let root = "mdc-video-player"
    let video = CSS.add_element root "video"
    let gradient = root ^ "-controls-gradient"
    let controls = root ^ "-controls"
    let controls_section = CSS.add_element controls "section"

    let theater = CSS.add_modifier root "theater"
    let controls_section_start = CSS.add_modifier controls_section "align-start"
    let controls_section_end = CSS.add_modifier controls_section "align-end"

  end

  let create_video ?(classes = []) ?attrs
        ?(autoplay = false)
        ?(playsinline = false)
        ?(controls = true)
        () : 'a elt =
    video ~a:([a_class classes]
              |> cons_if_lazy controls a_controls
              |> cons_if_lazy autoplay a_autoplay
              |> cons_if_lazy playsinline (fun () ->
                     Unsafe.string_attrib "playsinline" "true")
              <@> attrs) []

  let create ?(classes = []) ?attrs ?(theater_mode = false)
        ~video () : 'a elt =
    let classes =
      classes
      |> cons_if theater_mode CSS.theater
      |> List.cons CSS.root in
    div ~a:([a_class classes] <@> attrs) [video]

end

let make_controls_gradient () =
  let gradient = Widget.create_div () in
  gradient#add_class Markup.CSS.gradient;
  gradient

module Controls = struct

  let make_play () : Icon_button.t =
    let play_icon = Icon.SVG.(create_simple Path.play) in
    let pause_icon = Icon.SVG.(create_simple Path.pause) in
    Icon_button.make ~icon:play_icon ~on_icon:pause_icon ()

  let make_fullscreen () : Icon_button.t =
    let icon = Icon.SVG.(create_simple Path.fullscreen) in
    let on_icon = Icon.SVG.(create_simple Path.fullscreen_exit) in
    Icon_button.make ~icon ~on_icon ()

  let make_section ?(align : [`Start | `End] option) ?widgets () : Widget.t =
    let section = Widget.create_div ?widgets () in
    section#add_class Markup.CSS.controls_section;
    begin match align with
    | None -> ()
    | Some `End -> section#add_class Markup.CSS.controls_section_end
    | Some `Start -> section#add_class Markup.CSS.controls_section_start
    end;
    section

  class t () =
    let play = make_play () in
    let fullscreen = make_fullscreen () in
    let left_section = make_section ~align:`Start ~widgets:[play] () in
    let right_section = make_section ~align:`End ~widgets:[fullscreen] () in
    object

      inherit Hbox.t ~widgets:[left_section; right_section] () as super

      method! init () : unit =
        super#init ();
        super#add_class Markup.CSS.controls

      method play_button : Icon_button.t =
        play

    end

  let make () : t =
    new t ()

end

class t (elt : #Dom_html.element Js.t) () =
  let gradient = make_controls_gradient () in
  let controls = Controls.make () in
  let (video_elt : Dom_html.videoElement Js.t) =
    match Element.query_selector elt ("." ^ Markup.CSS.video) with
    | Some x -> Js.Unsafe.coerce x
    | None -> failwith "no video element found" in
  let video = Widget.create video_elt in
  object(self)

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      super#append_child video;
      super#append_child gradient;
      super#append_child controls;
      self#set_controls false;
      (* Add event listeners *)
      controls#play_button#set_on_change (function
          | true -> self#play ()
          | false -> self#pause ());
      video#listen_lwt' Widget.Event.play (fun _ _ ->
          print_endline "play";
          Lwt.return_unit);
      video#listen_lwt' Widget.Event.playing (fun _ _ ->
          print_endline "playing";
          Lwt.return_unit);

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

  end

let make ?autoplay () : t =
  let video = Markup.create_video ?autoplay () in
  let (elt : Element.t) =
    Markup.create ~video ()
    |> Tyxml_js.To_dom.of_element in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
