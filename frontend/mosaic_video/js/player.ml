open Js_of_ocaml
open Containers
open Components
open Tyxml_js

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

module Fullscreen = struct

  let enter (elt : #Dom.node Js.t) : unit =
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

  let cancel (elt : #Dom.node Js.t) : unit =
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

end

module Markup = Page_mosaic_video_tyxml.Player.Make(Xml)(Svg)(Html)

module Controls = struct

  module Selectors = struct
    let play = "." ^ Markup.CSS.Controls.action_play
    let fullscreen = "." ^ Markup.CSS.Controls.action_fullscreen
  end

  class t (elt : #Dom_html.element Js.t) () =
  object

    val mutable fullscreen = None
    val mutable play = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ()

    method! initial_sync_with_dom () : unit =
      let play' =
        Option.map Icon_button.attach
        @@ Element.query_selector elt Selectors.play in
      let fullscreen' =
        Option.map Icon_button.attach
        @@ Element.query_selector elt Selectors.fullscreen in
      play <- play';
      fullscreen <- fullscreen'

    method play_button : Icon_button.t option =
      play

    method fullscreen_button : Icon_button.t option =
      fullscreen

  end

  let attach (elt : #Dom_html.element Js.t) : t =
    new t elt ()

end

class t (elt : #Dom_html.element Js.t) () =
  let (video_elt : Dom_html.videoElement Js.t) =
    match Element.query_selector elt ("." ^ Markup.CSS.video) with
    | Some x -> Js.Unsafe.coerce x
    | None -> failwith "no video element found" in
  let (controls : Controls.t option) =
    match Element.query_selector elt ("." ^ Markup.CSS.Controls.root) with
    | None -> None
    | Some x -> Some (Controls.attach x) in
  let (play_button : Icon_button.t option) =
    Option.flat_map (fun x -> x#play_button) controls in
  let (fullscreen_button : Icon_button.t option) =
    Option.flat_map (fun x -> x#fullscreen_button) controls in
  let video = Widget.create video_elt in
  object(self)

    val mutable fullscreen_handler = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      self#set_controls false;
      let doc = Dom_html.document in
      (* Attach DOM event listeners *)
      begin match play_button with
      | None -> ()
      | Some btn ->
         btn#set_on_change (function
             | true -> self#play ()
             | false -> self#pause ())
      end;
      begin match fullscreen_button with
      | None -> ()
      | Some btn ->
         btn#set_on_change (function
             | true -> Fullscreen.enter super#root
             | false -> Fullscreen.cancel doc)
      end;
      video#listen_lwt' Widget.Event.play (fun _ _ ->
          Option.iter (fun x -> x#set_on true) play_button;
          Lwt.return_unit);
      video#listen_lwt' Widget.Event.pause (fun _ _ ->
          Option.iter (fun x -> x#set_on false) play_button;
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
      | None -> Option.iter (fun x -> x#set_on false) fullscreen_button
      | Some (elt : Dom_html.element Js.t) ->
         Option.iter (fun x ->
             x#set_on (Element.equal elt super#root)) fullscreen_button
      end;
      true

  end

let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
