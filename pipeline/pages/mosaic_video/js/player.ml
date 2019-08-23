open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js
open Js_of_ocaml_lwt
open Components

let ( >>= ) = Lwt.( >>= )

let fullscreen_enabled = Fullscreen.is_enabled ()

let autohide_timeout = 2.

let get_boolean_attr ?(default = false)
    (elt : #Dom_html.element Js.t)
    (attr : string) : bool =

  match Js.Opt.to_option (elt##getAttribute (Js.string attr)) with
  | None -> default
  | Some x -> bool_of_string @@ Js.to_string x

let set_boolean_attr (elt : #Dom_html.element Js.t)
    (attr : string) (v : bool) : unit =
  elt##setAttribute (Js.string attr) (Js.string @@ string_of_bool v)

include Page_mosaic_video_tyxml.Player
module Markup = Make(Xml)(Svg)(Html)

module Selectors = struct
  let video = "." ^ CSS.video
  let audio = "." ^ CSS.audio

  module Controls = struct
    let action_icon =
      Printf.sprintf ".%s:not(.%s)"
        Icon_button.CSS.icon
        Icon_button.CSS.icon_on
    let play = "." ^ CSS.Controls.action_play
    let fullscreen = "." ^ CSS.Controls.action_fullscreen
    let mute = "." ^ CSS.Controls.action_mute
    let volume = "." ^ CSS.Controls.volume
  end

  module State_overlay = struct
    let root = "." ^ CSS.state_overlay
    let icon = "." ^ CSS.state_overlay_icon
  end
end

let icon_of_volume ?(muted = false) (vol : float) : string =
  if vol <= 0. || muted
  then Path.volume_off
  else if vol < 0.33
  then Path.volume_low
  else if vol < 0.67
  then Path.volume_medium
  else Path.volume_high

module Action = struct

  class t (elt : #Dom_html.element Js.t) () =
    object
      inherit Icon_button.t elt ()

      method! initial_sync_with_dom () : unit =
        ()

    end

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Js.Unsafe.coerce elt) ()

end

module State_overlay = struct

  class t (elt : #Dom_html.element Js.t) () =
    object(self)
      val icon : Icon.SVG.t =
        match Element.query_selector elt Selectors.State_overlay.icon with
        | None -> failwith "no state overlay icon element found"
        | Some x -> Icon.SVG.attach x

      val mutable _animation_ended = true
      val mutable _on_animationend = None

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        self#hide ();
        let listener =
          Lwt_js_events.seq_loop
            (Lwt_js_events.make_event Dom_html.Event.animationend)
            super#root (fun _ _ ->
                _animation_ended <- true;
                self#hide ();
                Lwt.return_unit) in
        _on_animationend <- Some listener

      method! destroy () : unit =
        Option.iter Lwt.cancel _on_animationend;
        _on_animationend <- None;
        super#destroy ();

      method show ?(path : string option) () : unit =
        (match path with
         | None -> ()
         | Some path ->
           match icon#paths with
           | [x] -> x##setAttribute (Js.string "d") (Js.string path)
           | _ -> ());
        if not _animation_ended
        then (self#hide ();
              (* Trigger reflow *)
              ignore super#root##getBoundingClientRect);
        super#root##.style##.display := Js.string "";
        _animation_ended <- false;

      method private hide () : unit =
        super#root##.style##.display := Js.string "none"

    end

  let attach (elt : #Dom_html.element Js.t) : t =
    new t elt ()
end

let make_big_button () =
  let icon = Icon.SVG.(make_simple Path.play)#root in
  let ph = Components_lab.Placeholder.With_icon.make ~icon ~text:"" () in
  ph#add_class CSS.big_button;
  ph

class t (elt : #Dom_html.element Js.t) () =
  let (video_elt : Dom_html.videoElement Js.t) =
    match Element.query_selector elt ("." ^ CSS.video) with
    | Some x -> Js.Unsafe.coerce x
    | None -> failwith "no video element found" in
  let (audio_elt : Dom_html.audioElement Js.t) =
    match Element.query_selector elt ("." ^ CSS.audio) with
    | Some x -> Js.Unsafe.coerce x
    | None -> failwith "no audio element found" in
  let (state_overlay : State_overlay.t option) =
    Option.map State_overlay.attach
    @@ Element.query_selector elt ("." ^ CSS.state_overlay_wrapper) in
  let video = Widget.create video_elt in
  let audio = Widget.create audio_elt in
  object(self)

    (* DOM nodes *)
    val mutable _progress = None
    val mutable _controls = None
    (* DOM event handlers *)
    val mutable fullscreen_handlers = []
    val mutable _listeners = []

    (* Timers *)
    val mutable _move_timer = None

    val mutable _video_playing = false
    val mutable _video_can_play = false
    val mutable _audio_can_play = false

    val mutable _last_volume : float = 1.

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      let cons x =
        _listeners <- x :: _listeners in
      (* Attach controls *)
      let (controls : controls option) =
        match Element.query_selector elt ("." ^ CSS.Controls.root) with
        | None -> None
        | Some x -> Some (new controls (self :> t) x ()) in
      _controls <- controls;
      (* Handle events *)
      cons @@ Lwt_js_events.keydowns ~use_capture:true super#root
        self#handle_keydown;
      cons @@ Lwt_js_events.seq_loop
        (Lwt_js_events.make_event @@ Dom_html.Event.make "mouseenter")
        super#root
        (fun _ _ ->
           if not self#paused then super#remove_class CSS.autohide;
           Lwt.return_unit);
      cons @@ Events.mousemoves super#root (fun _ _ ->
          self#remove_class CSS.autohide;
          if not self#paused then self#set_move_timer ();
          Lwt.return_unit);
      cons @@ Lwt_js_events.seq_loop
        (Lwt_js_events.make_event @@ Dom_html.Event.make "mouseleave")
        super#root
        (fun _ _ ->
           if not self#paused then super#add_class CSS.autohide;
           Lwt.return_unit);
      (* Single-click toggles play *)
      cons @@ Lwt_js_events.clicks super#root (fun _ _ ->
          if _video_can_play then self#toggle_play ();
          Lwt.return_unit);
      (* Double-click toggles fullscreen mode *)
      if fullscreen_enabled
      then cons @@ Lwt_js_events.dblclicks super#root (fun _ _ ->
          self#toggle_fullscreen ();
          Lwt.return_unit);
      (* Listen to 'loadstart' event *)
      cons @@ Lwt_js_events.loadstarts video#root (fun _ _ ->
          let progress =
            Components_lab.Placeholder.Progress.make
              ~size:60
              ~text:"Загружаем видео"
              () in
          self#set_overlay progress;
          Lwt.return_unit);
      (* Listen to 'loadedmetadata' event *)
      cons @@ Events.loadedmetadatas audio#root (fun _ _ ->
          _audio_can_play <- true;
          if _video_playing then self#audio_element##play;
          Lwt.return_unit);
      cons @@ Events.loadedmetadatas video#root (fun _ _ ->
          self#set_video_can_play ();
          self#set_overlay @@ make_big_button ();
          Lwt.return_unit);
      (* Listen to 'canplay' event. Fired when content has been loaded *)
      cons @@ Events.canplays audio#root (fun _ _ ->
          _audio_can_play <- true;
          if _video_playing then self#audio_element##play;
          Lwt.return_unit);
      cons @@ Events.canplays video#root (fun _ _ ->
          if not (self#has_overlay ())
          then self#set_overlay @@ make_big_button ();
          self#set_video_can_play ();
          Lwt.return_unit);
      (* Listen to 'play' event *)
      cons @@ Events.plays video#root (fun _ _ ->
          if _video_can_play then (
            self#remove_overlay ();
            Option.iter (fun (x : Icon_button.t) ->
                x#toggle ~force:true ()) self#play_button);
          Lwt.return_unit);
      (* Listen to 'playing' event *)
      cons @@ Events.playings video#root (fun _ _ ->
          if _video_can_play then (
            self#remove_overlay ();
            self#set_move_timer ();
            _video_playing <- true;
            Option.iter (fun (x : Icon_button.t) ->
                x#toggle ~force:true ()) self#play_button;
            if _audio_can_play then self#audio_element##play);
          Lwt.return_unit);
      (* Listen to 'pause' event *)
      cons @@ Events.pauses video#root (fun _ _ ->
          _video_playing <- false;
          if _audio_can_play then self#audio_element##pause;
          super#remove_class CSS.autohide;
          self#clear_move_timer ();
          Option.iter (fun (x : Icon_button.t) ->
              x#toggle ~force:false ()) self#play_button;
          Lwt.return_unit);
      (* Listen to 'volumechange' event *)
      cons @@ Events.volumechanges video#root (fun _ _ ->
          begin match self#volume_slider with
            | None -> ()
            | Some (s : Slider.t) ->
              if self#muted
              then s#set_value 0.
              else s#set_value (100. *. self#volume)
          end;
          match self#mute_button with
          | None -> Lwt.return_unit
          | Some (x : Icon_button.t) ->
            if self#muted
            then x#toggle ~force:true ()
            else (
              x#toggle ~force:false ();
              let path = icon_of_volume self#volume in
              match Element.query_selector x#root Selectors.Controls.action_icon with
              | None -> ()
              | Some action ->
                match Element.query_selector action "path" with
                | None -> ()
                | Some e -> Element.set_attribute e "d" path);
            Lwt.return_unit);
      (* Listen to 'fullscreenchange' event *)
      let fs_handlers =
        List.map (fun typ ->
            Lwt_js_events.seq_loop
              (Lwt_js_events.make_event (Dom_html.Event.make typ))
              Dom_html.document
              self#handle_fullscreenchange)
          Fullscreen.events in
      fullscreen_handlers <- fs_handlers;

    method! destroy () : unit =
      super#destroy ();
      (* Detach DOM event listeners *)
      List.iter Lwt.cancel fullscreen_handlers;
      fullscreen_handlers <- [];

    method video_element : Dom_html.videoElement Js.t =
      video_elt

    method audio_element : Dom_html.audioElement Js.t =
      audio_elt

    method fullscreen : bool =
      match fullscreen_enabled with
      | false -> false
      | true -> Fullscreen.is_fullscreen ()

    method set_fullscreen (x : bool) : unit =
      match fullscreen_enabled with
      | false -> Option.iter Lwt.cancel _move_timer;
      | true ->
        (if not (x = self#fullscreen)
         then (if x then Fullscreen.enter super#root
               else Fullscreen.cancel ()));
        self#on_action ()

    method toggle_fullscreen () : unit =
      if fullscreen_enabled
      then self#set_fullscreen (not self#fullscreen)

    method play ?(show_overlay = true) () : unit =
      self#video_element##play;
      if _audio_can_play then self#audio_element##play;
      self#on_action ();
      (* Show overlay if needed *)
      match show_overlay, state_overlay with
      | false, _ | _, None -> ()
      | true, Some (x : State_overlay.t) ->
        x#show ~path:Path.play ()

    method pause ?(show_overlay = true) () : unit =
      self#video_element##pause;
      self#audio_element##pause;
      self#on_action ();
      (* Show overlay if needed *)
      match show_overlay, state_overlay with
      | false, _ | _, None -> ()
      | true, Some (x : State_overlay.t) ->
        x#show ~path:Path.pause ()

    method toggle_play ?show_overlay () : unit =
      if Js.to_bool self#video_element##.paused
      || Js.to_bool self#video_element##.ended
      then self#play ?show_overlay ()
      else self#pause ?show_overlay ()

    method paused : bool =
      Js.to_bool (self#video_element##.paused)

    method muted : bool =
      Js.to_bool (self#video_element##.muted) || self#volume = 0.

    method set_muted ?(show_overlay = true) (x : bool) =
      if not x
      then (
        let to_set =
          if _last_volume < 0.1
          then 0.1 else _last_volume in
        self#video_element##.volume := to_set;
        self#video_element##.muted := Js._false;
        self#audio_element##.volume := to_set;
        self#audio_element##.muted := Js._false)
      else (
        let muted = Js.to_bool self#video_element##.muted in
        _last_volume <- self#video_element##.volume;
        let v = Js.bool @@ not muted in
        self#video_element##.muted := v;
        self#audio_element##.muted := v);
      self#on_action ();
      (* Show overlay if needed *)
      match show_overlay, state_overlay with
      | false, _ | _, None -> ()
      | true, Some (x : State_overlay.t) ->
        x#show ~path:(icon_of_volume ~muted:self#muted self#volume) ()

    method toggle_muted ?show_overlay () : unit =
      self#set_muted ?show_overlay (not self#muted)

    method volume : float =
      self#video_element##.volume

    method set_volume ?(show_overlay = true) (v : float) : unit =
      let v = Float.(min 1. (max v 0.)) in
      self#video_element##.volume := v;
      self#audio_element##.volume := v;
      self#on_action ();
      (* Show overlay if needed *)
      match show_overlay, state_overlay with
      | false, _ | _, None -> ()
      | true, Some (x : State_overlay.t) ->
        x#show ~path:(icon_of_volume v) ()

    method has_overlay () : bool =
      Option.is_some _progress

    method set_overlay : 'a. (#Widget.t as 'a) -> unit =
      fun (w : #Widget.t) ->
      self#remove_overlay ();
      w#add_class CSS.overlay;
      super#append_child w;
      _progress <- Some w#widget

    method remove_overlay () : unit =
      Option.iter (fun w -> super#remove_child w; w#destroy ()) _progress;
      _progress <- None

    (* Private methods *)

    method private set_video_can_play () : unit =
      Option.iter (fun (x : Icon_button.t) -> x#set_disabled false)
        self#play_button;
      _video_can_play <- true

    method private on_action () : unit =
      super#remove_class CSS.autohide;
      self#set_move_timer ();

    method private set_move_timer () : unit =
      self#clear_move_timer ();
      _move_timer <- Some (
          Lwt_js.sleep autohide_timeout
          >>= fun () -> super#add_class CSS.autohide; Lwt.return_unit)

    method private clear_move_timer () : unit =
      Option.iter Lwt.cancel _move_timer;
      _move_timer <- None

    method private play_button : Icon_button.t option =
      match _controls with
      | None -> None
      | Some x -> x#play_button

    method private fullscreen_button : Icon_button.t option =
      match _controls with
      | None -> None
      | Some x -> x#fullscreen_button

    method private mute_button : Icon_button.t option =
      match _controls with
      | None -> None
      | Some x -> x#mute_button

    method private volume_slider : Slider.t option =
      match _controls with
      | None -> None
      | Some x -> x#volume_slider

    method private set_controls (x : bool) : unit =
      self#video_element##.controls := Js.bool x

    method private handle_fullscreenchange _ _ : unit Lwt.t =
      begin match Fullscreen.is_fullscreen () with
        | true ->
          super#add_class CSS.big_mode;
          Option.iter (fun (x : Icon_button.t) ->
              x#toggle ~force:true ()) self#fullscreen_button
        | false ->
          super#remove_class CSS.big_mode;
          Option.iter (fun (x : Icon_button.t) ->
              x#toggle ~force:false ()) self#fullscreen_button
      end;
      Lwt.return_unit

    method private handle_keydown (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      let key = Dom_html.Keyboard_code.of_event e in
      begin match key with
        | ArrowUp ->
          Dom.preventDefault e;
          Dom_html.stopPropagation e;
          let cur = self#volume in
          let vol = min (cur +. 0.05) 1. in
          self#set_volume ~show_overlay:true vol
        | ArrowDown ->
          Dom.preventDefault e;
          Dom_html.stopPropagation e;
          let cur = self#volume in
          let vol = max (cur -. 0.05) 0. in
          self#set_volume ~show_overlay:true vol
        | Space ->
          Dom.preventDefault e;
          if _video_can_play then self#toggle_play ~show_overlay:true ()
        | KeyF -> self#toggle_fullscreen ()
        | KeyM -> self#toggle_muted ~show_overlay:true ()
        | _ -> ()
      end;
      Lwt.return_unit

  end
and controls (t : t) (elt : #Dom_html.element Js.t) () =
  object(self)

    (* React events *)
    val mutable e_volume : unit React.event option = None

    (* DOM nodes *)
    val play =
      Option.map Action.attach
      @@ Element.query_selector elt Selectors.Controls.play
    val fullscreen =
      Option.map Action.attach
      @@ Element.query_selector elt Selectors.Controls.fullscreen
    val mute =
      Option.map Action.attach
      @@ Element.query_selector elt Selectors.Controls.mute
    val volume =
      Option.map Slider.attach
      @@ Element.query_selector elt Selectors.Controls.volume

    (* Event listeners *)
    val mutable _listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      (* Option.iter (fun (a : Action.t) -> a#set_disabled true)
       *   self#play_button; *)
      Option.iter (fun (s : Slider.t) -> s#set_value (100. *. t#volume))
        self#volume_slider;
      (* Add event listeners *)
      let click_play = match self#play_button with
        | None -> None
        | Some btn ->
          Some (Events.clicks btn#root (fun _ _ ->
              t#toggle_play ~show_overlay:false ();
              Lwt.return_unit)) in
      let click_mute = match self#mute_button with
        | None -> None
        | Some btn ->
          Some (Events.clicks btn#root (fun _ _ ->
              t#toggle_muted ~show_overlay:false ();
              Lwt.return_unit)) in
      let dblclick, click_fullscreen = match fullscreen_enabled with
        | false -> None, None
        | true ->
          ( Some (Events.dblclicks super#root (fun e _ ->
                Dom_html.stopPropagation e;
                Lwt.return_unit))
          , match self#fullscreen_button with
          | None -> None
          | Some btn ->
            btn#root##.style##.display := Js.string "block";
            Some (Events.clicks btn#root (fun _ _ ->
                t#toggle_fullscreen ();
                Lwt.return_unit))) in
      let click = Events.clicks super#root (fun e _ ->
          Dom_html.stopPropagation e;
          Lwt.return_unit) in
      let volume_change = match volume with
        | None -> None
        | Some slider ->
          Some (Lwt_js_events.seq_loop
                  (Lwt_js_events.make_event Slider.Event.input)
                  slider#root (fun e _ ->
                      match Js.Opt.to_option e##.detail with
                      | None -> Lwt.return_unit
                      | Some v ->
                        if Js.to_bool t#video_element##.muted
                        then t#video_element##.muted := Js._false;
                        t#set_volume ~show_overlay:false (v /. 100.);
                        Lwt.return_unit)) in
      _listeners <- List.filter_map (fun x -> x)
          [ click_play
          ; click_mute
          ; dblclick
          ; click_fullscreen
          ; Some click
          ; volume_change
          ]

    method! destroy () : unit =
      super#destroy ();
      List.iter Lwt.cancel _listeners;
      _listeners <- [];
      Option.(
        iter Widget.destroy play;
        iter Widget.destroy fullscreen;
        iter Widget.destroy mute;
        iter Widget.destroy volume;
        iter (React.E.stop ~strong:true) e_volume);
      e_volume <- None

    method play_button : Icon_button.t option =
      play

    method fullscreen_button : Icon_button.t option =
      fullscreen

    method mute_button : Icon_button.t option =
      mute

    method volume_slider : Slider.t option =
      volume

  end

let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
