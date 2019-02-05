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

module CSS = struct

  let root = "mdc-video-player"

end

class t =
  let play =
    let icon = Icon.SVG.(create_simple Path.play) in
    new Icon_button.t ~icon () in
  let controls = Widget.create_div ~widgets:[play#widget] () in
  object(self)

    inherit Widget.t (failwith "not implemented") () as super

    method! init () : unit =
      super#init ();
      ignore controls;
      super#add_class CSS.root;
      self#set_controls false

    method video_element : Dom_html.videoElement Js.t =
      failwith "not implemented"

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
