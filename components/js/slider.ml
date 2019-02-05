open Containers
open Tyxml_js

let string_of_float (f : float) : string =
  Printf.sprintf "%g" f

let quantize ~(step : float) (v : float) : float =
  let steps = Float.round (v /. step) in
  steps *. step

module Markup = Components_tyxml.Slider.Make(Xml)(Svg)(Html)

class t ?discrete ?markers ?step ?(min = 0.0) ?(max = 100.0) ?value  () =

  let elt = Markup.create ?discrete ?markers ?value ?step ~min ~max ()
            |> To_dom.of_div in
  object(self)

    inherit Widget.t elt () as super

    val mutable saved_tab_index = None
    val mutable active = false
    val mutable in_transit = false
    val mutable is_discrete = false
    val mutable has_tracker_marker = false
    val mutable hndling_thumb_target_evt = false

    val mutable min = min
    val mutable max = max
    val mutable step = 0.
    val mutable value = 0.
    val mutable disabled = false
    val mutable prevent_focus_state = false
    val mutable update_ui_frame = 0

    method! init () : unit =
      super#init ()

    method discrete : bool =
      super#has_class Markup.CSS.discrete

    method has_track_marker : bool =
      super#has_class Markup.CSS.display_markers

    method value : float =
      value

    method set_value (v : float) : unit =
      self#set_value_ ~fire_input:false v

    method max : float =
      max

    method set_max (v : float) : unit =
      if v <. self#min
      then raise (Invalid_argument "Max cannot be less then min")
      else (
        max <- v;
        self#set_value_ ~fire_input:false ~force:true self#value;
        super#set_attribute "aria-valuemax" (string_of_float v);
        self#setup_track_marker ())

    method min : float =
      min

    method set_min (v : float) : unit =
      if v >. self#max
      then raise (Invalid_argument "Min cannot be greater then max")
      else (
        min <- v;
        self#set_value_ ~fire_input:false ~force:true self#value;
        super#set_attribute "aria-valuemin" (string_of_float v);
        self#setup_track_marker ())

    method step : float =
      step

    method set_step (v : float) : unit =
      if v <. 0.
      then raise (Invalid_argument "Step cannot be negative")
      else (
        let v = if self#discrete && v <. 1. then 1. else v in
        step <- v;
        self#set_value_ ~fire_input:false ~force:true self#value;
        self#setup_track_marker ())

    method private setup_track_marker () : unit =
      let step = self#step in
      if self#discrete && self#has_track_marker && step <>. 0.
      then
        let markers' = (self#max -. self#min) /. step in
        let markers = ceil markers' in
        self#remove_track_markers ();
        self#append_track_markers (int_of_float markers);
        if markers' <>. markers
        then
          let last_step_ratio = (max -. (markers *. step)) /. step +. 1. in
          ignore last_step_ratio

    method disabled : bool =
      disabled

    method set_disabled (x : bool) : unit =
      disabled <- x;
      self#add_or_remove_class x Markup.CSS.disabled;
      if x
      then (
        (* FIXME add tabindex logic *)
        super#set_attribute "aria-disabled" "true";
        super#remove_attribute "tabindex")
      else (
        (* FIXME add tabindex logic *)
        super#remove_attribute "aria-disabled")

    method set_active (x : bool) : unit =
      active <- x;

    (* Private methods *)

    method private remove_track_markers () : unit =
      ()

    method private append_track_markers (markers : int) : unit =
      ignore markers

    method private update_ui_for_current_value () : unit =
      ()

    method private notify_input () : unit =
      ()

    method private set_marker_value (v : float) : unit =
      ignore v

    method private set_value_ ?(force = false) ?(fire_input = false)
                     (v : float) : unit =
      if force || not (Float.equal v self#value)
      then
        let min = self#min in
        let max = self#max in
        let step = self#step in
        let is_boundary = Float.(v = self#min || v = self#max) in
        let v =
          if is_boundary || Float.equal step 0. then v
          else quantize ~step v in
        let v = if v <. min then min else if v >. max then max else v in
        value <- v;
        super#set_attribute "aria-valuenow" (string_of_float v);
        self#update_ui_for_current_value ();
        if fire_input
        then (self#notify_input ();
              if self#discrete then self#set_marker_value v)

  end
