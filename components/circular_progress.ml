open Containers
open Tyxml_js

module Markup = Components_markup.Circular_progress.Make(Xml)(Svg)(Html)

let round x = if Float.(x < (floor x +. 0.5)) then floor x else ceil x

class t ?(max = 1.) ?(min = 0.) ?(value = 0.)
        ?(indeterminate = true) ?(thickness = 3.6) ?(size = 40) () =
  let s_progress, set_progress = React.S.create value in
  let pi = 4.0 *. atan 1.0 in
  let elt = Markup.create ~thickness ~size ()
            |> Tyxml_js.To_dom.of_element in
  object(self)

    inherit Widget.t elt ()

    val circle =
      elt##querySelector (Js.string ("." ^ Markup.circle_class))
      |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce

    val mutable min : float = min
    val mutable max : float = max

    method min : float = min

    method set_min (x : float) : unit =
      min <- x;
      self#set_progress self#progress;
      if not self#indeterminate
      then self#set_attribute "aria-valuemin" (Printf.sprintf "%f" x)

    method max : float = max

    method set_max (x : float) : unit =
      max <- x;
      self#set_progress self#progress;
      if not self#indeterminate
      then self#set_attribute "aria-valuemax" (Printf.sprintf "%f" x)

    method set_indeterminate (x : bool) : unit =
      self#add_or_remove_class x Markup.indeterminate_class;
      if x
      then (circle##.style##.strokeDashoffset := Js.string "";
            circle##.style##.strokeDasharray  := Js.string "")
      else (self#set_min min; self#set_max max; self#set_progress self#progress)

    method indeterminate : bool =
      self#has_class Markup.indeterminate_class

    method set_progress (v : float) : unit =
      let v = if Float.(<) v min then min
              else if Float.(>) v max
              then max else v in
      set_progress v;
      let rel_val = (v -. min) /. (max -. min) *. 100. in
      let circumference = 2. *. pi *. (Markup.sz /. 2. -. 5.) in
      let dash_offset =
        (round ((100. -. rel_val) /. 100. *. circumference *. 1000.))
        /. 1000. in
      let dash_array = (round (circumference *. 1000.)) /. 1000. in
      circle##.style##.strokeDashoffset := Js.string (Printf.sprintf "%fpx" dash_offset);
      circle##.style##.strokeDasharray := Js.string (Printf.sprintf "%f" dash_array);
      if not self#indeterminate
      then self#set_attribute "aria-valuenow" (Printf.sprintf "%f" v)

    method progress : float =
      React.S.value s_progress

    method s_progress : float React.signal =
      s_progress

    method show () : unit =
      self#style##.display := Js.string ""
    method hide () : unit =
      self#style##.display := Js.string "none"

    initializer
      self#set_min min;
      self#set_max max;
      self#set_progress value;
      self#set_indeterminate indeterminate

  end
