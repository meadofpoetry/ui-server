open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Circular_progress
module Markup = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

module Attr = struct
  let now = "aria-valuenow"

  let max = "aria-valuemax"

  let min = "aria-valuemin"
end

module Selector = struct
  let circle = "." ^ CSS.circle
end

let clamp ~min ~max v = Float.min (Float.max v min) max

let get_float_attribute (elt : Dom_html.element Js.t) (a : string) =
  match Element.get_attribute elt a with
  | None -> None
  | Some a -> float_of_string_opt a

class t (elt : #Dom_html.element Js.t) () =
  object (self)
    val circle = Element.query_selector_exn elt Selector.circle

    val mutable _min : float = 0.

    val mutable _max : float = 1.

    val mutable _value : float = 0.

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      let min' =
        match get_float_attribute elt Attr.min with
        | None -> _min
        | Some x -> x
      in
      let max' =
        match get_float_attribute elt Attr.max with
        | None -> _max
        | Some x -> x
      in
      if min' >= self#max
      then (
        self#set_max max';
        self#set_min min')
      else (
        self#set_min min';
        self#set_max max');
      let val' =
        match get_float_attribute elt Attr.now with
        | None -> _value
        | Some x -> x
      in
      self#set_value val'

    method min : float = _min

    method set_min (x : float) : unit =
      if x > self#max
      then raise (Invalid_argument "Min cannot be greater than max")
      else (
        _min <- x;
        self#set_value_ ~force:true self#value;
        super#set_attribute Attr.min (string_of_float x))

    method max : float = _max

    method set_max (x : float) : unit =
      if x < self#min
      then raise (Invalid_argument "Max cannot be less than min")
      else (
        _max <- x;
        self#set_value_ ~force:true self#value;
        super#set_attribute Attr.max (string_of_float x))

    method value : float = _value

    method set_value (v : float) : unit = self#set_value_ v

    method set_indeterminate (x : bool) : unit =
      super#toggle_class ~force:x CSS.indeterminate;
      if x
      then (
        (Js.Unsafe.coerce circle##.style)##.strokeDashoffset := Js.string "";
        (Js.Unsafe.coerce circle##.style)##.strokeDasharray := Js.string "")
      else self#set_value_ ~force:true self#value

    method indeterminate : bool = super#has_class CSS.indeterminate

    method show () : unit = super#root##.style##.display := Js.string ""

    method hide () : unit = super#root##.style##.display := Js.string "none"

    (* Private methods *)
    method private set_value_ ?(force = false) (v : float) : unit =
      let min, max, prev = self#min, self#max, self#value in
      let v = clamp ~min ~max v in
      if force || v <> prev
      then (
        _value <- v;
        super#set_attribute Attr.now (string_of_float v);
        self#update_ui_for_value ())

    method private update_ui_for_value () : unit =
      let min, max, value = self#min, self#max, self#value in
      let rel_val = (value -. min) /. (max -. min) *. 100. in
      let circumference = 2. *. Float.pi *. ((Markup.sz /. 2.) -. 5.) in
      let dash_offset =
        Float.(round ((100. -. rel_val) /. 100. *. circumference *. 1000.) /. 1000.)
      in
      let dash_array = Float.(round (circumference *. 1000.) /. 1000.) in
      let dash_offset' = Js.string (Printf.sprintf "%gpx" dash_offset) in
      let dash_array' = Js.string (Printf.sprintf "%g" dash_array) in
      (Js.Unsafe.coerce circle##.style)##.strokeDashoffset := dash_offset';
      (Js.Unsafe.coerce circle##.style)##.strokeDasharray := dash_array'
  end

let make ?min ?max ?value ?indeterminate ?thickness ?size () : t =
  let (elt : Element.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?min ?max ?value ?indeterminate ?thickness ?size ()
  in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t = new t elt ()
