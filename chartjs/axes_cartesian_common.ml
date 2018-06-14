open Containers
open Base
open Axes_tick_common
open Axes_scale_label

type _ numeric =
  | Int   : int numeric
  | Int32 : int32 numeric
  | Int64 : int64 numeric
  | Float : float numeric

type (_,_) time =
  | Ptime : (Common.Time.t,Common.Time.span) time

type position = [`Top | `Left | `Bottom | `Right]

let position_to_string = function
  | `Top -> "top" | `Left -> "left" | `Bottom -> "bottom" | `Right -> "right"
let position_of_string_exn = function
  | "top" -> `Top | "left" -> `Left | "bottom" -> `Bottom | "right" -> `Right
  | _     -> failwith "bad position string"

type (_,_) axis =
  | Linear      : 'a numeric   * 'a option -> ('a,'a) axis
  | Logarithmic : 'a numeric   * 'a option -> ('a,'a) axis
  | Time        : ('a,'b) time * 'b option -> ('a,'b) axis
  | Category    : string list              -> (string,string) axis

let axis_to_type_string : type a b. (a,b) axis -> string = function
  | Linear _      -> "linear"
  | Logarithmic _ -> "logarithmic"
  | Category _    -> "category"
  | Time _        -> "time"

let axis_to_cmp (type a b) (t:(a,b) axis) : (a -> a -> int) =
  match t with
  | Linear (Int,_)        -> compare
  | Linear (Int32,_)      -> Int32.compare
  | Linear (Int64,_)      -> Int64.compare
  | Linear (Float,_)      -> Float.compare
  | Logarithmic (Int,_)   -> compare
  | Logarithmic (Int32,_) -> Int32.compare
  | Logarithmic (Int64,_) -> Int64.compare
  | Logarithmic (Float,_) -> Float.compare
  | Time (Ptime,_)        -> Ptime.compare
  | Category _            -> (fun _ _ -> 0)

let axis_new_min_value (type a b) ~(max:a) ~(delta:b) (t:(a,b) axis) : a =
  match t with
  | Linear (Int,_)        -> max - delta
  | Linear (Int32,_)      -> Int32.sub max delta
  | Linear (Int64,_)      -> Int64.(max - delta)
  | Linear (Float,_)      -> max -. delta
  | Logarithmic (Int,_)   -> max - delta
  | Logarithmic (Int32,_) -> Int32.sub max delta
  | Logarithmic (Int64,_) -> Int64.(max - delta)
  | Logarithmic (Float,_) -> max -. delta
  | Time (Ptime,_)        -> Option.get_or ~default:Ptime.epoch @@ Ptime.sub_span max delta
  | Category _            -> ""

let axis_max_value (type a b) (t:(a,b) axis) (data:a list) : a option =
  let f ~cmp x =
    List.fold_left (fun acc x ->
        match acc with
        | None     -> Some x
        | Some acc -> if cmp x acc = 1 then Some x else Some acc) None x
  in
  match t with
  | Linear _       -> f ~cmp:(axis_to_cmp t) data
  | Logarithmic _  -> f ~cmp:(axis_to_cmp t) data
  | Time (Ptime,_) -> f ~cmp:(axis_to_cmp t) data
  | Category _     -> None

let axis_value_to_string (type a b) (x:a) : (a,b) axis -> string = function
  | Linear (Int,_)        -> string_of_int x
  | Linear (Int32,_)      -> Int32.to_string x
  | Linear (Int64,_)      -> Int64.to_string x
  | Linear (Float,_)      -> string_of_float x
  | Logarithmic (Int,_)   -> string_of_int x
  | Logarithmic (Int32,_) -> Int32.to_string x
  | Logarithmic (Int64,_) -> Int64.to_string x
  | Logarithmic (Float,_) -> string_of_float x
  | Time (Ptime,_)        -> Ptime.to_rfc3339 ~frac_s:12 x
  | Category _            -> x

let axis_value_to_js (type a b) (t:(a,b) axis) : (a -> 'a Js.t) =
  match t with
  | Linear (Int,_)        -> float_of_int %> Js.number_of_float %> Js.Unsafe.coerce
  | Linear (Int32,_)      -> Int32.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Linear (Int64,_)      -> Int64.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Linear (Float,_)      -> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (Int,_)   -> float_of_int %> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (Int32,_) -> Int32.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (Int64,_) -> Int64.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (Float,_) -> Js.number_of_float %> Js.Unsafe.coerce
  | Time (Ptime,_)        -> (fun t ->
    (Ptime.to_float_s t *. 1000.) |> (Js.number_of_float %> Js.Unsafe.coerce))
  | Category _            -> Js.string %> Js.Unsafe.coerce

let axis_value_of_js (type a b) (t:(a,b) axis) : ('a Js.t -> a) =
  match t with
  | Linear (Int,_)        -> Js.Unsafe.coerce %> Js.float_of_number %> int_of_float
  | Linear (Int32,_)      -> Js.Unsafe.coerce %> Js.float_of_number %> Int32.of_float
  | Linear (Int64,_)      -> Js.Unsafe.coerce %> Js.float_of_number %> Int64.of_float_exn
  | Linear (Float,_)      -> Js.Unsafe.coerce %> Js.float_of_number
  | Logarithmic (Int,_)   -> Js.Unsafe.coerce %> Js.float_of_number %> int_of_float
  | Logarithmic (Int32,_) -> Js.Unsafe.coerce %> Js.float_of_number %> Int32.of_float
  | Logarithmic (Int64,_) -> Js.Unsafe.coerce %> Js.float_of_number %> Int64.of_float_exn
  | Logarithmic (Float,_) -> Js.Unsafe.coerce %> Js.float_of_number
  | Time (Ptime,_)        -> (fun t -> (Js.Unsafe.coerce %> Js.float_of_number) t
                                       |> (fun x -> x /. 1000.)
                                       |> Ptime.of_float_s
                                       |> function Some x -> x | None -> assert false)
  | Category _            -> Js.Unsafe.coerce %> Js.to_string

let axis_delta : type a b. (a,b) axis -> b option = function
  | Linear (_,x)      -> x
  | Logarithmic (_,x) -> x
  | Time (_,x)        -> x
  | Category _        -> None

module Tick = struct

  class type t_js =
    object
      inherit tick_common_js
      method autoSkip        : bool Js.t Js.prop
      method autoSkipPadding : int Js.prop
      method labelOffset     : int Js.prop
      method maxRotation     : int Js.prop
      method minRotation     : int Js.prop
      method mirror          : bool Js.t Js.prop
      method padding         : int Js.prop
    end

  class virtual t () = object(self)
    inherit tick_common ()
    val mutable virtual _obj : #t_js Js.t

    (** If true, automatically calculates how many labels
        that can be shown and hides labels accordingly.
        Turn it off to show all labels no matter what *)
    method auto_skip : bool = Js.to_bool _obj##.autoSkip
    method set_auto_skip x = _obj##.autoSkip := Js.bool x

    (** Padding between the ticks on the horizontal axis when autoSkip is enabled.
        Note: Only applicable to hypotorizontal scales. *)
    method auto_skip_padding : int = _obj##.autoSkipPadding
    method set_auto_skip_padding x = _obj##.autoSkipPadding := x

    (** Distance in pixels to offset the label from the centre point
        of the tick (in the y direction for the x axis, and the x direction for the y axis).
        Note: this can cause labels at the edges to be cropped by the edge of the canvas *)
    method label_offset : int = _obj##.labelOffset
    method set_label_offset x = _obj##.labelOffset := x

    (** Maximum rotation for tick labels when rotating to condense labels.
        Note: Rotation doesn't occur until necessary.
        Note: Only applicable to horizontal scales. *)
    method max_rotation : int = _obj##.maxRotation
    method set_max_rotation x = _obj##.maxRotation := x

    (** Minimum rotation for tick labels.
        Note: Only applicable to horizontal scales. *)
    method min_rotation : int = _obj##.minRotation
    method set_min_rotation x = _obj##.minRotation := x

    (** Flips tick labels around axis, displaying the labels
        inside the chart instead of outside.
        Note: Only applicable to vertical scales. *)
    method mirror : bool = Js.to_bool _obj##.mirror
    method set_mirror x = _obj##.mirror := Js.bool x

    (** Padding between the tick label and the axis.
        When set on a vertical axis, this applies in the horizontal (X) direction.
        When set on a horizontal axis, this applies in the vertical (Y) direction. *)
    method padding : int = _obj##.padding
    method set_padding x = _obj##.padding := x

    initializer
      self#set_auto_skip true;
      self#set_auto_skip_padding 0;
      self#set_label_offset 0;
      self#set_max_rotation 50;
      self#set_min_rotation 0;
      self#set_mirror false;
      self#set_padding 10
  end

end

class type t_js =
  object
    method type_      : Js.js_string Js.t Js.prop
    method position   : Js.js_string Js.t Js.prop
    method offset     : bool Js.t Js.prop
    method id         : Js.js_string Js.t Js.prop
    method gridLines  : Axes_grid_line.t_js Js.t Js.prop
    method scaleLabel : Axes_scale_label.t_js Js.t Js.prop

    (* FIXME specific to bar chart only, make as functor? *)
    method barPercentage      : float Js.prop
    method categoryPercentage : float Js.prop
    method barThickness       : int Js.optdef_prop
    method maxBarThickness    : int Js.optdef_prop
    method stacked            : bool Js.t Js.optdef_prop
  end

class t_base ~id ~position ~(axis:(_,_) axis) (o:'c Js.t) () =
object(self)
  constraint 'c = #t_js
  inherit base_option o () as super

  val _grid_lines  = new Axes_grid_line.t ()
  val _scale_label = new Axes_scale_label.t ()

  (** Position of the axis in the chart. Possible values are: Top, Left, Bottom, Right *)
  method position : position = position_of_string_exn @@ Js.to_string _obj##.position
  method set_position (x:position) = _obj##.position := Js.string @@ position_to_string x

  (** If true, extra space is added to the both edges and the axis is scaled
        to fit into the chart area. This is set to true in the bar chart by default. *)
  method offset : bool = Js.to_bool _obj##.offset
  method set_offset x = _obj##.offset := Js.bool x

  (** The ID is used to link datasets and scale axes together. *)
  method id : string = Js.to_string _obj##.id

  (** Grid line configuration. *)
  method grid_lines : Axes_grid_line.t = _grid_lines

  (** Scale title configuration. *)
  method scale_label : Axes_scale_label.t = _scale_label

  (* FIXME specific for bar chart only !!! *)
  method bar_percentage = _obj##.barPercentage
  method set_bar_percentage x = _obj##.barPercentage := x

  method category_percentage = _obj##.categoryPercentage
  method set_category_percentage x = _obj##.categoryPercentage := x

  method bar_thickness = Js.Optdef.to_option _obj##.barThickness
  method set_bar_thickness x = _obj##.barThickness := x

  method max_bar_thickness = Js.Optdef.to_option _obj##.maxBarThickness
  method set_max_bar_thickness x = _obj##.maxBarThickness := x

  method stacked = Option.map Js.to_bool @@ Js.Optdef.to_option _obj##.stacked
  method set_stacked x = _obj##.stacked := Js.bool x

  method coerce_base : t_base = (self :> t_base)

  method! replace x = super#replace x;
                      self#grid_lines#replace _obj##.gridLines;
                      self#scale_label#replace _obj##.scaleLabel

  initializer
    _obj##.type_ := Js.string @@ axis_to_type_string axis;
    self#set_position position;
    self#set_offset false;
    _obj##.id := Js.string id;
    _obj##.gridLines  := Js.Unsafe.coerce self#grid_lines#get_obj;
    _obj##.scaleLabel := Js.Unsafe.coerce self#scale_label#get_obj;
    (* FIXME specific for bar chart only !!! *)
    self#set_bar_percentage 0.9;
    self#set_category_percentage 0.8
end

class virtual ['a,'b] t ~id ~position ~(axis:('a,'b) axis) (o:'c Js.t) () =
  let s_max,s_max_push = React.S.create None in
  object(self)

    constraint 'c = #t_js
    inherit t_base ~id ~position ~axis o () as super

    method virtual max     : 'a option
    method virtual set_max : 'a option -> unit
    method virtual min     : 'a option
    method virtual set_min : 'a option -> unit

    method delta : 'b option = axis_delta axis

    method s_max : 'a option React.signal = s_max
    method update_max (x:'a option) : unit =
      (match x,self#delta with
       | Some max, Some d -> let min = axis_new_min_value ~max ~delta:d axis in
                             self#set_min (Some min);
                             self#set_max (Some max)
       | _ -> ());
      s_max_push x

    method cmp_value : 'a -> 'a -> int = axis_to_cmp axis

    method get_max_value = axis_max_value axis

    method coerce_common : ('a,'b) t = (self :> ('a,'b) t)

    method value_to_js (x:'a) : Line_types.axis_value_js Js.t = axis_value_to_js axis x
    method value_of_js (x:Line_types.axis_value_js Js.t) : 'a = axis_value_of_js axis x

  end
