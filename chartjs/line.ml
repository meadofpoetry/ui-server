open Base

type point_js
type cubic_interpolation_mode = Default | Monotone
type stepped_line = Disabled | Before | After
type fill = Disabled
          | Start
          | End
          | Origin
          | Absolute_index of int
          | Relative_index of int
type ('a,'b) point =
  { x : 'a
  ; y : 'b
  }

type ('a,'b) dataset =
  { data                     : ('a,'b) point list
  ; label                    : string
  }

let point_to_js (type a b) (axis:(a,b) Axes.Cartesian.axis) : (a -> point_js Js.t) =
  let open Axes in
  let open Axes.Cartesian in
  let (%>) = CCFun.(%>) in
  match axis with
  | Linear (_,_,Integer)      -> float_of_int %> Js.number_of_float %> Js.Unsafe.coerce
  | Linear (_,_,Float)        -> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (_,_,Integer) -> float_of_int %> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (_,_,Float)   -> Js.number_of_float %> Js.Unsafe.coerce
  | Time (_,_,Unix)           -> Int32.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Category _                -> Js.string %> Js.Unsafe.coerce

let point_of_js (type a b) (axis:(a,b) Axes.Cartesian.axis) : (point_js Js.t -> a) =
  let open Axes in
  let open Axes.Cartesian in
  let (%>) = CCFun.(%>) in
  match axis with
  | Linear (_,_,Integer)      -> Js.Unsafe.coerce %> Js.float_of_number %> int_of_float
  | Linear (_,_,Float)        -> Js.Unsafe.coerce %> Js.float_of_number
  | Logarithmic (_,_,Integer) -> Js.Unsafe.coerce %> Js.float_of_number %> int_of_float
  | Logarithmic (_,_,Float)   -> Js.Unsafe.coerce %> Js.float_of_number
  | Time (_,_,Unix)           -> Js.Unsafe.coerce %> Js.float_of_number %> Int32.of_float
  | Category _                -> Js.Unsafe.coerce %> Js.to_string

module Dataset = struct

  let cubic_interpolation_mode_to_string = function
    | Default -> "default" | Monotone -> "monotone"
  let cubic_interpolation_mode_of_string_exn = function
    | "default" -> Default | "monotone" -> Monotone | _ -> failwith "Bad cubic interpolation mode string"

  type bool_or_string

  type 'a or_array

  class type data_js =
    object
      method x : point_js Js.t Js.prop
      method y : point_js Js.t Js.prop
    end

  class type t_js =
    object
      method data                      : data_js Js.t Js.js_array Js.t Js.prop
      method label                     : Js.js_string Js.t Js.prop
      method xAxisID                   : Js.js_string Js.t Js.optdef_prop
      method yAxisID                   : Js.js_string Js.t Js.optdef_prop
      method backgroundColor           : CSS.Color.js_t Js.optdef_prop
      method borderWidth               : int Js.optdef_prop
      method borderColor               : CSS.Color.js_t Js.optdef_prop
      method borderCapStyle            : Js.js_string Js.t Js.optdef_prop
      method borderDash                : int Js.js_array Js.t Js.optdef_prop
      method borderDashOffset          : int Js.optdef_prop
      method borderJoinStyle           : Js.js_string Js.t Js.optdef_prop
      method cubicInterpolationMode    : Js.js_string Js.t Js.optdef_prop
      method fill                      : bool_or_string Js.t Js.optdef_prop
      method lineTension               : float Js.optdef_prop
      method pointBackgroundColor      : Js.js_string Js.t or_array Js.t Js.optdef_prop
      method pointBorderColor          : Js.js_string Js.t or_array Js.t Js.optdef_prop
      method pointBorderWidth          : int Js.t or_array Js.t Js.optdef_prop
      method pointRadius               : int Js.t or_array Js.t Js.optdef_prop
      method pointStyle                : Js.js_string Js.t or_array Js.t Js.optdef_prop
      method pointHitRadius            : int Js.t or_array Js.t Js.optdef_prop
      method pointHoverBackgroundColor : Js.js_string Js.t or_array Js.t Js.optdef_prop
      method pointHoverBorderColor     : Js.js_string Js.t or_array Js.t Js.optdef_prop
      method pointHoverBorderWidth     : int Js.t or_array Js.t Js.optdef_prop
      method pointHoverRadius          : int Js.t or_array Js.t Js.optdef_prop
      method showLine                  : bool Js.t Js.optdef_prop
      method spanGaps                  : bool Js.t Js.optdef_prop
      method steppedLine               : bool_or_string Js.t Js.optdef_prop
    end

  class ['a,'b] t
                ~(data:('a,'b) dataset)
                ~(x_to_js:'a -> point_js Js.t)
                ~(y_to_js:'b -> point_js Js.t)
                ~(x_of_js:point_js Js.t -> 'a)
                ~(y_of_js:point_js Js.t -> 'b)
                () = object(self)

    inherit [t_js] base_option ()

    method private point_to_js p = object%js
                                     val mutable x = x_to_js p.x
                                     val mutable y = y_to_js p.y
                                     end
    method private point_of_js o = { x = x_of_js o##.x
                                   ; y = y_of_js o##.y
                                   }

    method length = obj##.data##.length

    method get_data : ('a,'b) point list = List.map self#point_of_js (Array.to_list @@ Js.to_array obj##.data)

    method push_back (x:('a,'b) point)  = obj##.data##push (self#point_to_js x)    |> ignore
    method push_front (x:('a,'b) point) = obj##.data##unshift (self#point_to_js x) |> ignore

    method take_back : ('a,'b) point option = obj##.data##pop
                                              |> Js.Optdef.to_option
                                              |> CCOpt.map self#point_of_js
    method take_front : ('a,'b) point option = obj##.data##shift
                                               |> Js.Optdef.to_option
                                               |> CCOpt.map self#point_of_js

    method append_back (x:('a,'b) point list) = obj##.data##concat (List.map self#point_to_js x
                                                                    |> Array.of_list
                                                                    |> Js.array)
    method append_front (x:('a,'b) point list) = (List.map self#point_to_js x
                                                  |> Array.of_list
                                                  |> Js.array)##concat obj##.data

    method set_label x = obj##.label := Js.string x
    method get_label   = Js.to_string obj##.label

    method set_background_color x = obj##.backgroundColor := CSS.Color.js x
    method get_background_color   = CCOpt.map CSS.Color.ml @@ Js.Optdef.to_option obj##.backgroundColor

    method set_border_width x = obj##.borderWidth := x
    method get_border_width   = Js.Optdef.to_option obj##.borderWidth

    method set_border_color x = obj##.borderColor := CSS.Color.js x
    method get_border_color   = CCOpt.map CSS.Color.ml @@ Js.Optdef.to_option obj##.borderColor

    method set_border_cap_style x = obj##.borderCapStyle := Js.string @@ Canvas.line_cap_to_string x
    method get_border_cap_style   = CCOpt.map (Js.to_string %> Canvas.line_cap_of_string_exn)
                                    @@ Js.Optdef.to_option obj##.borderCapStyle

    method set_border_dash x = obj##.borderDash := Js.array @@ Array.of_list x
    method get_border_dash   = CCOpt.map (Js.to_array %> Array.to_list) @@ Js.Optdef.to_option obj##.borderDash

    method set_border_dash_offset x = obj##.borderDashOffset := x
    method get_border_dash_offset   = Js.Optdef.to_option obj##.borderDashOffset

    method set_border_join_style x = obj##.borderJoinStyle := Js.string @@ Canvas.line_join_to_string x
    method get_border_join_style   = CCOpt.map (Js.to_string %> Canvas.line_join_of_string_exn)
                                     @@ Js.Optdef.to_option obj##.borderJoinStyle

    method set_cubic_interpolation_mode x = Js.string @@ cubic_interpolation_mode_to_string x
                                            |> (fun x -> obj##.cubicInterpolationMode := x)
    method get_cubic_interpolation_mode   = CCOpt.map (Js.to_string %> cubic_interpolation_mode_of_string_exn)
                                            @@ Js.Optdef.to_option obj##.cubicInterpolationMode

    method set_fill : fill -> unit = function
      | Disabled         -> obj##.fill := Js.Unsafe.coerce Js._false
      | Start            -> obj##.fill := Js.Unsafe.coerce @@ Js.string "start"
      | End              -> obj##.fill := Js.Unsafe.coerce @@ Js.string "end"
      | Origin           -> obj##.fill := Js.Unsafe.coerce @@ Js.string "origin"
      | Absolute_index x -> obj##.fill := Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int x
      | Relative_index x -> obj##.fill := Js.Unsafe.coerce @@ Js.string (if x > -1 then "+" ^ (string_of_int x)
                                                                         else string_of_int x)
    method get_fill : fill option =
      CCOpt.map (fun fill -> match Cast.to_bool fill with
                             | Some true  -> Origin
                             | Some false -> Disabled
                             | None   -> (match Cast.to_int fill with
                                          | Some i -> Absolute_index i
                                          | None   -> (match Cast.to_string fill with
                                                       | Some "start"  -> Start
                                                       | Some "end"    -> End
                                                       | Some "origin" -> Origin
                                                       | Some s -> Relative_index (int_of_string s)
                                                       | None -> failwith "Bad fill value")))
                (Js.Optdef.to_option obj##.fill)

    method set_line_tension x = obj##.lineTension := x
    method get_line_tension   = Js.Optdef.to_option obj##.lineTension

    method set_show_line x = obj##.showLine := Js.bool x
    method get_show_line   = CCOpt.map Js.to_bool @@ Js.Optdef.to_option obj##.showLine

    method set_span_gaps x = obj##.spanGaps := Js.bool x
    method get_span_gaps   = CCOpt.map Js.to_bool @@ Js.Optdef.to_option obj##.spanGaps

    method set_stepped_line : stepped_line -> unit = function
      | Disabled -> obj##.steppedLine := Js.Unsafe.coerce Js._false
      | Before   -> obj##.steppedLine := Js.Unsafe.coerce @@ Js.string "before"
      | After    -> obj##.steppedLine := Js.Unsafe.coerce @@ Js.string "after"
    method get_stepped_line : stepped_line option =
      CCOpt.map (fun x -> match Cast.to_string x with
                          | Some "before" -> Before
                          | Some "after"  -> After
                          | Some _        -> failwith "Bad stepped line string"
                          | None -> (match Cast.to_bool x with
                                     | Some true  -> Before
                                     | Some false -> Disabled
                                     | None       -> failwith "Bad stepped line value"))
                (Js.Optdef.to_option obj##.steppedLine)

    initializer
      self#set_label data.label;
      obj##.data := List.map (fun p -> self#point_to_js p) data.data
                    |> Array.of_list
                    |> Js.array

  end

end

module Config = struct

  type ('a,'b) axis = ('a,'b) Axes.Cartesian.axis

  class ['a,'b,'c,'d] options ~(x_axis:('a,'b) axis) ~(y_axis:('c,'d) axis) () =
    let x_axis   = Axes.Cartesian.create x_axis in
    let y_axis   = Axes.Cartesian.create y_axis in
    object

      inherit Options.t () as super

      val scales = new Axes.Cartesian.t ~x_axes:[x_axis] ~y_axes:[y_axis] ()

      method x_axis = x_axis
      method y_axis = y_axis

      method! replace x = super#replace x;
                          scales#replace obj##.scales

      initializer
        obj##.scales := scales#get_obj

    end

  class ['a,'b,'c,'d] t ~(x_axis:('a,'b) axis)
                      ~(y_axis:('c,'d) axis)
                      ~(data:('a,'c) dataset list)
                      () =
    let x_to_js = point_to_js x_axis in
    let y_to_js = point_to_js y_axis in
    let x_of_js = point_of_js x_axis in
    let y_of_js = point_of_js y_axis in

    let datasets = List.map (fun x -> new Dataset.t ~data:x ~x_to_js ~y_to_js ~x_of_js ~y_of_js ()) data in
    let data     = object%js
                     val mutable datasets = List.map (fun x -> x#get_obj) datasets |> Array.of_list |> Js.array
                   end in
    object
      val options = new options ~x_axis ~y_axis ()

      method x_to_js = x_to_js
      method y_to_js = y_to_js
      method x_of_js = x_of_js
      method y_of_js = y_of_js

      method options  = options
      method datasets = datasets

      method data = data
    end

end

class ['a,'b,'c,'d] t ~(config:('a,'b,'c,'d) Config.t) () = object
  inherit [('a,'b,'c,'d) Config.options] Base_chart.t ~typ:Line
                                         ~options:config#options
                                         ~data:(Js.Unsafe.inject config#data) ()

  method config = config
end
