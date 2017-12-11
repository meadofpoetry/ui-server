open Base

module type P = sig
  type t
  val compare : t -> t -> int
  val axis : [`Linear | `Logarithmic | `Category | `Time ]
end

module Make_numeric_coord (X : P) (Y : P) = struct
  type t = { x : X.t
           ; y : Y.t option
           }

  type lst = t list
end

module Data = struct

  module Dataset = struct

    type data = Numbers of float list
              | Points  of xy list
     and xy = { x : float; y : float }

    type number_or_point

    class type point =
      object
        method x : Js.number Js.t Js.prop
        method y : Js.number Js.t Js.prop
      end

    let cast_number (x : number_or_point Js.t)  : Js.number Js.t Js.opt =
      if Js.typeof x = (Js.string "number")
      then Js.some (Js.Unsafe.coerce x)
      else Js.null

    let cast_point (x : number_or_point Js.t) : point Js.t Js.opt =
      if Js.typeof x = (Js.string "object")
      then Js.some (Js.Unsafe.coerce x)
      else Js.null

    let data_to_array = function
      | Numbers x -> List.map Js.Unsafe.inject x |> Array.of_list |> Js.array
      | Points  x -> let point_to_obj (p : xy) = Js.Unsafe.obj [| "x", Js.Unsafe.inject p.x
                                                                ; "y", Js.Unsafe.inject p.y |] in
                     List.map (point_to_obj %> Js.Unsafe.inject) x
                     |> Array.of_list
                     |> Js.array

    (* type 'a point_setting = Single of 'a *)
    (*                       | Multi of 'a list *)

    type cubic_interpolation_mode = Default | Monotone

    type stepped_line = Bool of bool | Before | After

    type fill = Bool of bool
              | Start
              | End
              | Origin
              | Absolute_index of int
              | Relative_index of signed
    and signed = Plus of int | Minus of int


    let fill_to_js = function
      | Bool b           -> Js.Unsafe.coerce @@ Js.bool b
      | Start            -> Js.Unsafe.coerce @@ Js.string "start"
      | End              -> Js.Unsafe.coerce @@ Js.string "end"
      | Origin           -> Js.Unsafe.coerce @@ Js.string "origin"
      | Absolute_index x -> Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int x
      | Relative_index x -> (match x with
                             | Plus x  -> Js.Unsafe.coerce @@ Js.string ("+" ^ string_of_int x)
                             | Minus x -> Js.Unsafe.coerce @@ Js.string ("-" ^ string_of_int x))

    let cubic_interpolation_mode_to_string = function
      | Default -> "default" | Monotone -> "monotone"

    type bool_or_string

    type 'a or_array

    class type t_js =
      object
        method data                      : number_or_point Js.t Js.js_array Js.t Js.prop
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

    class t () = object

      inherit [t_js] base_option ()

      method set_label x = obj##.label := Js.string x
      method get_label   = Js.to_string obj##.label

      method set_background_color x = obj##.backgroundColor := CSS.Color.js x
      method get_background_color   = CCOpt.map CSS.Color.ml @@ Js.Optdef.to_option obj##.backgroundColor

      method set_border_width x = obj##.borderWidth := x
      method get_border_width   = Js.Optdef.to_option obj##.borderWidth

      method set_border_color x = obj##.borderColor := CSS.Color.js x
      method get_border_color   = CCOpt.map CSS.Color.ml @@ Js.Optdef.to_option obj##.borderColor

    end

    let to_obj ?label ?x_axis_id ?y_axis_id ?background_color ?border_color ?border_width
               ?border_dash ?border_dash_offset ?border_cap_style ?border_join_style
               ?cubic_interpolation_mode ?fill ?line_tension ?show_line ?span_gaps ?stepped_line
               ~(data : data) () : t_js Js.t =
      let inject = Js.Unsafe.inject in
      let stepped_line_to_any = (function
                                 | (Bool b : stepped_line) -> inject @@ Js.bool b
                                 | Before -> inject @@ Js.string "before"
                                 | After  -> inject @@ Js.string "after") in
      [ "data", inject @@ data_to_array data ]
      |> Obj.map_cons_option ~f:Js.string "label" label
      |> Obj.map_cons_option ~f:Js.string "xAxisID" x_axis_id
      |> Obj.map_cons_option ~f:Js.string "yAxisID" y_axis_id
      |> Obj.map_cons_option ~f:Js.string "backgroundColor" background_color
      |> Obj.map_cons_option ~f:Js.string "borderColor" border_color
      |> Obj.cons_option "borderWidth" border_width
      |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "borderDash" border_dash
      |> Obj.cons_option "borderDashOffset" border_dash_offset
      |> Obj.map_cons_option ~f:(Base.Canvas.line_cap_to_string %> Js.string) "borderCapStyle" border_cap_style
      |> Obj.map_cons_option ~f:(Base.Canvas.line_join_to_string %> Js.string) "borderJoinStyle" border_join_style
      |> Obj.map_cons_option ~f:(cubic_interpolation_mode_to_string %> Js.string)
                             "cubicInterpolationMode"
                             cubic_interpolation_mode
      |> Obj.map_cons_option ~f:fill_to_js "fill" fill
      |> Obj.cons_option "lineTension" line_tension
      |> Obj.map_cons_option ~f:Js.bool "showLine" show_line
      |> Obj.map_cons_option ~f:Js.bool "spanGaps" span_gaps
      |> Obj.map_cons_option ~f:stepped_line_to_any "steppedLine" stepped_line
      |> Array.of_list
      |> Js.Unsafe.obj

  end

  class type t =
    object
      method labels   : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop
      method datasets : Dataset.t_js Js.t Js.js_array Js.t Js.prop
    end

  let to_obj ?(labels : string list option) ?(datasets : Dataset.t_js Js.t list option) () : t Js.t =
    Obj.map_cons_option ~f:(List.map Js.string %> Array.of_list %> Js.array) "labels" labels []
    |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "datasets" datasets
    |> Array.of_list
    |> Js.Unsafe.obj

end

module Options = struct

  class t () = object
    inherit Options.t ()

  end

end

class t ~options ~(data:Data.t Js.t) () = object
  inherit Base_chart.t ~options ~typ:Line ~data:(Js.Unsafe.inject data) ()

  initializer
    ()
end
