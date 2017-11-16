[@@@ocaml.warning "-60"]

open Base

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

    type fill = Elements.Line.fill
    let fill_to_any = Elements.Line.fill_to_any

    let cubic_interpolation_mode_to_string = function
      | Default -> "default" | Monotone -> "monotone"

    type bool_or_string

    type 'a or_array

    class type t =
      object
        method data                      : number_or_point Js.t Js.js_array Js.t Js.prop
        method label                     : Js.js_string Js.t Js.optdef_prop
        method xAxisID                   : Js.js_string Js.t Js.optdef_prop
        method yAxisID                   : Js.js_string Js.t Js.optdef_prop
        method backgroundColor           : Js.js_string Js.t Js.optdef_prop
        method borderWidth               : float Js.optdef_prop
        method borderColor               : Js.js_string Js.t Js.optdef_prop
        method borderCapStyle            : Js.js_string Js.t Js.optdef_prop
        method borderDash                : float Js.js_array Js.t Js.optdef_prop
        method borderDashOffset          : float Js.optdef_prop
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

    let to_obj ?label ?x_axis_id ?y_axis_id ?background_color ?border_color ?border_width
               ?border_dash ?border_dash_offset ?border_cap_style ?border_join_style
               ?cubic_interpolation_mode ?fill ?line_tension ?show_line ?span_gaps ?stepped_line
               ~(data : data) () : t Js.t =
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
      |> Obj.map_cons_option ~f:fill_to_any "fill" fill
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
      method datasets : Dataset.t Js.t Js.js_array Js.t Js.prop
    end

  let to_obj ?(labels : string list option) ?(datasets : Dataset.t Js.t list option) () : t Js.t =
    Obj.map_cons_option ~f:(List.map Js.string %> Array.of_list %> Js.array) "labels" labels []
    |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "datasets" datasets
    |> Array.of_list
    |> Js.Unsafe.obj

end

class type conf =
  object
    method type_   : Js.js_string Js.t Js.prop
    method data    : Data.t Js.t Js.prop
    method options : Options.t Js.t Js.optdef_prop
  end

let constr : (Dom_html.canvasElement Js.t -> conf Js.t -> Base.chart Js.t) Js.constr =
  Js.Unsafe.global##.Chart

let attach ?options ~(data : Data.t Js.t) canvas =
  let (conf : conf Js.t) = [ "type", Js.Unsafe.inject @@ Js.string @@ Base.typ_to_string Base.Line
                           ; "data", Js.Unsafe.inject data ]
                           |> Obj.cons_option "options" options
                           |> Array.of_list
                           |> Js.Unsafe.obj in
  Json.output conf |> Js.to_string |> print_endline;
  new%js constr canvas conf