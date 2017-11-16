[@@@ocaml.warning "-60"]

module Data : sig

  module Dataset : sig

    type data = Numbers of float list
              | Points  of xy list
     and xy = { x : float; y : float}

    type number_or_point
    type bool_or_string
    type 'a or_array
    type cubic_interpolation_mode = Default | Monotone
    type stepped_line = Bool of bool | Before | After
    type fill = Elements.Line.fill

    class type point =
      object
        method x : Js.number Js.t Js.prop
        method y : Js.number Js.t Js.prop
      end

    val cast_number : number_or_point Js.t -> Js.number Js.t Js.opt
    val cast_point  : number_or_point Js.t -> point Js.t Js.opt

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

    val to_obj : ?label:string ->
                 ?x_axis_id:string ->
                 ?y_axis_id:string ->
                 ?background_color:string ->
                 ?border_color:string ->
                 ?border_width:float ->
                 ?border_dash:float list ->
                 ?border_dash_offset:float ->
                 ?border_cap_style:Base.Canvas.line_cap ->
                 ?border_join_style:Base.Canvas.line_join ->
                 ?cubic_interpolation_mode:cubic_interpolation_mode ->
                 ?fill:fill ->
                 ?line_tension:float ->
                 ?show_line:bool ->
                 ?span_gaps:bool ->
                 ?stepped_line:stepped_line ->
                 data:data ->
                 unit -> t Js.t

  end

  class type t =
    object
      method labels   : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop
      method datasets : Dataset.t Js.t Js.js_array Js.t Js.prop
    end

  val to_obj : ?labels:string list ->
               ?datasets:Dataset.t Js.t list ->
               unit -> t Js.t

end

val attach : ?options:Options.t Js.t ->
             data:Data.t Js.t ->
             Dom_html.canvasElement Js.t ->
             Base.chart Js.t
