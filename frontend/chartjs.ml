[@@@ocaml.warning "-60"]

let (>|=) x f = Js.Optdef.map x f
let (%>)      = CCFun.(%>)
let wrap_optdef x f = Js.Optdef.option x >|= f |> Js.Unsafe.inject

module Canvas = struct

  type line_cap = Butt
                | Round
                | Square

  type line_join = Bevel
                 | Round
                 | Miter

  let line_cap_to_string = function
    | Butt -> "butt" | Round -> "round" | Square -> "square"

  let line_join_to_string = function
    | Bevel -> "bevel" | Round -> "round" | Miter -> "miter"

end

module Options = struct

  type interaction_mode = Point
                        | Nearest
                        | Index
                        | Dataset
                        | X
                        | Y

  let interaction_mode_to_string = function
    | Point -> "point" | Nearest -> "nearest" | Index -> "index" | Dataset -> "dataset" | X -> "x" | Y -> "y"

  module Interaction = struct

    type axis = X
              | Y
              | XY

    let axis_to_string = function
      | X -> "x" | Y -> "y" | XY -> "xy"

    let hover_to_obj ?mode ?intersect ?axis ?animation_duration () =
      let open Js.Unsafe in
      let mode'               = ("mode", wrap_optdef mode (interaction_mode_to_string %> Js.string)) in
      let intersect'          = ("intersect", wrap_optdef intersect Js.bool) in
      let axis'               = ("axis", wrap_optdef axis (axis_to_string %> Js.string)) in
      let animation_duration' = ("animationDuration", wrap_optdef animation_duration Js.number_of_float) in
      obj [| mode'; intersect'; axis'; animation_duration' |]

  end

end

type color = string

type typ = Line
         | Bar
         | Radar
         | Pie
         | Doughnut
         | Polar
         | Bubble
         | Scatter

let typ_to_string = function
  | Line   -> "line"   | Bar      -> "bar"      | Radar -> "radar"
  | Pie    -> "pie"    | Doughnut -> "doughnut" | Polar -> "polarArea"
  | Bubble -> "bubble" | Scatter  -> "scatter"

module type Chart = sig

  type data
  type options

  val typ : typ

end


module Make(Chart : Chart) = struct

  class type dataset =
    object
      method label_ : Js.js_string Js.t Js.prop
      method data_  : Js.number Js.t Js.js_array Js.t Js.prop
    end

  class type data =
    object
      method labels_   : Js.js_string Js.t Js.js_array Js.t Js.prop
      method datasets_ : dataset Js.t Js.js_array Js.t Js.prop
    end

  class type conf =
    object
      method type_ : Js.js_string Js.t Js.prop
      method data_ : data Js.t Js.prop
    end

  class type t =
    object
    end

  let constr : (Dom_html.canvasElement Js.t -> conf Js.t -> t Js.t) Js.constr =
    Js.Unsafe.global##.Chart

  type config =
    { typ     : typ
    ; data    : Chart.data
    ; options : Chart.options option
    }

  let create ?id ?width ?height () =
    let open Tyxml_js.Html in
    canvas ~a:(CCOpt.map_or ~default:[] (fun x -> [a_id x]) id
               |> (fun attrs -> CCOpt.map_or ~default:attrs (fun x -> (a_width x) :: attrs) width)
               |> (fun attrs -> CCOpt.map_or ~default:attrs (fun x -> (a_height x) :: attrs) height))
           []

  let attach elt =
    Random.init (Unix.time () |> int_of_float);
    let (dataset : dataset Js.t) =  object%js
                                      val mutable label_ = Js.string "MER"
                                      val mutable data_  = List.map (fun _ -> Random.float 20.0)
                                                                    (CCList.range 0 20)
                                                           |> List.map Js.number_of_float
                                                           |> Array.of_list
                                                           |> Js.array
                                    end in
    let (data : data Js.t) = object%js
                               val mutable labels_ = List.map (fun x -> Js.string @@ string_of_int x)
                                                              (CCList.range 0 20)
                                                     |> Array.of_list
                                                     |> Js.array
                               val mutable datasets_ = [| dataset |] |> Js.array
                             end in
    let (conf : conf Js.t) = object%js
                               val mutable type_ = Js.string @@ typ_to_string Chart.typ
                               val mutable data_ = data
                             end in
    new%js constr elt conf

end

module Line_ : Chart = struct

  type data
  type options

  type 'a point_setting = Single of 'a
                        | Multi of 'a list

  type cubic_interpolation_mode = Default
                                | Monotone

  type stepped_line = Bool of bool
                    | Before
                    | After

  type data_points = Numbers of int list
                   | Points  of point list
   and point = { x : int; y : int }

  type dataset_config =
    { label                        : string
    ; x_axis_id                    : string
    ; y_axis_id                    : string
    ; background_color             : color
    ; border_color                 : color
    ; border_width                 : int
    ; border_dash                  : int list
    ; border_dash_offset           : int
    ; border_cap_style             : Canvas.line_cap
    ; border_join_style            : Canvas.line_join
    ; cubic_interpolation_mode     : cubic_interpolation_mode
    ; line_tension                 : int
    ; point_background_color       : color point_setting
    ; point_border_color           : color point_setting
    ; point_border_width           : float point_setting
    ; point_radius                 : float point_setting
    ; point_style                  : string point_setting
    ; point_hit_radius             : float point_setting
    ; point_hover_background_color : color point_setting
    ; point_hover_border_color     : color point_setting
    ; point_hover_border_width     : float point_setting
    ; point_hover_radius           : float point_setting
    ; show_line                    : bool
    ; span_gaps                    : bool
    ; stepped_line                 : stepped_line
    }

  let typ = Line

end

module Line = Make(Line_)
