[@@@ocaml.warning "-60"]

let (%>) = CCFun.(%>)

module Obj = Base.Obj

module Grid_line = struct

  class type t =
    object
      method display                  : bool Js.t Js.optdef_prop
      method color                    : Js.js_string Js.t Js.optdef_prop
      method borderDash               : int Js.js_array Js.t Js.optdef_prop
      method borderDashOffset         : int Js.optdef_prop
      method lineWidth                : int Js.optdef_prop
      method drawBorder               : bool Js.t Js.optdef_prop
      method drawOnChartArea          : bool Js.t Js.optdef_prop
      method drawTicks                : bool Js.t Js.optdef_prop
      method tickMarkLength           : int Js.t Js.optdef_prop
      method zeroLineWidth            : int Js.t Js.optdef_prop
      method zeroLineColor            : Js.js_string Js.t Js.optdef_prop
      method zeroLineBorderDash       : int Js.js_array Js.t Js.optdef_prop
      method zeroLineBorderDashOffset : int Js.optdef_prop
      method offsetGridLines          : bool Js.t Js.optdef_prop
    end

  let to_obj ?display ?color ?border_dash ?border_dash_offset ?line_width
             ?draw_border ?draw_on_chart_area ?draw_ticks ?tick_mark_length
             ?zero_line_width ?zero_line_color ?zero_line_border_dash ?zero_line_border_dash_offset
             ?offset_grid_lines () : t Js.t =
    Obj.map_cons_option ~f:Js.bool "display" display []
    |> Obj.map_cons_option ~f:Js.string "color" color
    |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "borderDash" border_dash
    |> Obj.cons_option "borderDashOffset" border_dash_offset
    |> Obj.cons_option "lineWidth" line_width
    |> Obj.map_cons_option ~f:Js.bool "drawBorder" draw_border
    |> Obj.map_cons_option ~f:Js.bool "drawOnChartArea" draw_on_chart_area
    |> Obj.map_cons_option ~f:Js.bool "drawTicks" draw_ticks
    |> Obj.cons_option "tickMarkLength" tick_mark_length
    |> Obj.cons_option "zeroLineWidth" zero_line_width
    |> Obj.map_cons_option ~f:Js.string "zeroLineColor" zero_line_color
    |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "zeroLineBorderDash" zero_line_border_dash
    |> Obj.cons_option "zeroLineBorderDashOffset" zero_line_border_dash_offset
    |> Obj.map_cons_option ~f:Js.bool "offsetGridLines" offset_grid_lines
    |> (Array.of_list %> Js.Unsafe.obj)

end

module Scale_label = struct

  type line_height_js
  type padding_js

  type line_height = String of string | Float of float
  type padding = Number of int | Object of padding_obj
   and padding_obj = { top : int; bottom : int }

  class type coord =
    object
      method top    : int Js.optdef_prop
      method bottom : int Js.optdef_prop
    end

  class type t =
    object
      inherit Font.t
      method display     : bool Js.t Js.optdef_prop
      method labelString : Js.js_string Js.t Js.optdef_prop
      method lineHeight  : line_height_js Js.t Js.optdef_prop
      method padding     : padding_js Js.t Js.optdef_prop
    end


  let cast_line_height_to_number (x : line_height_js Js.t) = Base.Cast.to_number x
  let cast_line_height_to_string (x : line_height_js Js.t) = Base.Cast.to_string x
  let cast_padding_height_to_number (x : padding_js Js.t) = Base.Cast.to_number x
  let cast_padding_to_object (x : padding_js Js.t) : coord Js.t Js.opt = Base.Cast.to_object x

  let to_obj ?display ?label_string ?line_height ?font ?padding () : t Js.t =
    let line_height_to_any x = (match x with
                                | String s -> Js.Unsafe.inject @@ Js.string s
                                | Float x  -> Js.Unsafe.inject x) in
    let padding_to_any x = (match x with
                            | Number x -> Js.Unsafe.inject x
                            | Object o -> [| "top",    Js.Unsafe.inject o.top
                                           ; "bottom", Js.Unsafe.inject o.bottom |]
                                          |> Js.Unsafe.obj |> Js.Unsafe.inject) in
    Obj.map_cons_option ~f:Js.bool "display" display []
    |> Obj.map_cons_option ~f:Js.string "labelString" label_string
    |> Obj.map_cons_option ~f:line_height_to_any "lineHeight" line_height
    |> Obj.map_cons_option ~f:padding_to_any "padding" padding
    |> Array.of_list
    |> (fun x -> match font with
                 | Some font -> Array.append x @@ Font.to_array font
                 | None      -> x)
    |> Js.Unsafe.obj

end

class type tick_minor =
  object
    inherit Font.t
    method callback : Js.js_string Js.t ->
                      Js.number Js.t ->
                      Js.js_string Js.t Js.js_array Js.t ->
                      Js.js_string Js.t Js.opt Js.meth Js.optdef_prop
  end

class type tick_major =
  object
    inherit Font.t
    method callback : Js.js_string Js.t ->
                      Js.number Js.t ->
                      Js.js_string Js.t Js.js_array Js.t ->
                      Js.js_string Js.t Js.opt Js.meth Js.optdef_prop
  end

class type tick_common =
  object
    inherit Font.t
    method callback : Js.js_string Js.t ->
                      Js.number Js.t ->
                      Js.js_string Js.t Js.js_array Js.t ->
                      Js.js_string Js.t Js.opt Js.meth Js.optdef_prop
    method display  : bool Js.t Js.optdef_prop
    method reverse  : bool Js.t Js.optdef_prop
    method minor    : tick_minor Js.t Js.optdef_prop
    method major    : tick_major Js.t Js.optdef_prop
  end

let minor_major_to_list ?callback ?font () =
  Obj.map_cons_option ~f:Js.wrap_callback "callback" callback []
  |> Obj.map_append_option ~f:Font.to_list font

let common_tick_to_list ?font ?callback ?display ?reverse ?minor_callback
                        ?minor_font ?major_callback ?major_font () =
  let minor = minor_major_to_list ?callback:minor_callback ?font:minor_font ()
              |> (fun x -> if CCList.is_empty x then None else Some x) in
  let major = minor_major_to_list ?callback:major_callback ?font:major_font ()
              |> (fun x -> if CCList.is_empty x then None else Some x) in
  Obj.map_cons_option ~f:Js.wrap_callback "callback" callback []
  |> Obj.map_cons_option ~f:Js.bool "display" display
  |> Obj.map_cons_option ~f:Js.bool "reverse" reverse
  |> Obj.map_append_option ~f:Font.to_list font
  |> Obj.map_cons_option ~f:(Array.of_list %> Js.Unsafe.obj) "minor" minor
  |> Obj.map_cons_option ~f:(Array.of_list %> Js.Unsafe.obj) "major" major

module Cartesian = struct

  class type cartesian_tick =
    object
      inherit tick_common
      method autoSkip        : bool Js.t Js.optdef_prop
      method autoSkipPadding : int Js.optdef_prop
      method labelOffset     : int Js.optdef_prop
      method maxRotation     : int Js.optdef_prop
      method minRotation     : int Js.optdef_prop
      method mirror          : bool Js.t Js.optdef_prop
      method padding         : int Js.optdef_prop
    end

  let tick_to_list ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                   ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                   ?mirror ?padding () =
    common_tick_to_list ?font ?callback ?display ?reverse
                        ?minor_callback ?minor_font ?major_callback ?major_font ()
    |> Obj.map_cons_option ~f:Js.bool "autoSkip" auto_skip
    |> Obj.cons_option "autoSkipPadding" auto_skip_padding
    |> Obj.cons_option "labelOffset" label_offset
    |> Obj.cons_option "maxRotation" max_rotation
    |> Obj.cons_option "minRotation" min_rotation
    |> Obj.map_cons_option ~f:Js.bool "mirror" mirror
    |> Obj.cons_option "padding" padding

  type position = Top | Left | Bottom | Right

  let position_to_string = function
    | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"

  let typ_to_string = function
    | `Linear -> "linear" | `Logarithmic -> "logarithmic" | `Category -> "category" | `Time -> "time"

  class type cartesian =
    object
      method type_      : Js.js_string Js.t Js.optdef_prop
      method position   : Js.js_string Js.t Js.optdef_prop
      method offset     : bool Js.t Js.optdef_prop
      method id         : Js.js_string Js.t Js.optdef_prop
      method gridLines  : Grid_line.t Js.t Js.optdef_prop
      method scaleLabel : Scale_label.t Js.t Js.optdef_prop
    end

  let to_list ?position ?offset ?id ?grid_lines ?scale_label ~type_ () =
    [ "type", Js.Unsafe.inject @@ Js.string @@ typ_to_string type_ ]
    |> Obj.map_cons_option ~f:(position_to_string %> Js.string) "position" position
    |> Obj.map_cons_option ~f:Js.bool "offset" offset
    |> Obj.map_cons_option ~f:Js.string "id" id
    |> Obj.cons_option "gridLines" grid_lines
    |> Obj.cons_option "scaleLabel" scale_label

  module Category = struct

    module Tick = struct

      class type t =
        object
          inherit cartesian_tick
          method labels : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop
          method min    : Js.js_string Js.t Js.optdef_prop
          method max    : Js.js_string Js.t Js.optdef_prop
        end

      let to_obj ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                 ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                 ?mirror ?padding ?labels ?min ?max () : t Js.t =
        tick_to_list ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                     ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                     ?mirror ?padding ()
        |> Obj.map_cons_option ~f:(List.map Js.string %> Array.of_list %> Js.array) "labels" labels
        |> Obj.map_cons_option ~f:Js.string "min" min
        |> Obj.map_cons_option ~f:Js.string "max" max
        |> Array.of_list
        |> Js.Unsafe.obj

    end

    class type t =
      object
        inherit cartesian
        method ticks : Tick.t Js.t Js.optdef_prop
      end

    let to_obj ?position ?offset ?id ?grid_lines ?scale_label ?ticks () : t Js.t =
      to_list ?position ?offset ?id ?grid_lines ?scale_label ~type_:`Category ()
      |> Obj.cons_option "ticks" ticks
      |> (Array.of_list %> Js.Unsafe.obj)

  end

  module Linear = struct

    module Tick = struct

      class type t =
        object
          inherit cartesian_tick
          method beginAtZero   : bool Js.t Js.optdef_prop
          method min           : float Js.optdef_prop
          method max           : float Js.optdef_prop
          method maxTicksLimit : int Js.optdef_prop
          method stepSize      : float Js.optdef_prop
          method suggestedMax  : float Js.optdef_prop
          method suggestedMin  : float Js.optdef_prop
        end

      let to_obj ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                 ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                 ?mirror ?padding ?begin_at_zero ?min ?max ?max_ticks_limit ?step_size
                 ?suggested_max ?suggested_min () : t Js.t =
        tick_to_list ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                     ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                     ?mirror ?padding ()
        |> Obj.map_cons_option ~f:Js.bool "beginAtZero" begin_at_zero
        |> Obj.cons_option "min" min
        |> Obj.cons_option "max" max
        |> Obj.cons_option "maxTicksLimit" max_ticks_limit
        |> Obj.cons_option "stepSize" step_size
        |> Obj.cons_option "suggestedMax" suggested_max
        |> Obj.cons_option "suggestedMin" suggested_min
        |> (Array.of_list %> Js.Unsafe.obj)

    end

    class type t =
      object
        inherit cartesian
        method ticks : Tick.t Js.t Js.optdef_prop
      end

    let to_obj ?position ?offset ?id ?grid_lines ?scale_label ?ticks () : t Js.t =
      to_list ?position ?offset ?id ?grid_lines ?scale_label ~type_:`Linear ()
      |> Obj.cons_option "ticks" ticks
      |> (Array.of_list %> Js.Unsafe.obj)

  end

  module Logarithmic = struct

    module Tick = struct

      class type t =
        object
          inherit cartesian_tick
          method min : float Js.optdef_prop
          method max : float Js.optdef_prop
        end

      let to_obj ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                 ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                 ?mirror ?padding ?min ?max () : t Js.t =
        tick_to_list ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                     ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                     ?mirror ?padding ()
        |> Obj.cons_option "min" min
        |> Obj.cons_option "max" max
        |> (Array.of_list %> Js.Unsafe.obj)

    end

    class type t =
      object
        inherit cartesian
        method ticks : Tick.t Js.t Js.optdef_prop
      end

    let to_obj ?position ?offset ?id ?grid_lines ?scale_label ?ticks () : t Js.t =
      to_list ?position ?offset ?id ?grid_lines ?scale_label ~type_:`Logarithmic ()
      |> Obj.cons_option "ticks" ticks
      |> (Array.of_list %> Js.Unsafe.obj)

  end

  module Time = struct

    module Tick = struct

      type source = Auto | Data | Labels

      let source_to_string : source -> string = function
        | Auto -> "auto" | Data -> "data" | Labels -> "labels"

      class type t =
        object
          inherit cartesian_tick
          method source : Js.js_string Js.t Js.optdef_prop
        end

      let to_obj ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                 ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                 ?mirror ?padding ?source () : t Js.t =
        tick_to_list ?font ?callback ?display ?reverse ?minor_callback ?minor_font ?major_callback ?major_font
                     ?auto_skip ?auto_skip_padding ?label_offset ?max_rotation ?min_rotation
                     ?mirror ?padding ()
        |> Obj.map_cons_option ~f:(source_to_string %> Js.string) "source" source
        |> (Array.of_list %> Js.Unsafe.obj)

    end

    module Time = struct

      type time_unit = Millisecond
                     | Second
                     | Minute
                     | Hour
                     | Day
                     | Week
                     | Month
                     | Quarter
                     | Year

      let time_unit_to_string = function
        | Millisecond -> "millisecond" | Second  -> "second"  | Minute -> "minute"
        | Hour        -> "hour"        | Day     -> "day"     | Week   -> "week"
        | Month       -> "month"       | Quarter -> "quarter" | Year   -> "year"

      class type display_formats_obj =
        object
          method millisecond : Js.js_string Js.t Js.optdef_prop
          method second      : Js.js_string Js.t Js.optdef_prop
          method minute      : Js.js_string Js.t Js.optdef_prop
          method hour        : Js.js_string Js.t Js.optdef_prop
          method day         : Js.js_string Js.t Js.optdef_prop
          method week        : Js.js_string Js.t Js.optdef_prop
          method month       : Js.js_string Js.t Js.optdef_prop
          method quarter     : Js.js_string Js.t Js.optdef_prop
          method year        : Js.js_string Js.t Js.optdef_prop
        end

      class type t =
        object
          method displayFormats : display_formats_obj Js.t Js.optdef_prop
          method isoWeekday     : bool Js.t Js.optdef_prop
          method max            : float Js.t Js.optdef_prop
          method min            : float Js.t Js.optdef_prop
          (* method parser_        : unit Js.t Js.optdef_prop *)
          method round          : Js.js_string Js.t Js.optdef_prop
          method tooltipFormat  : Js.js_string Js.t Js.optdef_prop
          method unit           : Js.js_string Js.t Js.optdef_prop
          method stepSize       : int Js.optdef_prop
          method minUnit        : Js.js_string Js.t Js.optdef_prop
        end

      let to_display_formats_list ?millisecond ?second ?minute ?hour ?day ?week ?month ?quarter ?year () =
        Obj.map_cons_option ~f:Js.string "millisecond" millisecond []
        |> Obj.map_cons_option ~f:Js.string "second" second
        |> Obj.map_cons_option ~f:Js.string "minute" minute
        |> Obj.map_cons_option ~f:Js.string "hour" hour
        |> Obj.map_cons_option ~f:Js.string "day" day
        |> Obj.map_cons_option ~f:Js.string "week" week
        |> Obj.map_cons_option ~f:Js.string "month" month
        |> Obj.map_cons_option ~f:Js.string "quarter" quarter
        |> Obj.map_cons_option ~f:Js.string "year" year

      let to_obj ?millisecond_fmt ?second_fmt ?minute_fmt ?hour_fmt ?day_fmt ?week_fmt ?month_fmt
                 ?quarter_fmt ?year_fmt ?iso_weekday ?max ?min ?round ?tooltip_fmt ?time_unit ?step_size
                 ?min_unit () : t Js.t =
        let fmts = to_display_formats_list ?millisecond:millisecond_fmt
                                           ?second:second_fmt
                                           ?minute:minute_fmt
                                           ?hour:hour_fmt
                                           ?day:day_fmt
                                           ?week:week_fmt
                                           ?month:month_fmt
                                           ?quarter:quarter_fmt
                                           ?year:year_fmt
                                           ()
                 |> (fun x -> if CCList.is_empty x then None else Some x) in
        Obj.map_cons_option ~f:Js.bool "isoWeekday" iso_weekday []
        |> Obj.cons_option "min" min
        |> Obj.cons_option "max" max
        |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "displayFormats" fmts
        |> Obj.map_cons_option ~f:(time_unit_to_string %> Js.string) "round" round
        |> Obj.map_cons_option ~f:Js.string "tooltipFormat" tooltip_fmt
        |> Obj.map_cons_option ~f:(time_unit_to_string %> Js.string) "unit" time_unit
        |> Obj.cons_option "stepSize" step_size
        |> Obj.map_cons_option ~f:(time_unit_to_string %> Js.string) "minUnit" min_unit
        |> (Array.of_list %> Js.Unsafe.obj)

    end

    type distribution = Linear | Series

    type bounds = Data | Ticks

    let distribution_to_string = function
        Linear -> "linear" | Series -> "series"

    let bounds_to_string = function
      | Data -> "data" | Ticks -> "ticks"

    class type t =
      object
        inherit cartesian
        method distribution : Js.js_string Js.t Js.optdef_prop
        method bounds       : Js.js_string Js.t Js.optdef_prop
        method ticks        : Tick.t Js.t Js.optdef_prop
        method time         : Time.t Js.t Js.optdef_prop
      end

    let to_obj ?position ?offset ?id ?grid_lines ?scale_label ?ticks ?time ?distribution ?bounds () : t Js.t =
      to_list ?position ?offset ?id ?grid_lines ?scale_label ~type_:`Time ()
      |> Obj.map_cons_option ~f:(distribution_to_string %> Js.string) "distribution" distribution
      |> Obj.map_cons_option ~f:(bounds_to_string %> Js.string) "bounds" bounds
      |> Obj.cons_option "ticks" ticks
      |> Obj.cons_option "time" time
      |> (Array.of_list %> Js.Unsafe.obj)

  end

end

module Radial = struct

end

class type t =
  object
    method xAxes : 'a Js.t Js.js_array Js.t Js.optdef_prop
    method yAxes : 'a Js.t Js.js_array Js.t Js.optdef_prop
  end

let to_obj ?x_axes ?y_axes () =
  Obj.map_cons_option ~f:(Array.of_list %> Js.array) "xAxes" x_axes []
  |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "yAxes" y_axes
  |> (Array.of_list %> Js.Unsafe.obj)
