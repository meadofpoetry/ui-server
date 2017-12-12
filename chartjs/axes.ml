open Base
module Obj = Base.Obj

module Grid_line = struct

  type color_or_color_array
  type int_or_int_array

  type clr = Single of CSS.Color.t | Several of CSS.Color.t list
  type lw  = Single of int | Several of int list

  class type t_js =
    object
      method display                  : bool Js.t Js.prop
      method color                    : color_or_color_array Js.t Js.prop
      method borderDash               : int Js.js_array Js.t Js.prop
      method borderDashOffset         : int Js.prop
      method lineWidth                : int_or_int_array Js.t Js.prop
      method drawBorder               : bool Js.t Js.prop
      method drawOnChartArea          : bool Js.t Js.prop
      method drawTicks                : bool Js.t Js.prop
      method tickMarkLength           : int Js.prop
      method zeroLineWidth            : int Js.prop
      method zeroLineColor            : CSS.Color.js_t Js.prop
      method zeroLineBorderDash       : int Js.js_array Js.t Js.prop
      method zeroLineBorderDashOffset : int Js.prop
      method offsetGridLines          : bool Js.t Js.prop
    end

  class t () = object(self)
    inherit [t_js] base_option ()

    method set_display x = obj##.display := Js.bool x
    method get_display   = Js.to_bool obj##.display

    method set_color : clr -> unit = function
      | Single c  -> obj##.color := Js.Unsafe.coerce @@ Js.string @@ CSS.Color.string_of_t c
      | Several l -> obj##.color := Js.Unsafe.coerce @@ Js.array @@ Array.of_list @@ List.map CSS.Color.js l
    method get_color : clr =
      match Cast.to_list ~f:CSS.Color.ml obj##.color with
      | Some l -> Several l
      | None   -> Single (CSS.Color.ml @@ CSS.Color.js_t_of_js_string @@ Js.Unsafe.coerce obj##.color)

    method set_border_dash x = obj##.borderDash := Js.array @@ Array.of_list x
    method get_border_dash   = Array.to_list @@ Js.to_array obj##.borderDash

    method set_border_dash_offset x = obj##.borderDashOffset := x
    method get_border_dash_offset   = obj##.borderDashOffset

    method set_line_width : lw -> unit = function
      | Single w  -> obj##.lineWidth := Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int w
      | Several l -> obj##.lineWidth := Js.Unsafe.coerce @@ Js.array @@ Array.of_list l
    method get_line_width : lw =
      match Cast.to_list ~f:(fun x -> x) obj##.lineWidth with
      | Some l -> Several l
      | None   -> Single (int_of_float @@ Js.float_of_number (Js.Unsafe.coerce obj##.lineWidth))

    method set_draw_border x = obj##.drawBorder := Js.bool x
    method get_draw_border   = Js.to_bool obj##.drawBorder

    method set_draw_on_chart_area x = obj##.drawOnChartArea := Js.bool x
    method get_draw_on_chart_area   = Js.to_bool obj##.drawOnChartArea

    method set_draw_ticks x = obj##.drawTicks := Js.bool x
    method get_draw_ticks   = Js.to_bool obj##.drawTicks

    method set_tick_mark_length x = obj##.tickMarkLength := x
    method get_tick_mark_length   = obj##.tickMarkLength

    method set_zero_line_width x = obj##.zeroLineWidth := x
    method get_zero_line_width   = obj##.zeroLineWidth

    method set_zero_line_color x = obj##.zeroLineColor := CSS.Color.js x
    method get_zero_line_color   = CSS.Color.ml obj##.zeroLineColor

    method set_zero_line_border_dash x = obj##.zeroLineBorderDash := Js.array @@ Array.of_list x
    method get_zero_line_border_dash   = Array.to_list @@ Js.to_array obj##.zeroLineBorderDash

    method set_zero_line_border_dash_offset x = obj##.zeroLineBorderDashOffset := x
    method get_zero_line_border_dash_offset   = obj##.zeroLineBorderDashOffset

    method set_offset_grid_lines x = obj##.offsetGridLines := Js.bool x
    method get_offset_grid_lines   = Js.to_bool obj##.offsetGridLines

    initializer
      self#set_display true;
      self#set_color @@ Single (CSS.Color.rgb ~a:0.1 0 0 0);
      self#set_border_dash [];
      self#set_border_dash_offset 0;
      self#set_line_width @@ Single 1;
      self#set_draw_border true;
      self#set_draw_on_chart_area true;
      self#set_draw_ticks true;
      self#set_tick_mark_length 10;
      self#set_zero_line_width 1;
      self#set_zero_line_color @@ CSS.Color.rgb ~a:0.25 0 0 0;
      self#set_zero_line_border_dash [];
      self#set_zero_line_border_dash_offset 0;
      self#set_offset_grid_lines false
  end

end

module Scale_label = struct

  type number_or_object

  type padding = [ `Number of int | `Object of padding_obj ]
  and padding_obj = { top : int; bottom : int }

  class type coord =
    object
      method top    : int Js.prop
      method bottom : int Js.prop
    end

  class type t_js =
    object
      inherit Font.t_js
      method display     : bool Js.t Js.prop
      method labelString : Js.js_string Js.t Js.prop
      method lineHeight  : float Js.prop (* FIXME type. may be string *)
      method padding     : number_or_object Js.t Js.prop
    end

  class t () = object(self)
    inherit [t_js] base_option ()
    inherit [t_js] Font.t { size = 12
                          ; color = CSS.Color.rgb 102 102 102
                          ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                          ; style  = `Normal
                          } ()

    method set_display x = obj##.display := Js.bool x
    method get_display   = Js.to_bool obj##.display

    method set_label_string x = obj##.labelString := Js.string x
    method get_label_string   = Js.to_string obj##.labelString

    method set_line_height x = obj##.lineHeight := x
    method get_line_height   = obj##.lineHeight

    method set_padding : padding -> unit = function
      | `Number n -> obj##.padding := Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int n
      | `Object o -> obj##.padding := Js.Unsafe.coerce @@ (object%js
                                                             val mutable top    = o.top
                                                             val mutable bottom = o.bottom
                                                           end : coord Js.t)
    method get_padding : padding =
      match Cast.to_int obj##.padding with
      | Some x -> `Number x
      | None   -> `Object { top    = (Js.Unsafe.coerce obj##.padding)##.top
                          ; bottom = (Js.Unsafe.coerce obj##.padding)##.bottom
                          }
    initializer
      self#set_display false;
      self#set_label_string "";
      self#set_line_height 1.2;
      self#set_padding @@ `Number 4
  end

end

class type tick_minor_js =
  object
    inherit Font.t_js
  end

class type tick_major_js =
  object
    inherit Font.t_js
  end

class tick_minor () = object
  inherit [tick_minor_js] base_option ()
  inherit [tick_minor_js] Font.t { size   = 12
                                 ; color  = CSS.Color.rgb 102 102 102
                                 ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                                 ; style  = `Normal
                                 } ()
end

class tick_major () = object
  inherit [tick_major_js] base_option ()
  inherit [tick_major_js] Font.t { size   = 12
                                 ; color  = CSS.Color.rgb 102 102 102
                                 ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                                 ; style  = `Normal
                                 } ()
end

class type tick_common_js =
  object
    inherit Font.t_js
    (* FIXME add callback, to major and minor too *)
    method display  : bool Js.t Js.prop
    method reverse  : bool Js.t Js.prop
    method minor    : tick_minor_js Js.t Js.prop
    method major    : tick_major_js Js.t Js.prop
  end

class virtual tick_common () =
        object(self)
          val mutable virtual obj : #tick_common_js Js.t
          inherit ['a] Font.t { size = 12
                              ; color = CSS.Color.rgb 102 102 102
                              ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                              ; style  = `Normal
                              } ()

          val minor = new tick_minor ()
          val major = new tick_major ()

          method set_display x = obj##.display := Js.bool x
          method get_display   = Js.to_bool obj##.display

          method set_reverse x = obj##.reverse := Js.bool x
          method get_reverse   = Js.to_bool obj##.reverse

          method minor = minor (* FIXME make empty ? *)
          method major = major

          initializer
            self#set_display true;
            self#set_reverse false

        end

type _ numeric =
  | Integer : int numeric
  | Float   : float numeric

type _ time =
  | Unix : Int32.t time

module Cartesian = struct

  type position = Top | Left | Bottom | Right

  class type cartesian_tick_js =
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

  class type cartesian_js =
    object
      method type_      : Js.js_string Js.t Js.prop
      method position   : Js.js_string Js.t Js.prop
      method offset     : bool Js.t Js.prop
      method id         : Js.js_string Js.t Js.prop
      method gridLines  : Grid_line.t_js Js.t Js.prop
      method scaleLabel : Scale_label.t_js Js.t Js.prop
    end

  let position_to_string = function
    | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"
  let position_of_string_exn = function
    | "top" -> Top | "left" -> Left | "bottom" -> Bottom | "right" -> Right | _ -> failwith "bad position string"

  let typ_to_string = function
    | `Linear -> "linear" | `Logarithmic -> "logarithmic" | `Category -> "category" | `Time -> "time"

  class virtual cartesian_tick () =
          object(self)
            inherit tick_common ()
            val mutable virtual obj : #cartesian_tick_js Js.t

            method set_auto_skip x = obj##.autoSkip := Js.bool x
            method get_auto_skip   = Js.to_bool obj##.autoSkip

            method set_auto_skip_padding x = obj##.autoSkipPadding := x
            method get_auto_skip_padding   = obj##.autoSkipPadding

            method set_label_offset x = obj##.labelOffset := x
            method get_label_offset   = obj##.labelOffset

            method set_max_rotation x = obj##.maxRotation := x
            method get_max_rotation   = obj##.maxRotation

            method set_min_rotation x = obj##.minRotation := x
            method get_min_rotation   = obj##.minRotation

            method set_mirror x = obj##.mirror := Js.bool x
            method get_mirror   = Js.to_bool obj##.mirror

            method set_padding x = obj##.padding := x
            method get_padding   = obj##.padding

            initializer
              self#set_auto_skip true;
              self#set_auto_skip_padding 0;
              self#set_label_offset 0;
              self#set_max_rotation 50;
              self#set_min_rotation 0;
              self#set_mirror false;
              self#set_padding 10
          end

  class ['a] cartesian ~typ ~id ~position () = object(self)
    constraint 'a = #cartesian_js
    inherit ['a] base_option () as super
    val grid_lines  = new Grid_line.t ()
    val scale_label = new Scale_label.t ()

    method set_position x = obj##.position := Js.string @@ position_to_string x
    method get_position   = position_of_string_exn @@ Js.to_string obj##.position

    method set_offset x = obj##.offset := Js.bool x
    method get_offset   = Js.to_bool obj##.offset

    method set_id x = obj##.id := Js.string x
    method get_id   = Js.to_string obj##.id

    method grid_lines  = grid_lines
    method scale_label = scale_label

    method! replace x = super#replace x;
                        grid_lines#replace obj##.gridLines;
                        scale_label#replace obj##.scaleLabel

    initializer
      obj##.type_ := Js.string @@ typ_to_string typ;
      self#set_position position;
      self#set_offset false;
      obj##.id := Js.string id;
      obj##.gridLines  := grid_lines#get_obj;
      obj##.scaleLabel := scale_label#get_obj
  end

  module Category = struct

    module Tick = struct

      class type t_js =
        object
          inherit cartesian_tick_js
          method labels : Js.js_string Js.t Js.js_array Js.t Js.opt Js.prop
          method min    : Js.js_string Js.t Js.opt Js.prop
          method max    : Js.js_string Js.t Js.opt Js.prop
        end

      class t ~labels () = object(self)
        inherit [t_js] base_option ()
        inherit cartesian_tick ()

        method set_labels x = obj##.labels := Js.some @@ Js.array @@ Array.of_list @@ List.map Js.string x
        method get_labels   = CCOpt.map (fun x -> List.map Js.to_string @@ Array.to_list @@ Js.to_array x)
                                        (Js.Opt.to_option obj##.labels)

        method set_min x = obj##.min := Js.some @@ Js.string x
        method get_min   = CCOpt.map Js.to_string @@ Js.Opt.to_option obj##.min

        method set_max x = obj##.max := Js.some @@ Js.string x
        method get_max   = CCOpt.map Js.to_string @@ Js.Opt.to_option obj##.max

        initializer
          self#set_labels labels
      end

    end

    class type t_js =
      object
        inherit cartesian_js
        method ticks : Tick.t_js Js.t Js.prop
      end

    class t ~id ~position ~labels () = object
      inherit [t_js] cartesian ~typ:`Category ~id ~position () as super
      val ticks = new Tick.t ~labels ()

      method ticks = ticks
      method! replace x = super#replace x; ticks#replace obj##.ticks

      initializer
        obj##.ticks := ticks#get_obj

    end

  end

  module Linear = struct

    module Tick = struct

      class type ['a] t_js =
        object
          inherit cartesian_tick_js
          method beginAtZero   : bool Js.t Js.prop
          method min           : 'a Js.opt Js.prop
          method max           : 'a Js.opt Js.prop
          method maxTicksLimit : int Js.prop
          method stepSize      : float Js.opt Js.prop
          method suggestedMax  : 'a Js.opt Js.prop
          method suggestedMin  : 'a Js.opt Js.prop
        end

      class ['a] t () = object(self)
        inherit ['a t_js] base_option ()
        inherit tick_common ()

        method set_begin_at_zero x = obj##.beginAtZero := Js.bool x
        method get_begin_at_zero   = Js.to_bool obj##.beginAtZero

        method set_min (x:'a) = obj##.min := Js.some x
        method get_min : 'a option = Js.Opt.to_option obj##.min

        method set_max (x:'a) = obj##.max := Js.some x
        method get_max : 'a option = Js.Opt.to_option obj##.max

        method set_max_ticks_limit x = obj##.maxTicksLimit := x
        method get_max_ticks_limit   = obj##.maxTicksLimit

        method set_step_size x = obj##.stepSize := Js.some x
        method get_step_size   = Js.Opt.to_option obj##.stepSize

        method set_suggested_max (x:'a) = obj##.suggestedMax := Js.some x
        method get_suggested_max : 'a option = Js.Opt.to_option obj##.suggestedMax

        method set_suggested_min (x:'a) = obj##.suggestedMin := Js.some x
        method get_suggested_min : 'a option = Js.Opt.to_option obj##.suggestedMin

        initializer
          self#set_begin_at_zero false;
          self#set_max_ticks_limit 11
      end

    end

    class type ['a] t_js =
      object
        inherit cartesian_js
        method ticks : 'a Tick.t_js Js.t Js.prop
      end

    class ['a] t ~id ~position () = object
      inherit ['a t_js] cartesian ~typ:`Linear ~id ~position () as super
      val ticks = new Tick.t ()

      method ticks = ticks
      method! replace x = super#replace x; ticks#replace obj##.ticks

      initializer
        obj##.ticks := ticks#get_obj
    end

  end

  module Logarithmic = struct

    module Tick = struct

      class type ['a] t_js =
        object
          inherit cartesian_tick_js
          method min : 'a Js.opt Js.prop
          method max : 'a Js.opt Js.prop
        end

      class ['a] t () = object
        inherit ['a t_js] base_option ()
        inherit tick_common ()

        method set_max (x:'a) = obj##.max := Js.some x
        method get_max : 'a option = Js.Opt.to_option obj##.max

        method set_min (x:'a) = obj##.min := Js.some x
        method get_min : 'a option = Js.Opt.to_option obj##.min
      end

    end

    class type ['a] t_js =
      object
        inherit cartesian_js
        method ticks : 'a Tick.t_js Js.t Js.prop
      end

    class ['a] t ~id ~position () = object
      inherit ['a t_js] cartesian ~typ:`Logarithmic ~id ~position () as super
      val ticks = new Tick.t ()

      method ticks = ticks
      method! replace x = super#replace x; ticks#replace obj##.ticks

      initializer
        obj##.ticks := ticks#get_obj
    end

  end

  module Time = struct

    module Tick = struct

      type source = Auto | Data | Labels

      let source_to_string : source -> string = function
        | Auto -> "auto" | Data -> "data" | Labels -> "labels"
      let source_of_string_exn = function
        | "auto" -> Auto | "data" -> Data | "labels" -> Labels | _ -> failwith "Bad source string"

      class type t_js =
        object
          inherit cartesian_tick_js
          method source : Js.js_string Js.t Js.prop
        end

      class t () = object(self)
        inherit [t_js] base_option ()
        inherit tick_common ()

        method set_source x = obj##.source := Js.string @@ source_to_string x
        method get_source   = source_of_string_exn @@ Js.to_string obj##.source

        initializer
          self#set_source Auto
      end

    end

    module Time = struct

      type bool_or_string

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
      let time_unit_of_string_exn = function
        | "millisecond" -> Millisecond | "second"  -> Second  | "minute" -> Minute
        | "hour"        -> Hour        | "day"     -> Day     | "week"   -> Week
        | "month"       -> Month       | "quarter" -> Quarter | "year"   -> Year
        | _ -> failwith "Bad time unit string"

      class type display_formats_js =
        object
          method millisecond : Js.js_string Js.t Js.prop
          method second      : Js.js_string Js.t Js.prop
          method minute      : Js.js_string Js.t Js.prop
          method hour        : Js.js_string Js.t Js.prop
          method day         : Js.js_string Js.t Js.prop
          method week        : Js.js_string Js.t Js.prop
          method month       : Js.js_string Js.t Js.prop
          method quarter     : Js.js_string Js.t Js.prop
          method year        : Js.js_string Js.t Js.prop
        end

      class type ['a] t_js =
        object
          method displayFormats : display_formats_js Js.t Js.prop
          method isoWeekday     : bool Js.t Js.prop
          method max            : 'a Js.opt Js.prop
          method min            : 'a Js.opt Js.prop
          (* method parser_        : unit Js.t Js.optdef_prop *)
          method round          : bool_or_string Js.t Js.prop
          method tooltipFormat  : Js.js_string Js.t Js.opt Js.prop
          method unit           : bool_or_string Js.t Js.prop
          method stepSize       : int Js.prop
          method minUnit        : Js.js_string Js.t Js.prop
        end

      class display_formats () = object(self)
        inherit [display_formats_js] base_option ()

        method set_millisecond x = obj##.millisecond := Js.string x
        method get_millisecond   = Js.to_string obj##.millisecond

        method set_second x = obj##.second := Js.string x
        method get_second   = Js.to_string obj##.second

        method set_minute x = obj##.minute := Js.string x
        method get_minute   = Js.to_string obj##.minute

        method set_hour x = obj##.hour := Js.string x
        method get_hour   = Js.to_string obj##.hour

        method set_day x = obj##.day := Js.string x
        method get_day   = Js.to_string obj##.day

        method set_week x = obj##.week := Js.string x
        method get_week   = Js.to_string obj##.week

        method set_month x = obj##.month := Js.string x
        method get_month   = Js.to_string obj##.month

        method set_quarter x = obj##.quarter := Js.string x
        method get_quarter   = Js.to_string obj##.quarter

        method set_year x = obj##.year := Js.string x
        method get_year   = Js.to_string obj##.year

        initializer
          self#set_millisecond "h:mm:ss.SSS a";
          self#set_second "h:mm:ss a";
          self#set_minute "h:mm a";
          self#set_hour "hA";
          self#set_day "MMM D";
          self#set_week "ll";
          self#set_month "MMM YYYY";
          self#set_quarter "[Q]Q - YYYY";
          self#set_year "YYYY"
      end

      type bool_or_time = Bool of bool | Time_unit of time_unit

      class ['a] t () = object(self)
        inherit ['a t_js] base_option ()
        val display_formats = new display_formats ()

        method display_formats = display_formats

        method set_iso_weekday x = obj##.isoWeekday := Js.bool x
        method get_iso_weekday   = Js.to_bool obj##.isoWeekday

        method set_max (x:'a) = obj##.max := Js.some x (* FIXME time type *)
        method get_max : 'a option = Js.Opt.to_option obj##.max

        method set_min (x:'a) = obj##.min := Js.some x
        method get_min : 'a option = Js.Opt.to_option obj##.min

        method set_round : bool_or_time -> unit = function
          | Bool x      -> obj##.round := Js.Unsafe.coerce @@ Js.bool x
          | Time_unit x -> obj##.round := Js.Unsafe.coerce @@ Js.string @@ time_unit_to_string x
        method get_round : bool_or_time =
          match Cast.to_string obj##.round with
          | Some s -> Time_unit (time_unit_of_string_exn s)
          | None   -> Bool (CCOpt.get_exn @@ Cast.to_bool obj##.round)

        method set_tooltip_format x = obj##.tooltipFormat := Js.some @@ Js.string x
        method get_tooltip_format   = CCOpt.map Js.to_string @@ Js.Opt.to_option obj##.tooltipFormat

        method set_unit : bool_or_time -> unit = function
          | Bool x      -> obj##.unit := Js.Unsafe.coerce @@ Js.bool x
          | Time_unit x -> obj##.unit := Js.Unsafe.coerce @@ Js.string @@ time_unit_to_string x
        method get_unit : bool_or_time =
          match Cast.to_string obj##.unit with
          | Some s -> Time_unit (time_unit_of_string_exn s)
          | None   -> Bool (CCOpt.get_exn @@ Cast.to_bool obj##.unit)

        method set_step_size x = obj##.stepSize := x
        method get_step_size   = obj##.stepSize

        method set_min_unit x = obj##.minUnit := Js.string @@ time_unit_to_string x
        method get_min_unit   = time_unit_of_string_exn @@ Js.to_string obj##.minUnit

        initializer
          self#set_iso_weekday false;
          self#set_round @@ Bool false;
          self#set_unit @@ Bool false;
          self#set_step_size 1;
          self#set_min_unit Millisecond
      end

    end

    type distribution = Linear | Series

    type bounds = Data | Ticks

    let distribution_to_string = function
      | Linear -> "linear" | Series -> "series"
    let distribution_of_string_exn = function
      | "linear" -> Linear | "series" -> Series | _ -> failwith "Bad distrubution string"

    let bounds_to_string = function
      | Data -> "data" | Ticks -> "ticks"
    let bounds_of_string_exn = function
      | "data" -> Data | "ticks" -> Ticks | _ -> failwith "Bad bounds string"

    class type ['a] t_js =
      object
        inherit cartesian_js
        method distribution : Js.js_string Js.t Js.prop
        method bounds       : Js.js_string Js.t Js.prop
        method ticks        : Tick.t_js Js.t Js.prop
        method time         : 'a Time.t_js Js.t Js.prop
      end

    class ['a] t ~id ~position () = object
      inherit ['a t_js] cartesian ~typ:`Logarithmic ~id ~position () as super
      val ticks = new Tick.t ()
      val time  = new Time.t ()

      method ticks = ticks
      method time  = time

      method set_distribution x = obj##.distribution := Js.string @@ distribution_to_string x
      method get_distribution   = distribution_of_string_exn @@ Js.to_string obj##.distribution

      method set_bounds x = obj##.bounds := Js.string @@ bounds_to_string x
      method get_bounds   = bounds_of_string_exn @@ Js.to_string obj##.bounds

      method! replace x = super#replace x; ticks#replace obj##.ticks; time#replace obj##.time

      initializer
        obj##.time  := time#get_obj;
        obj##.ticks := ticks#get_obj
    end

  end

  type (_,_) axis =
    | Linear      : (string * position * 'a numeric)  -> ('a,'a Linear.t) axis
    | Logarithmic : (string * position * 'a numeric)  -> ('a,'a Logarithmic.t) axis
    | Category    : (string * position * string list) -> (string,Category.t) axis
    | Time        : (string * position * 'a time)     -> ('a,'a Time.t) axis

  let create (type a b) (axis:(a,b) axis) : b =
    match axis with
    | Linear (id,position,_)        -> new Linear.t ~id ~position ()
    | Logarithmic (id,position,_)   -> new Logarithmic.t ~id ~position ()
    | Category (id,position,labels) -> new Category.t ~id ~position ~labels ()
    | Time (id,position,_)          -> new Time.t ~id ~position ()

  class type t_js =
    object
      method xAxes : 'a Js.t Js.js_array Js.t Js.prop
      method yAxes : 'a Js.t Js.js_array Js.t Js.prop
    end

  class ['a,'b] t ~(x_axes:'a list) ~(y_axes:'b list) () = object
    constraint 'a = < get_obj : #cartesian_js Js.t; .. >
    constraint 'b = < get_obj : #cartesian_js Js.t; .. >
    inherit [t_js] base_option ()
    val x_axes = x_axes
    val y_axes = y_axes

    method x_axes = x_axes
    method y_axes = y_axes

    (* TODO add replace *)
    initializer
      obj##.xAxes := Js.array @@ Array.of_list @@ List.map (fun x -> x#get_obj) x_axes;
      obj##.yAxes := Js.array @@ Array.of_list @@ List.map (fun x -> x#get_obj) y_axes
  end

end

module Radial = struct

end
