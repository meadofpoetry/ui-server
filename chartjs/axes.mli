[@@@ocaml.warning "-60"]

module Grid_line : sig

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

  val to_obj : ?display:bool ->
               ?color:string ->
               ?border_dash:int list ->
               ?border_dash_offset:int ->
               ?line_width:int ->
               ?draw_border:bool ->
               ?draw_on_chart_area:bool ->
               ?draw_ticks:bool ->
               ?tick_mark_length:int ->
               ?zero_line_width:int ->
               ?zero_line_color:string ->
               ?zero_line_border_dash:int list ->
               ?zero_line_border_dash_offset:int ->
               ?offset_grid_lines:bool ->
               unit -> t Js.t

end

module Scale_label : sig

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

  val cast_line_height_to_number    : line_height_js Js.t -> Js.number Js.t Js.opt
  val cast_line_height_to_string    : line_height_js Js.t -> Js.js_string Js.t Js.opt
  val cast_padding_height_to_number : padding_js Js.t -> Js.number Js.t Js.opt
  val cast_padding_to_object        : padding_js Js.t -> coord Js.t Js.opt

  val to_obj : ?display:bool ->
               ?label_string:string ->
               ?line_height:line_height ->
               ?font:Font.font ->
               ?padding:padding ->
               unit -> t Js.t

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

module Cartesian : sig

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

  type position = Top | Left | Bottom | Right

  class type cartesian =
    object
      method type_      : Js.js_string Js.t Js.optdef_prop
      method position   : Js.js_string Js.t Js.optdef_prop
      method offset     : bool Js.t Js.optdef_prop
      method id         : Js.js_string Js.t Js.optdef_prop
      method gridLines  : Grid_line.t Js.t Js.optdef_prop
      method scaleLabel : Scale_label.t Js.t Js.optdef_prop
    end

  module Category : sig

    module Tick : sig

      class type t =
        object
          inherit cartesian_tick
          method labels : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop
          method min    : Js.js_string Js.t Js.optdef_prop
          method max    : Js.js_string Js.t Js.optdef_prop
        end

      val to_obj : ?font:Font.font ->
                   ?callback:(Js.js_string Js.t -> Js.number Js.t ->
                              Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?display:bool ->
                   ?reverse:bool ->
                   ?minor_callback:(Js.js_string Js.t -> Js.number Js.t ->
                                    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?minor_font:Font.font ->
                   ?major_callback:(Js.js_string Js.t -> Js.number Js.t ->
                                    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?major_font:Font.font ->
                   ?auto_skip:bool ->
                   ?auto_skip_padding:int ->
                   ?label_offset:int ->
                   ?max_rotation:int ->
                   ?min_rotation:int ->
                   ?mirror:bool ->
                   ?padding:int ->
                   ?labels:string list ->
                   ?min:string ->
                   ?max:string ->
                   unit -> t Js.t

    end

    class type t =
      object
        inherit cartesian
        method ticks : Tick.t Js.t Js.optdef_prop
      end

    val to_obj : ?position:position ->
                 ?offset:bool ->
                 ?id:string ->
                 ?grid_lines:Grid_line.t Js.t ->
                 ?scale_label:Scale_label.t Js.t ->
                 ?ticks:Tick.t Js.t ->
                 unit -> t Js.t

  end

  module Linear : sig

    module Tick : sig

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

      val to_obj : ?font:Font.font ->
                   ?callback:(Js.js_string Js.t -> Js.number Js.t ->
                              Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?display:bool ->
                   ?reverse:bool ->
                   ?minor_callback:(Js.js_string Js.t -> Js.number Js.t ->
                                    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?minor_font:Font.font ->
                   ?major_callback:(Js.js_string Js.t -> Js.number Js.t ->
                                    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?major_font:Font.font ->
                   ?auto_skip:bool ->
                   ?auto_skip_padding:int ->
                   ?label_offset:int ->
                   ?max_rotation:int ->
                   ?min_rotation:int ->
                   ?mirror:bool ->
                   ?padding:int ->
                   ?begin_at_zero:bool ->
                   ?min:float ->
                   ?max:float ->
                   ?max_ticks_limit:int ->
                   ?step_size:float ->
                   ?suggested_max:float ->
                   ?suggested_min:float ->
                   unit -> t Js.t

    end

    class type t =
      object
        inherit cartesian
        method ticks : Tick.t Js.t Js.optdef_prop
      end

    val to_obj : ?position:position ->
                 ?offset:bool ->
                 ?id:string ->
                 ?grid_lines:Grid_line.t Js.t ->
                 ?scale_label:Scale_label.t Js.t ->
                 ?ticks:Tick.t Js.t ->
                 unit -> t Js.t

  end

  module Logarithmic : sig

    module Tick : sig

      class type t =
        object
          inherit cartesian_tick
          method min : float Js.optdef_prop
          method max : float Js.optdef_prop
        end

      val to_obj : ?font:Font.font ->
                   ?callback:(Js.js_string Js.t -> Js.number Js.t ->
                              Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?display:bool ->
                   ?reverse:bool ->
                   ?minor_callback:(Js.js_string Js.t -> Js.number Js.t ->
                                    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?minor_font:Font.font ->
                   ?major_callback:(Js.js_string Js.t -> Js.number Js.t ->
                                    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?major_font:Font.font ->
                   ?auto_skip:bool ->
                   ?auto_skip_padding:int ->
                   ?label_offset:int ->
                   ?max_rotation:int ->
                   ?min_rotation:int ->
                   ?mirror:bool ->
                   ?padding:int ->
                   ?min:float ->
                   ?max:float ->
                   unit -> t Js.t

    end

    class type t =
      object
        inherit cartesian
        method ticks : Tick.t Js.t Js.optdef_prop
      end

    val to_obj : ?position:position ->
                 ?offset:bool ->
                 ?id:string ->
                 ?grid_lines:Grid_line.t Js.t ->
                 ?scale_label:Scale_label.t Js.t ->
                 ?ticks:Tick.t Js.t ->
                 unit -> t Js.t

  end

  module Time : sig

    module Tick : sig

      type source = Auto | Data | Labels

      class type t =
        object
          inherit cartesian_tick
          method source : Js.js_string Js.t Js.optdef_prop
        end

      val to_obj : ?font:Font.font ->
                   ?callback:(Js.js_string Js.t -> Js.number Js.t ->
                              Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?display:bool ->
                   ?reverse:bool ->
                   ?minor_callback:(Js.js_string Js.t -> Js.number Js.t ->
                                    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?minor_font:Font.font ->
                   ?major_callback:(Js.js_string Js.t -> Js.number Js.t ->
                                    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.opt) ->
                   ?major_font:Font.font ->
                   ?auto_skip:bool ->
                   ?auto_skip_padding:int ->
                   ?label_offset:int ->
                   ?max_rotation:int ->
                   ?min_rotation:int ->
                   ?mirror:bool ->
                   ?padding:int ->
                   ?source:source ->
                   unit -> t Js.t

    end

    module Time : sig

      type time_unit = Millisecond
                     | Second
                     | Minute
                     | Hour
                     | Day
                     | Week
                     | Month
                     | Quarter
                     | Year

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

      val to_obj : ?millisecond_fmt:string ->
                   ?second_fmt:string ->
                   ?minute_fmt:string ->
                   ?hour_fmt:string ->
                   ?day_fmt:string ->
                   ?week_fmt:string ->
                   ?month_fmt:string ->
                   ?quarter_fmt:string ->
                   ?year_fmt:string ->
                   ?iso_weekday:bool ->
                   ?max:float ->
                   ?min:float ->
                   ?round:time_unit ->
                   ?tooltip_fmt:string ->
                   ?time_unit:time_unit ->
                   ?step_size:int ->
                   ?min_unit:time_unit ->
                   unit -> t Js.t

    end

    type distribution = Linear | Series

    type bounds = Data | Ticks

    class type t =
      object
        inherit cartesian
        method distribution : Js.js_string Js.t Js.optdef_prop
        method bounds       : Js.js_string Js.t Js.optdef_prop
        method ticks        : Tick.t Js.t Js.optdef_prop
        method time         : Time.t Js.t Js.optdef_prop
      end

    val to_obj : ?position:position ->
                 ?offset:bool ->
                 ?id:string ->
                 ?grid_lines:Grid_line.t Js.t ->
                 ?scale_label:Scale_label.t Js.t ->
                 ?ticks:Tick.t Js.t ->
                 ?time:Time.t Js.t ->
                 ?distribution:distribution ->
                 ?bounds:bounds ->
                 unit -> t Js.t

  end

end

class type t =
  object
    method xAxes : 'a Js.t Js.js_array Js.t Js.optdef_prop
    method yAxes : 'a Js.t Js.js_array Js.t Js.optdef_prop
  end

val to_obj : ?x_axes:'a Js.t list ->
             ?y_axes:'a Js.t list ->
             unit -> t Js.t
