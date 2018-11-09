open Containers
open Base
open Axes_cartesian_common

module Realtime = struct

  class type t_js =
    object
      method duration : int Js.prop
      method refresh : int Js.prop
      method delay : int Js.prop
      method ttl : int Js.optdef_prop
      method frameRate : int Js.prop
      method pause : bool Js.t Js.prop
    end

  class t () =
    let (o : t_js Js.t) = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object

      inherit base_option o ()

    end

end

module Tick = struct

  type source = [`Auto | `Data | `Labels]

  class type t_js =
    object
      inherit Axes_cartesian_common.Tick.t_js
      method source : Js.js_string Js.t Js.prop
    end

  class t () =
    let (o : t_js Js.t) = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)
      inherit base_option o ()
      inherit Axes_cartesian_common.Tick.t ()

      (** The ticks.source property controls the ticks generation
        'auto':   generates 'optimal' ticks based on scale size and time options.
        'data':   generates ticks from data (including labels from data objects)
        'labels': generates ticks from user given data.labels values ONLY *)
      method source : source  = match Js.to_string _obj##.source with
        | "auto" -> `Auto
        | "data"  -> `Data
        | "labels" -> `Labels
        | _ -> failwith "Bad source string"

      method set_source (x : source) =
        let v = match x with
          | `Auto   -> "auto"
          | `Data   -> "data"
          | `Labels -> "labels"
        in _obj##.source := Js.string v

      initializer
        self#set_source `Auto
    end
end

module Time = struct

  type bool_or_string

  type time_unit =
    [ `Millisecond
    | `Second
    | `Minute
    | `Hour
    | `Day
    | `Week
    | `Month
    | `Quarter
    | `Year
    ]

  let time_unit_to_string : time_unit -> string = function
    | `Millisecond -> "millisecond"
    | `Second -> "second"
    | `Minute -> "minute"
    | `Hour -> "hour"
    | `Day -> "day"
    | `Week -> "week"
    | `Month -> "month"
    | `Quarter -> "quarter"
    | `Year -> "year"

  let time_unit_of_string : string -> time_unit option = function
    | "millisecond" -> Some `Millisecond
    | "second" -> Some `Second
    | "minute" -> Some `Minute
    | "hour" -> Some `Hour
    | "day" -> Some `Day
    | "week" -> Some `Week
    | "month" -> Some `Month
    | "quarter" -> Some `Quarter
    | "year" -> Some `Year
    | _ -> None

  class type display_formats_js =
    object
      method millisecond : Js.js_string Js.t Js.prop
      method second : Js.js_string Js.t Js.prop
      method minute : Js.js_string Js.t Js.prop
      method hour : Js.js_string Js.t Js.prop
      method day : Js.js_string Js.t Js.prop
      method week : Js.js_string Js.t Js.prop
      method month : Js.js_string Js.t Js.prop
      method quarter : Js.js_string Js.t Js.prop
      method year : Js.js_string Js.t Js.prop
    end

  class type t_js =
    object
      method displayFormats : display_formats_js Js.t Js.prop
      method isoWeekday : bool Js.t Js.prop
      method max : Js.number Js.t Js.opt Js.prop
      method min : Js.number Js.t Js.opt Js.prop
      method round : bool_or_string Js.t Js.prop
      method tooltipFormat : Js.js_string Js.t Js.opt Js.prop
      method unit : bool_or_string Js.t Js.prop
      method stepSize : int Js.prop
      method minUnit : Js.js_string Js.t Js.prop
    end

  (** The following display formats are used to configure how
      different time units are formed into strings for the axis tick marks. *)
  class display_formats () =
    let o : display_formats_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)
      inherit base_option o ()

      method millisecond : string = Js.to_string _obj##.millisecond
      method set_millisecond x = _obj##.millisecond := Js.string x

      method second : string = Js.to_string _obj##.second
      method set_second x = _obj##.second := Js.string x

      method minute : string = Js.to_string _obj##.minute
      method set_minute x = _obj##.minute := Js.string x

      method hour : string = Js.to_string _obj##.hour
      method set_hour x = _obj##.hour := Js.string x

      method day : string = Js.to_string _obj##.day
      method set_day x = _obj##.day := Js.string x

      method week : string = Js.to_string _obj##.week
      method set_week x = _obj##.week := Js.string x

      method month : string = Js.to_string _obj##.month
      method set_month x = _obj##.month := Js.string x

      method quarter : string = Js.to_string _obj##.quarter
      method set_quarter x = _obj##.quarter := Js.string x

      method year : string = Js.to_string _obj##.year
      method set_year x = _obj##.year := Js.string x

      initializer
        self#set_millisecond "HH:mm:ss.SSS";
        self#set_second "HH:mm:ss";
        self#set_minute "HH:mm";
        self#set_hour "HH:mm";
        self#set_day "DD.MM";
        self#set_week "ll";
        self#set_month "MM.YYYY";
        self#set_quarter "[Q]Q - YYYY";
        self#set_year "YYYY"
    end

  class ['a] t ~(value_to_js_number : 'a -> Js.number Js.t)
          ~(value_of_js_number : Js.number Js.t -> 'a) () =
    let (o : t_js Js.t) = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)
      inherit base_option o ()
      val _display_formats = new display_formats ()

      (** Sets how different time units are displayed. *)
      method display_formats : display_formats =
        _display_formats

      (** If true and the unit is set to 'week', then the first day of the week will be Monday.
        Otherwise, it will be Sunday. *)
      method iso_weekday : bool =
        Js.to_bool _obj##.isoWeekday

      method set_iso_weekday x =
        _obj##.isoWeekday := Js.bool x

      (** If defined, this will override the data maximum *)
      method max : 'a option =
        Option.map value_of_js_number
        @@ Js.Opt.to_option _obj##.max

      method set_max (x : 'a option) =
        let v = match x with
          | Some x -> Js.some (value_to_js_number x)
          | None   -> Js.null in
        _obj##.max := v

      (** If defined, this will override the data minimum *)
      method min : 'a option =
        Option.map value_of_js_number
        @@ Js.Opt.to_option _obj##.min

      method set_min (x : 'a option) =
        let v = match x with
          | Some x -> Js.some (value_to_js_number x)
          | None -> Js.null in
        _obj##.min := v

      (** If defined, dates will be rounded to the start of this unit. *)
      method round : time_unit option =
        Option.flat_map time_unit_of_string
        @@ Cast.to_string _obj##.round

      method set_round : time_unit option -> unit = function
        | None -> _obj##.round := Js.Unsafe.coerce Js._false
        | Some x -> _obj##.round := Js.Unsafe.coerce @@ Js.string @@ time_unit_to_string x

      (** The moment js format string to use for the tooltip. *)
      method tooltip_format =
        Option.map Js.to_string @@ Js.Opt.to_option _obj##.tooltipFormat

      method set_tooltip_format x =
        _obj##.tooltipFormat := Js.some @@ Js.string x

      (** If defined, will force the unit to be a certain type. *)
      method unit : time_unit option =
        Option.flat_map time_unit_of_string
        @@ Cast.to_string _obj##.unit

      method set_unit : time_unit option -> unit = function
        | None -> _obj##.unit := Js.Unsafe.coerce Js._false
        | Some x -> _obj##.unit := Js.Unsafe.coerce @@ Js.string @@ time_unit_to_string x

      (** The number of units between grid lines. *)
      method step_size : int =
        _obj##.stepSize

      method set_step_size x =
        _obj##.stepSize := x

      (** The minimum display format to be used for a time unit. *)
      method min_unit : time_unit =
        Option.get_or ~default:`Millisecond
        @@ time_unit_of_string
        @@ Js.to_string _obj##.minUnit

      method set_min_unit (x : time_unit) =
        _obj##.minUnit := Js.string @@ time_unit_to_string x

      initializer
        _obj##.displayFormats := Js.Unsafe.coerce self#display_formats#get_obj;
        self#set_iso_weekday false;
        self#set_round None;
        self#set_unit None;
        self#set_step_size 1;
        self#set_min_unit `Millisecond
    end

end

type distribution =
  [ `Linear
  | `Series
  ]

type bounds =
  [ `Data
  | `Ticks
  ]

class type t_js =
  object
    inherit Axes_cartesian_common.t_js
    method distribution : Js.js_string Js.t Js.prop
    method bounds : Js.js_string Js.t Js.prop
    method ticks : Tick.t_js Js.t Js.prop
    method time : Time.t_js Js.t Js.prop
  end

class ['a, 'b] t ?(delta : 'b option)
        ~id ~position ~(typ : ('a, 'b) time) () =
  let value_to_js_number = Axes_cartesian_common.axis_value_to_js (Time (typ,delta)) in
  let value_of_js_number = Axes_cartesian_common.axis_value_of_js (Time (typ,delta)) in
  let axis = Time (typ,delta) in
  let (o : t_js Js.t) =
    Js.Unsafe.coerce
    @@ Js.Unsafe.obj [| "realtime", Js.Unsafe.inject @@ Js.bool false |] in
  object(self)

    inherit ['a, 'b] Axes_cartesian_common.t ~axis ~id ~position o () as super

    val _ticks = new Tick.t ()
    val _time = new Time.t ~value_to_js_number ~value_of_js_number ()

    method ticks : Tick.t    = _ticks
    method time : 'a Time.t = _time

    method min = self#time#min
    method set_min x = self#time#set_min x
    method max = self#time#max
    method set_max x = self#time#set_max x

    (** The distribution property controls the data distribution along the scale:
        'linear': data are spread according to their time (distances can vary)
        'series': data are spread at the same distance from each other *)
    method distribution : distribution = match Js.to_string _obj##.distribution with
      | "linear" -> `Linear
      | "series" -> `Series
      | _ -> failwith "Bad distrubution string"
    method set_distribution (x : distribution) =
      let v = match x with
        | `Linear -> "linear" | `Series -> "series"
      in _obj##.distribution := Js.string v

    (** The bounds property controls the scale boundary strategy (bypassed by min/max time options)
        'data': make sure data are fully visible, labels outside are removed
        'ticks': make sure ticks are fully visible, data outside are truncated *)
    method bounds : bounds = match Js.to_string _obj##.bounds with
      | "data"  -> `Data
      | "ticks" -> `Ticks
      | _ -> failwith "Bad bounds string"
    method set_bounds (x : bounds) =
      let v = match x with
        | `Data -> "data" | `Ticks -> "ticks"
      in _obj##.bounds := Js.string v

    method! replace x =
      super#replace x;
      self#ticks#replace _obj##.ticks;
      self#time#replace _obj##.time

    initializer
      _obj##.time := Js.Unsafe.coerce self#time#get_obj;
      _obj##.ticks := Js.Unsafe.coerce self#ticks#get_obj
  end
