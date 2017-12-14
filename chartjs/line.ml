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
  { data  : ('a,'b) point list
  ; label : string
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

  type bool_or_string
  type 'a or_array
  type ('a,'b,'c) point_setting =
    [ `Val of 'c
    | `Lst of ' c list
    | `Fun of (int -> ('a,'b) point -> 'c) ]

  let cubic_interpolation_mode_to_string = function
    | Default -> "default" | Monotone -> "monotone"
  let cubic_interpolation_mode_of_string_exn = function
    | "default" -> Default | "monotone" -> Monotone | _ -> failwith "Bad cubic interpolation mode string"

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

      method pointBackgroundColor      : CSS.Color.js_t or_array Js.t Js.optdef_prop
      method pointBorderColor          : CSS.Color.js_t or_array Js.t Js.optdef_prop
      method pointBorderWidth          : int or_array Js.t Js.optdef_prop
      method pointRadius               : int or_array Js.t Js.optdef_prop
      method pointStyle                : Js.js_string Js.t or_array Js.t Js.optdef_prop
      method pointHitRadius            : int or_array Js.t Js.optdef_prop
      method pointHoverBackgroundColor : CSS.Color.js_t or_array Js.t Js.optdef_prop
      method pointHoverBorderColor     : CSS.Color.js_t or_array Js.t Js.optdef_prop
      method pointHoverBorderWidth     : int or_array Js.t Js.optdef_prop
      method pointHoverRadius          : int or_array Js.t Js.optdef_prop

      method showLine                  : bool Js.t Js.optdef_prop
      method spanGaps                  : bool Js.t Js.optdef_prop
      method steppedLine               : bool_or_string Js.t Js.optdef_prop
    end

  type ('a,'b) point_f =
    { background_color       : (int -> ('a,'b) point -> CSS.Color.t) option
    ; border_color           : (int -> ('a,'b) point -> CSS.Color.t) option
    ; border_width           : (int -> ('a,'b) point -> int) option
    ; radius                 : (int -> ('a,'b) point -> int) option
    ; style                  : (int -> ('a,'b) point -> string) option
    ; hit_radius             : (int -> ('a,'b) point -> int) option
    ; hover_background_color : (int -> ('a,'b) point -> CSS.Color.t) option
    ; hover_border_color     : (int -> ('a,'b) point -> CSS.Color.t) option
    ; hover_border_width     : (int -> ('a,'b) point -> int) option
    ; hover_radius           : (int -> ('a,'b) point -> int) option
    }

  let set_point_setting (type a b)
                        (f_set : 'd or_array Js.t -> unit)
                        (f_set_f : (int -> ('a,'b) point -> a) -> unit)
                        (f_conv : a -> b Js.t)
                        (data:('a,'b) point list)
                        (s : ('a,'b,a) point_setting) =
    match s with
    | `Val c -> f_set @@ Js.Unsafe.coerce @@ f_conv c
    | `Lst l -> f_set @@ Js.Unsafe.coerce @@ Js.array @@ Array.of_list @@ List.map (fun x -> f_conv x) l
    | `Fun f -> f_set_f f; f_set @@ Js.Unsafe.coerce @@ Js.array @@ Array.of_list
                           @@ List.mapi (fun i x -> f_conv @@ f i x) data
  let get_point_setting (type a b)
                        (v:'d or_array Js.t Js.optdef)
                        (f:(int -> ('a,'b) point -> a) option)
                        (f_conv:b Js.t -> a)
                        (f_cast:'e -> a option) =
    match f with
    | Some f -> Some (`Fun f)
    | None   -> CCOpt.map (fun x -> (match Cast.to_list ~f:f_conv x with
                                     | Some l -> `Lst l
                                     | None   -> (match f_cast x with
                                                  | Some c -> `Val c
                                                  | None   -> failwith "Bad point setting value")))
                @@ Js.Optdef.to_option v

  class ['a,'b] t ~(data:('a,'b) dataset) ~(x_to_js:'a -> point_js Js.t) ~(y_to_js:'b -> point_js Js.t)
                ~(x_of_js:point_js Js.t -> 'a) ~(y_of_js:point_js Js.t -> 'b) () = object(self)

    inherit [t_js] base_option ()

    val mutable point_f =
      { background_color       = None
      ; border_color           = None
      ; border_width           = None
      ; radius                 = None
      ; style                  = None
      ; hit_radius             = None
      ; hover_background_color = None
      ; hover_border_color     = None
      ; hover_border_width     = None
      ; hover_radius           = None
      }

    method private point_to_js p = object%js
                                     val mutable x = x_to_js p.x
                                     val mutable y = y_to_js p.y
                                   end
    method private point_of_js o = { x = x_of_js o##.x
                                   ; y = y_of_js o##.y
                                   }

    method private ps_add_action data m =
      let coerce = Js.Unsafe.coerce in
      let f p f_c f_d x =
        let open Js.Unsafe in
        CCOpt.iter (fun f -> let a = match Js.Optdef.to_option p with
                               | Some x -> (match Cast.to_js_array x with
                                            | Some a -> a
                                            | None   -> let a = Js.array [||] in f_d a; a)
                               | None -> let a = Js.array [||] in f_d a; a in
                             CCList.iter (fun x -> let p = (f_c @@ f a##.length x) in
                                                   match m with
                                                   | `Back -> a##push p     |> ignore
                                                   | `Front -> a##unshift p |> ignore) data) x in
      f obj##.pointBackgroundColor CSS.Color.js (fun x -> obj##.pointBackgroundColor := coerce x)
        point_f.background_color;
      f obj##.pointBorderColor CSS.Color.js (fun x -> obj##.pointBorderColor := coerce x) point_f.border_color;
      f obj##.pointBorderWidth (fun x -> x) (fun x -> obj##.pointBorderWidth := coerce x) point_f.border_width;
      f obj##.pointRadius (fun x -> x) (fun x -> obj##.pointRadius := coerce x) point_f.radius;
      f obj##.pointStyle Js.string (fun x -> obj##.pointStyle := coerce x) point_f.style;
      f obj##.pointHitRadius (fun x -> x) (fun x -> obj##.pointHitRadius := coerce x) point_f.hit_radius;
      f obj##.pointHoverBackgroundColor CSS.Color.js (fun x -> obj##.pointHoverBackgroundColor := coerce x)
        point_f.hover_background_color;
      f obj##.pointHoverBorderColor CSS.Color.js (fun x -> obj##.pointHoverBorderColor := coerce x)
        point_f.hover_border_color;
      f obj##.pointHoverBorderWidth (fun x -> x) (fun x -> obj##.pointHoverBorderWidth := coerce x)
        point_f.hover_border_width;
      f obj##.pointHoverRadius (fun x -> x) (fun x -> obj##.pointHoverRadius := coerce x) point_f.hover_radius

    method private ps_remove_action n m =
      let coerce = Js.Unsafe.coerce in
      let l = CCList.range 0 (n-1) in
      let f p f_d x =
        CCOpt.iter (fun _ ->
            let a = match Js.Optdef.to_option p with
              | Some x -> (match Cast.to_js_array x with
                           | Some a -> a
                           | None   -> let a = Js.array [||] in f_d a; a)
              | None -> let a = Js.array [||] in f_d a; a in
            CCList.iter (fun _ -> match m with
                                  | `Front -> a##shift |> ignore
                                  | `Back  -> a##pop |> ignore) l) x in
      f obj##.pointBackgroundColor (fun x -> obj##.pointBackgroundColor := coerce x) point_f.background_color;
      f obj##.pointBorderColor (fun x -> obj##.pointBorderColor := coerce x) point_f.border_color;
      f obj##.pointBorderWidth (fun x -> obj##.pointBorderWidth := coerce x) point_f.border_width;
      f obj##.pointRadius (fun x -> obj##.pointRadius := coerce x) point_f.radius;
      f obj##.pointStyle (fun x -> obj##.pointStyle := coerce x) point_f.style;
      f obj##.pointHitRadius (fun x -> obj##.pointHitRadius := coerce x) point_f.hit_radius;
      f obj##.pointHoverBackgroundColor (fun x -> obj##.pointHoverBackgroundColor := coerce x)
        point_f.hover_background_color;
      f obj##.pointHoverBorderColor (fun x -> obj##.pointHoverBorderColor := coerce x) point_f.hover_border_color;
      f obj##.pointHoverBorderWidth (fun x -> obj##.pointHoverBorderWidth := coerce x) point_f.hover_border_width;
      f obj##.pointHoverRadius (fun x -> obj##.pointHoverRadius := coerce x) point_f.hover_radius

    method private ps_push_back x  = self#ps_add_action x `Back
    method private ps_push_front x = self#ps_add_action x `Front

    method private ps_take_back x  = self#ps_remove_action x `Back
    method private ps_take_front x = self#ps_remove_action x `Front

    (* Data methods *)

    method n_points = obj##.data##.length

    method get_data : ('a,'b) point list = List.map self#point_of_js (Array.to_list @@ Js.to_array obj##.data)

    method push_back (x:('a,'b) point)  = obj##.data##push (self#point_to_js x) |> ignore; self#ps_push_back [x]
    method push_front (x:('a,'b) point) = obj##.data##unshift (self#point_to_js x) |> ignore; self#ps_push_front [x]

    method take_back : ('a,'b) point option =
      let p = obj##.data##pop |> Js.Optdef.to_option |> CCOpt.map self#point_of_js in
      self#ps_take_back 1; p
    method take_front : ('a,'b) point option =
      let p = obj##.data##shift |> Js.Optdef.to_option |> CCOpt.map self#point_of_js in
      self#ps_take_front 1; p

    method append_back  (x:('a,'b) point list) = List.iter (fun p -> self#push_back p) x; self#ps_push_back x
    method append_front (x:('a,'b) point list) = List.iter (fun p -> self#push_front p) x; self#ps_push_front x

    method detach_back x : ('a,'b) point list =
      let p = CCList.fold_left (fun acc _ -> self#take_back :: acc) [] @@ CCList.range 0 (x-1)
              |> CCList.filter_map (fun x -> x) in
      self#ps_take_back x; CCList.rev p
    method detach_front x : ('a,'b) point list =
      let p = CCList.fold_left (fun acc _ -> self#take_front :: acc) [] @@ CCList.range 0 (x-1)
              |> CCList.filter_map (fun x -> x) in
      self#ps_take_front x; CCList.rev p

    (* Config setters/getters *)

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
      | Relative_index x -> obj##.fill := Js.Unsafe.coerce @@ Js.string @@ Printf.sprintf "%+d" x
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

    method set_point_background_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.pointBackgroundColor := x)
                        (fun f -> point_f <- { point_f with background_color = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_point_background_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.pointBackgroundColor point_f.background_color
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color
    
    method set_point_border_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.pointBorderColor := x)
                        (fun f -> point_f <- { point_f with border_color = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_point_border_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.pointBorderColor point_f.border_color
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color
    
    method set_point_border_width (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointBorderWidth := x)
                        (fun f -> point_f <- { point_f with border_width = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_border_width : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointBorderWidth point_f.border_width
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_point_radius (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointRadius := x)
                        (fun f -> point_f <- { point_f with radius = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_radius : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointRadius point_f.radius
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_point_style (x : ('a,'b,string) point_setting) =
      set_point_setting (fun x -> obj##.pointStyle := x)
                        (fun f -> point_f <- { point_f with style = Some f})
                        Js.string self#get_data x
    method get_point_style : (('a,'b,string) point_setting) option =
      get_point_setting obj##.pointStyle point_f.style Js.to_string Cast.to_string
    
    method set_point_hit_radius (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointHitRadius := x)
                        (fun f -> point_f <- { point_f with hit_radius = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_hit_radius : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointHitRadius point_f.hit_radius
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_point_hover_background_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.pointHoverBackgroundColor := x)
                        (fun f -> point_f <- { point_f with hover_background_color = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_point_hover_background_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.pointHoverBackgroundColor point_f.hover_background_color
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color
    
    method set_point_hover_border_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.pointHoverBorderColor := x)
                        (fun f -> point_f <- { point_f with hover_border_color = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_point_hover_border_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.pointHoverBorderColor point_f.hover_border_color
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color
    
    method set_point_hover_border_width (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointHoverBorderWidth := x)
                        (fun f -> point_f <- { point_f with hover_border_width = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_hover_border_width : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointHoverBorderWidth point_f.hover_border_width
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_point_hover_radius (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointHoverRadius := x)
                        (fun f -> point_f <- { point_f with hover_radius = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_hover_radius : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointHoverRadius point_f.hover_radius
                        (Js.float_of_number %> int_of_float) Cast.to_int

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
                    |> Js.array;
      self#ps_push_back data.data

  end

end

module Config = struct

  type ('a,'b) axis = ('a,'b) Axes.Cartesian.axis

  class type data_js =
    object
      method datasets  : Dataset.t_js Js.t Js.js_array Js.t Js.prop
    end

  class type options_js =
    object
      inherit Options.t_js
      method showLines : bool Js.t Js.prop
      method spanGaps  : bool Js.t Js.prop
      method scales    : Axes.Cartesian.t_js Js.t Js.prop
    end

  class ['a,'b,'c,'d] options ~(x_axis:('a,'b) axis) ~(y_axis:('c,'d) axis) () =
    let x_axis   = Axes.Cartesian.create x_axis in
    let y_axis   = Axes.Cartesian.create y_axis in
    object

      inherit [options_js] Options.t () as super

      val scales = new Axes.Cartesian.t ~x_axes:[x_axis] ~y_axes:[y_axis] ()

      method x_axis = x_axis
      method y_axis = y_axis

      method set_show_lines x = obj##.showLines := Js.bool x
      method get_show_lines   = Js.to_bool obj##.showLines

      method set_span_gaps x = obj##.spanGaps := Js.bool x
      method get_span_gaps   = Js.to_bool obj##.spanGaps

      method! replace x = super#replace x;
                          scales#replace obj##.scales

      initializer
        obj##.scales := scales#get_obj

    end

  class ['a,'b,'c,'d] t ~(x_axis:('a,'b) axis) ~(y_axis:('c,'d) axis) ~(data:('a,'c) dataset list) () =
    let x_to_js = point_to_js x_axis in
    let y_to_js = point_to_js y_axis in
    let x_of_js = point_of_js x_axis in
    let y_of_js = point_of_js y_axis in

    let datasets = List.map (fun x -> new Dataset.t ~data:x ~x_to_js ~y_to_js ~x_of_js ~y_of_js ()) data in
    let (data:data_js Js.t) = object%js
                                val mutable datasets  = List.map (fun x -> x#get_obj) datasets
                                                        |> Array.of_list
                                                        |> Js.array
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
  inherit [('a,'b,'c,'d) Config.options,Config.options_js] Base_chart.t ~typ:Line
                                                           ~options:config#options
                                                           ~data:(Js.Unsafe.inject config#data) ()

  method config = config
end
