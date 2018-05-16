open Containers
open Base

(* TODO remove *)
let (<) = Pervasives.(<)

type axis_value_js
type cubic_interpolation_mode = Default | Monotone
type stepped_line             = Disabled | Before | After
type fill                     = Disabled
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

let axis_value_to_js (type a b) (t:(a,b) Axes.Cartesian.axis) : (a -> axis_value_js Js.t) =
  let open Axes in
  let open Axes.Cartesian in
  match t with
  | Linear (_,_,Int,_)        -> float_of_int %> Js.number_of_float %> Js.Unsafe.coerce
  | Linear (_,_,Int32,_)      -> Int32.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Linear (_,_,Int64,_)      -> Int64.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Linear (_,_,Float,_)      -> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (_,_,Int,_)   -> float_of_int %> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (_,_,Int32,_) -> Int32.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (_,_,Int64,_) -> Int64.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Logarithmic (_,_,Float,_) -> Js.number_of_float %> Js.Unsafe.coerce
  | Time (_,_,Unix,_)         -> Int64.to_float %> Js.number_of_float %> Js.Unsafe.coerce
  | Category _                -> Js.string %> Js.Unsafe.coerce

let axis_value_of_js (type a b) (t:(a,b) Axes.Cartesian.axis) : (axis_value_js Js.t -> a) =
  let open Axes in
  let open Axes.Cartesian in
  match t with
  | Linear (_,_,Int,_)        -> Js.Unsafe.coerce %> Js.float_of_number %> int_of_float
  | Linear (_,_,Int32,_)      -> Js.Unsafe.coerce %> Js.float_of_number %> Int32.of_float
  | Linear (_,_,Int64,_)      -> Js.Unsafe.coerce %> Js.float_of_number %> Int64.of_float_exn
  | Linear (_,_,Float,_)      -> Js.Unsafe.coerce %> Js.float_of_number
  | Logarithmic (_,_,Int,_)   -> Js.Unsafe.coerce %> Js.float_of_number %> int_of_float
  | Logarithmic (_,_,Int32,_) -> Js.Unsafe.coerce %> Js.float_of_number %> Int32.of_float
  | Logarithmic (_,_,Int64,_) -> Js.Unsafe.coerce %> Js.float_of_number %> Int64.of_float_exn
  | Logarithmic (_,_,Float,_) -> Js.Unsafe.coerce %> Js.float_of_number
  | Time (_,_,Unix,_)         -> Js.Unsafe.coerce %> Js.float_of_number %> Int64.of_float_exn
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

  class type point_js =
    object
      method x : axis_value_js Js.t Js.prop
      method y : axis_value_js Js.t Js.prop
    end

  class type t_js =
    object
      method data                      : point_js Js.t Js.js_array Js.t Js.prop
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

  type ('a,'b) f_point_props =
    { bg_clr             : (int -> ('a,'b) point -> CSS.Color.t) option
    ; border_clr         : (int -> ('a,'b) point -> CSS.Color.t) option
    ; border_width       : (int -> ('a,'b) point -> int) option
    ; radius             : (int -> ('a,'b) point -> int) option
    ; style              : (int -> ('a,'b) point -> string) option
    ; hit_radius         : (int -> ('a,'b) point -> int) option
    ; hover_bg_clr       : (int -> ('a,'b) point -> CSS.Color.t) option
    ; hover_border_clr   : (int -> ('a,'b) point -> CSS.Color.t) option
    ; hover_border_width : (int -> ('a,'b) point -> int) option
    ; hover_radius       : (int -> ('a,'b) point -> int) option
    }

  let set_point_setting (type a b)
                        (set_val : 'd or_array Js.t -> unit)
                        (set_fun : (int -> ('a,'b) point -> a) -> unit)
                        (to_js : a -> b Js.t)
                        (data:('a,'b) point list)
                        (s : ('a,'b,a) point_setting) =
    match s with
    | `Val c -> set_val @@ Js.Unsafe.coerce @@ to_js c
    | `Lst l -> set_val @@ Js.Unsafe.coerce @@ Js.array @@ Array.of_list @@ List.map to_js l
    | `Fun f -> set_fun f;
                set_val @@ Js.Unsafe.coerce @@ Js.array @@ Array.of_list
                @@ List.mapi (fun i x -> to_js @@ f i x) data
  let get_point_setting (type a b)
                        (v:'d or_array Js.t Js.optdef)
                        (f:(int -> ('a,'b) point -> a) option)
                        (of_js :b Js.t -> a)
                        (cast:'e -> a option) =
    match f with
    | Some f -> Some (`Fun f)
    | None   -> Option.map (fun x -> (match Cast.to_list ~f:of_js x with
                                     | Some l -> `Lst l
                                     | None   -> (match cast x with
                                                  | Some c -> `Val c
                                                  | None   -> failwith "Bad point setting value")))
                @@ Js.Optdef.to_option v

  class ['a,'b] t
                ~(data:('a,'b) dataset)
                ~(x_axis:('a,_) Axes.Cartesian.axis)
                ~(y_axis:('b,_) Axes.Cartesian.axis)
                ~(s_max_x:'a option React.signal)
                ~(s_max_x_push:'a option -> unit)
                () = object(self)

    inherit [t_js] base_option ()

    val x_to_js = axis_value_to_js x_axis
    val y_to_js = axis_value_to_js y_axis
    val x_of_js = axis_value_of_js x_axis
    val y_of_js = axis_value_of_js y_axis
    val delta   = Axes.Cartesian.get_axis_delta x_axis
    val cmp     = Axes.Cartesian.get_axis_cmp_fn x_axis
    val get_min = Axes.Cartesian.get_axis_new_min x_axis

    val mutable f_point_props =
      { bg_clr             = None
      ; border_clr         = None
      ; border_width       = None
      ; radius             = None
      ; style              = None
      ; hit_radius         = None
      ; hover_bg_clr       = None
      ; hover_border_clr   = None
      ; hover_border_width = None
      ; hover_radius       = None
      }

    method private has_functional_point_props =
      Option.is_some f_point_props.bg_clr
      || Option.is_some f_point_props.border_clr
      || Option.is_some f_point_props.border_width
      || Option.is_some f_point_props.radius
      || Option.is_some f_point_props.style
      || Option.is_some f_point_props.hit_radius
      || Option.is_some f_point_props.hover_bg_clr
      || Option.is_some f_point_props.hover_border_clr
      || Option.is_some f_point_props.hover_border_width
      || Option.is_some f_point_props.hover_radius

    method private point_to_js (p:('a,'b) point) : point_js Js.t =
      object%js
        val mutable x = x_to_js p.x
        val mutable y = y_to_js p.y
      end
    method private point_of_js (o:point_js Js.t) =
      { x = x_of_js o##.x
      ; y = y_of_js o##.y }

    method private ps_action action =
      let apply v to_js = function
        | Some f_prop ->
           (match action with
            | `Push (d,m) ->
               let a = (match Cast.to_js_array (Js.Unsafe.get obj v) with
                        | Some x -> x
                        | None   -> let a = Js.array [||] in
                                    Js.Unsafe.set obj v a;
                                    a)
               in
               List.iter (fun x -> let p = to_js @@ f_prop a##.length x in
                                     match m with
                                     | `Tail -> a##push p    |> ignore
                                     | `Head -> a##unshift p |> ignore) d
            | `Remove (n,m) ->
               (match Cast.to_js_array @@ Js.Unsafe.get obj v with
                | Some x -> List.iter (fun _ -> match m with
                                                  | `Head -> x##shift |> ignore
                                                  | `Tail -> x##pop   |> ignore) (List.range 0 (n-1))
                | None   -> Js.Unsafe.delete obj v)
            | `Replace d ->
               let a = Js.array [||] in
               Js.Unsafe.set obj v a;
               List.iter (fun x -> a##push (to_js @@ f_prop a##.length x) |> ignore) d)
        | None -> ()
      in
      apply "pointBackgroundColor"      CSS.Color.js f_point_props.bg_clr;
      apply "pointBorderColor"          CSS.Color.js f_point_props.border_clr;
      apply "pointBorderWidth"          (fun x -> x) f_point_props.border_width;
      apply "pointRadius"               (fun x -> x) f_point_props.radius;
      apply "pointStyle"                Js.string f_point_props.style;
      apply "pointHitRadius"            (fun x -> x) f_point_props.hit_radius;
      apply "pointHoverBackgroundColor" CSS.Color.js f_point_props.hover_bg_clr;
      apply "pointHoverBorderColor"     CSS.Color.js f_point_props.hover_border_clr;
      apply "pointHoverBorderWidth"     (fun x -> x) f_point_props.hover_border_width;
      apply "pointHoverRadius"          (fun x -> x) f_point_props.hover_radius
    method private ps_push data m  = self#ps_action (`Push (data,m))
    method private ps_replace data = self#ps_action (`Replace data)
    method private ps_remove n m   = self#ps_action (`Remove (n,m))

    (* Data methods *)

    method private tl = Js.array_get obj##.data (self#n_points - 1)
                        |> (fun x -> Js.Optdef.map x (fun x -> self#point_of_js x))
                        |> Js.Optdef.to_option
    method sorted_insert_uniq data x =
      let cmp = (fun p1 p2 -> cmp p1.x p2.x) in
      let rec aux x left l = match l with
        | [] -> List.rev_append left [x]
        | y :: tail ->
           match cmp x y with
           | 0 -> List.rev_append left (x :: tail)
           | n when n<0 -> List.rev_append left (x :: l)
           | _ -> aux x (y::left) tail
      in aux x [] data

    method private shift max_x =
      Option.map2 (fun d max ->
          let rec iter = (fun () ->
              (match Js.Optdef.to_option @@ Js.array_get obj##.data 0 with
               | Some js_p -> let p = self#point_of_js js_p in
                              if p.x < get_min max d
                              then (self#ps_remove 1 `Head; obj##.data##shift |> ignore; iter ())
                              else ()
               | None -> ()))
          in
          iter ()) delta max_x
      |> ignore

    method n_points = obj##.data##.length

    method private update_max = match self#tl,React.S.value s_max_x with
      | Some tl,Some max -> if cmp max tl.x < 0 then s_max_x_push @@ Some tl.x else self#shift (Some max)
      | Some tl,None     -> s_max_x_push @@ Some tl.x
      | _ -> ()
    method private set_data_no_sort (data:('a,'b) point list) =
      obj##.data := Js.array @@ Array.of_list @@ List.map (fun x -> self#point_to_js x) data;
      self#ps_replace data;
      self#update_max
    method set_data (data:('a,'b) point list) =
      let data = List.sort_uniq ~cmp:(fun p1 p2 -> cmp p1.x p2.x) data in
      obj##.data := Js.array @@ Array.of_list @@ List.map (fun x -> self#point_to_js x) data;
      self#ps_replace data;
      self#update_max
    method get_data : ('a,'b) point list =
      List.map (fun x -> self#point_of_js x) (Array.to_list @@ Js.to_array obj##.data)

    method push (x:('a,'b) point) = match self#tl with
      | Some tl when cmp tl.x x.x > 0 -> let data = self#sorted_insert_uniq self#get_data x in
                                         self#set_data_no_sort data
      | Some tl when cmp tl.x x.x = 0 -> obj##.data##pop |> ignore;
                                         self#ps_remove 1 `Tail;
                                         self#push x
      | _                             -> obj##.data##push (self#point_to_js x) |> ignore;
                                         self#ps_push [x] `Tail;
                                         self#update_max

    method append (x:('a,'b) point list) = match x with
      | []  -> ()
      | [x] -> self#push x
      | l   -> let cmp_x = (fun p1 p2 -> cmp p1.x p2.x) in
               let l     = List.sort cmp_x l in
               let data  = List.sorted_merge_uniq ~cmp:cmp_x l self#get_data in
               self#set_data_no_sort data

    (* Config setters/getters *)

    method set_label x = obj##.label := Js.string x
    method get_label   = Js.to_string obj##.label

    method set_background_color x = obj##.backgroundColor := CSS.Color.js x
    method get_background_color   = Option.map CSS.Color.ml @@ Js.Optdef.to_option obj##.backgroundColor
    
    method set_border_width x = obj##.borderWidth := x
    method get_border_width   = Js.Optdef.to_option obj##.borderWidth
    
    method set_border_color x = obj##.borderColor := CSS.Color.js x
    method get_border_color   = Option.map CSS.Color.ml @@ Js.Optdef.to_option obj##.borderColor
    
    method set_border_cap_style x = obj##.borderCapStyle := Js.string @@ Canvas.line_cap_to_string x
    method get_border_cap_style   = Option.map (Js.to_string %> Canvas.line_cap_of_string_exn)
                                    @@ Js.Optdef.to_option obj##.borderCapStyle
    
    method set_border_dash x = obj##.borderDash := Js.array @@ Array.of_list x
    method get_border_dash   = Option.map (Js.to_array %> Array.to_list) @@ Js.Optdef.to_option obj##.borderDash
    
    method set_border_dash_offset x = obj##.borderDashOffset := x
    method get_border_dash_offset   = Js.Optdef.to_option obj##.borderDashOffset
    
    method set_border_join_style x = obj##.borderJoinStyle := Js.string @@ Canvas.line_join_to_string x
    method get_border_join_style   = Option.map (Js.to_string %> Canvas.line_join_of_string_exn)
                                     @@ Js.Optdef.to_option obj##.borderJoinStyle
    
    method set_cubic_interpolation_mode x = Js.string @@ cubic_interpolation_mode_to_string x
                                            |> (fun x -> obj##.cubicInterpolationMode := x)
    method get_cubic_interpolation_mode   = Option.map (Js.to_string %> cubic_interpolation_mode_of_string_exn)
                                            @@ Js.Optdef.to_option obj##.cubicInterpolationMode
    
    method set_fill : fill -> unit = function
      | Disabled         -> obj##.fill := Js.Unsafe.coerce Js._false
      | Start            -> obj##.fill := Js.Unsafe.coerce @@ Js.string "start"
      | End              -> obj##.fill := Js.Unsafe.coerce @@ Js.string "end"
      | Origin           -> obj##.fill := Js.Unsafe.coerce @@ Js.string "origin"
      | Absolute_index x -> obj##.fill := Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int x
      | Relative_index x -> obj##.fill := Js.Unsafe.coerce @@ Js.string @@ Printf.sprintf "%+d" x
    method get_fill : fill option =
      Option.map (fun fill -> match Cast.to_bool fill with
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
                        (fun f -> f_point_props <- { f_point_props with bg_clr = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_point_background_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.pointBackgroundColor f_point_props.bg_clr
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color
    
    method set_point_border_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.pointBorderColor := x)
                        (fun f -> f_point_props <- { f_point_props with border_clr = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_point_border_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.pointBorderColor f_point_props.border_clr
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color
    
    method set_point_border_width (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointBorderWidth := x)
                        (fun f -> f_point_props <- { f_point_props with border_width = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_border_width : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointBorderWidth f_point_props.border_width
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_point_radius (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointRadius := x)
                        (fun f -> f_point_props <- { f_point_props with radius = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_radius : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointRadius f_point_props.radius
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_point_style (x : ('a,'b,string) point_setting) =
      set_point_setting (fun x -> obj##.pointStyle := x)
                        (fun f -> f_point_props <- { f_point_props with style = Some f})
                        Js.string self#get_data x
    method get_point_style : (('a,'b,string) point_setting) option =
      get_point_setting obj##.pointStyle f_point_props.style Js.to_string Cast.to_string
    
    method set_point_hit_radius (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointHitRadius := x)
                        (fun f -> f_point_props <- { f_point_props with hit_radius = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_hit_radius : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointHitRadius f_point_props.hit_radius
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_point_hover_background_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.pointHoverBackgroundColor := x)
                        (fun f -> f_point_props <- { f_point_props with hover_bg_clr = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_point_hover_background_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.pointHoverBackgroundColor f_point_props.hover_bg_clr
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color
    
    method set_point_hover_border_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.pointHoverBorderColor := x)
                        (fun f -> f_point_props <- { f_point_props with hover_border_clr = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_point_hover_border_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.pointHoverBorderColor f_point_props.hover_border_clr
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color
    
    method set_point_hover_border_width (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointHoverBorderWidth := x)
                        (fun f -> f_point_props <- { f_point_props with hover_border_width = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_hover_border_width : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointHoverBorderWidth f_point_props.hover_border_width
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_point_hover_radius (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.pointHoverRadius := x)
                        (fun f -> f_point_props <- { f_point_props with hover_radius = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_hover_radius : (('a,'b,int) point_setting) option =
      get_point_setting obj##.pointHoverRadius f_point_props.hover_radius
                        (Js.float_of_number %> int_of_float) Cast.to_int
    
    method set_line_tension x = obj##.lineTension := x
    method get_line_tension   = Js.Optdef.to_option obj##.lineTension
    
    method set_show_line x = obj##.showLine := Js.bool x
    method get_show_line   = Option.map Js.to_bool @@ Js.Optdef.to_option obj##.showLine
    
    method set_span_gaps x = obj##.spanGaps := Js.bool x
    method get_span_gaps   = Option.map Js.to_bool @@ Js.Optdef.to_option obj##.spanGaps
    
    method set_stepped_line : stepped_line -> unit = function
      | Disabled -> obj##.steppedLine := Js.Unsafe.coerce Js._false
      | Before   -> obj##.steppedLine := Js.Unsafe.coerce @@ Js.string "before"
      | After    -> obj##.steppedLine := Js.Unsafe.coerce @@ Js.string "after"
    method get_stepped_line : stepped_line option =
      Option.map (fun x -> match Cast.to_string x with
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
      self#set_data data.data;
      React.S.map (fun x -> self#shift x) s_max_x |> ignore

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
    let get_max = Axes.Cartesian.get_axis_max x_axis in
    let get_min = Axes.Cartesian.get_axis_new_min x_axis in
    let delta   = Axes.Cartesian.get_axis_delta x_axis in
    let max_x   = List.map (fun x -> get_max @@ List.map (fun x -> x.x) x.data) data
                  |> List.filter_map (fun x -> x) |> get_max in
    let set_min_max = Axes.Cartesian.set_axis_min_max x_axis in
    let s_max_x,s_max_x_push = React.S.create max_x in

    let datasets = List.map (fun data -> new Dataset.t ~data ~x_axis ~y_axis ~s_max_x ~s_max_x_push ()) data in
    let (data:data_js Js.t) =
      object%js
        val mutable datasets = List.map (fun x -> x#get_obj) datasets |> Array.of_list |> Js.array
      end in

    object
      val options     = new options ~x_axis ~y_axis ()
      method options  = options
      method datasets = datasets
      method data     = data
      initializer
        React.S.map (fun max_x -> match max_x,delta with
                                  | Some max, Some d -> let min = get_min max d in
                                                        set_min_max options#x_axis (Some min) (Some max)
                                  | _ -> ())
                    s_max_x |> ignore
    end

end

class ['a,'b,'c,'d] t ~(config:('a,'b,'c,'d) Config.t) () = object
  inherit [('a,'b,'c,'d) Config.options,Config.options_js] Base_chart.t ~typ:Line
                                                           ~options:config#options
                                                           ~data:(Js.Unsafe.inject config#data) ()

  method config = config
end
