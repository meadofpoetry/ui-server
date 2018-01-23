open Base

type axis_value_js
type ('a,'b) point =
  { x : 'a
  ; y : 'b
  }
type ('a,'b) dataset =
  { data  : ('a,'b) point list
  ; label : string
  ; stack : string option
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
  | Linear (_,_,Int64,_)      -> Js.Unsafe.coerce %> Js.float_of_number %> Int64.of_float
  | Linear (_,_,Float,_)      -> Js.Unsafe.coerce %> Js.float_of_number
  | Logarithmic (_,_,Int,_)   -> Js.Unsafe.coerce %> Js.float_of_number %> int_of_float
  | Logarithmic (_,_,Int32,_) -> Js.Unsafe.coerce %> Js.float_of_number %> Int32.of_float
  | Logarithmic (_,_,Int64,_) -> Js.Unsafe.coerce %> Js.float_of_number %> Int64.of_float
  | Logarithmic (_,_,Float,_) -> Js.Unsafe.coerce %> Js.float_of_number
  | Time (_,_,Unix,_)         -> Js.Unsafe.coerce %> Js.float_of_number %> Int64.of_float
  | Category _                -> Js.Unsafe.coerce %> Js.to_string

module Dataset = struct

  type bool_or_string
  type 'a or_array
  type ('a,'b,'c) point_setting =
    [ `Val of 'c
    | `Lst of ' c list
    | `Fun of (int -> ('a,'b) point -> 'c) ]

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
      method stack                     : Js.js_string Js.t Js.optdef_prop

      method backgroundColor      : CSS.Color.js_t or_array Js.t Js.optdef_prop
      method borderColor          : CSS.Color.js_t or_array Js.t Js.optdef_prop
      method borderWidth          : int or_array Js.t Js.optdef_prop
      method borderSkipped        : Js.js_string Js.t Js.optdef_prop
      method hoverBackgroundColor : CSS.Color.js_t or_array Js.t Js.optdef_prop
      method hoverBorderColor     : CSS.Color.js_t or_array Js.t Js.optdef_prop
      method hoverBorderWidth     : int or_array Js.t Js.optdef_prop
    end

  type ('a,'b) f_point_props =
    { bg_clr             : (int -> ('a,'b) point -> CSS.Color.t) option
    ; border_clr         : (int -> ('a,'b) point -> CSS.Color.t) option
    ; border_width       : (int -> ('a,'b) point -> int) option
    ; hover_bg_clr       : (int -> ('a,'b) point -> CSS.Color.t) option
    ; hover_border_clr   : (int -> ('a,'b) point -> CSS.Color.t) option
    ; hover_border_width : (int -> ('a,'b) point -> int) option
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
    | None   -> CCOpt.map (fun x -> (match Cast.to_list ~f:of_js x with
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
      ; hover_bg_clr       = None
      ; hover_border_clr   = None
      ; hover_border_width = None
      }

    method private has_functional_point_props =
      CCOpt.is_some f_point_props.bg_clr
      || CCOpt.is_some f_point_props.border_clr
      || CCOpt.is_some f_point_props.border_width
      || CCOpt.is_some f_point_props.hover_bg_clr
      || CCOpt.is_some f_point_props.hover_border_clr
      || CCOpt.is_some f_point_props.hover_border_width

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
               CCList.iter (fun x -> let p = to_js @@ f_prop a##.length x in
                                     match m with
                                     | `Tail -> a##push p    |> ignore
                                     | `Head -> a##unshift p |> ignore) d
            | `Remove (n,m) ->
               (match Cast.to_js_array @@ Js.Unsafe.get obj v with
                | Some x -> CCList.iter (fun _ -> match m with
                                                  | `Head -> x##shift |> ignore
                                                  | `Tail -> x##pop   |> ignore) (CCList.range 0 (n-1))
                | None   -> Js.Unsafe.delete obj v)
            | `Replace d ->
               let a = Js.array [||] in
               Js.Unsafe.set obj v a;
               CCList.iter (fun x -> a##push (to_js @@ f_prop a##.length x) |> ignore) d)
        | None -> ()
      in
      apply "backgroundColor"      CSS.Color.js f_point_props.bg_clr;
      apply "borderColor"          CSS.Color.js f_point_props.border_clr;
      apply "borderWidth"          (fun x -> x) f_point_props.border_width;
      apply "hoverBackgroundColor" CSS.Color.js f_point_props.hover_bg_clr;
      apply "hoverBorderColor"     CSS.Color.js f_point_props.hover_border_clr;
      apply "hoverBorderWidth"     (fun x -> x) f_point_props.hover_border_width;
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
      CCOpt.map2 (fun d max ->
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
      let data = CCList.sort_uniq ~cmp:(fun p1 p2 -> cmp p1.x p2.x) data in
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
               let l     = CCList.sort cmp_x l in
               let data  = CCList.sorted_merge_uniq ~cmp:cmp_x l self#get_data in
               self#set_data_no_sort data

    (* Config setters/getters *)

    method set_label x = obj##.label := Js.string x
    method get_label   = Js.to_string obj##.label

    method set_stack = function
      | Some x -> obj##.stack := Js.string x
      | None   -> Js.Unsafe.delete obj "stack"
    method get_stack   = CCOpt.map Js.to_string @@ Js.Optdef.to_option obj##.stack

    method set_background_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.backgroundColor := x)
                        (fun f -> f_point_props <- { f_point_props with bg_clr = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_background_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.backgroundColor f_point_props.bg_clr
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color

    method set_border_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.borderColor := x)
                        (fun f -> f_point_props <- { f_point_props with border_clr = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_border_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.borderColor f_point_props.border_clr
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color

    method set_border_width (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.borderWidth := x)
                        (fun f -> f_point_props <- { f_point_props with border_width = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_border_width : (('a,'b,int) point_setting) option =
      get_point_setting obj##.borderWidth f_point_props.border_width
                        (Js.float_of_number %> int_of_float) Cast.to_int

    method set_hover_background_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.hoverBackgroundColor := x)
                        (fun f -> f_point_props <- { f_point_props with hover_bg_clr = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_hover_background_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.hoverBackgroundColor f_point_props.hover_bg_clr
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color

    method set_hover_border_color (x : ('a,'b,CSS.Color.t) point_setting) =
      set_point_setting (fun x -> obj##.hoverBorderColor := x)
                        (fun f -> f_point_props <- { f_point_props with hover_border_clr = Some f})
                        (fun c -> Js.string @@ CSS.Color.string_of_t c) self#get_data x
    method get_hover_border_color : (('a,'b,CSS.Color.t) point_setting) option =
      get_point_setting obj##.hoverBorderColor f_point_props.hover_border_clr
                        (CSS.Color.js_t_of_js_string %> CSS.Color.ml) Cast.to_color

    method set_hover_border_width (x : ('a,'b,int) point_setting) =
      set_point_setting (fun x -> obj##.hoverBorderWidth := x)
                        (fun f -> f_point_props <- { f_point_props with hover_border_width = Some f})
                        (fun x -> Js.number_of_float @@ float_of_int x) self#get_data x
    method get_point_border_width : (('a,'b,int) point_setting) option =
      get_point_setting obj##.hoverBorderWidth f_point_props.hover_border_width
                        (Js.float_of_number %> int_of_float) Cast.to_int

    initializer
      self#set_label data.label;
      self#set_data data.data;
      self#set_stack data.stack;
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
      method scales             : Axes.Cartesian.t_js Js.t Js.prop
    end

  class ['a,'b,'c,'d] options ~(x_axis:('a,'b) axis) ~(y_axis:('c,'d) axis) () =
    let x_axis   = Axes.Cartesian.create x_axis in
    let y_axis   = Axes.Cartesian.create y_axis in
    object

      inherit [options_js] Options.t () as super

      val scales = new Axes.Cartesian.t ~x_axes:[x_axis] ~y_axes:[y_axis] ()

      method x_axis = x_axis
      method y_axis = y_axis

      method! replace x = super#replace x;
                          scales#replace obj##.scales

      initializer
        x_axis#grid_lines#set_offset_grid_lines true;
        obj##.scales := scales#get_obj;

    end

  class ['a,'b,'c,'d] t ~(x_axis:('a,'b) axis) ~(y_axis:('c,'d) axis) ~(data:('a,'c) dataset list) () =
    let get_max = Axes.Cartesian.get_axis_max x_axis in
    let get_min = Axes.Cartesian.get_axis_new_min x_axis in
    let delta   = Axes.Cartesian.get_axis_delta x_axis in
    let max_x   = CCList.map (fun x -> get_max @@ CCList.map (fun x -> x.x) x.data) data
                  |> CCList.filter_map (fun x -> x) |> get_max in
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
                                                        set_min_max options#x_axis min max
                                  | _ -> ())
                    s_max_x |> ignore
    end

end

class ['a,'b,'c,'d] t ~(config:('a,'b,'c,'d) Config.t) () = object
  inherit [('a,'b,'c,'d) Config.options,Config.options_js] Base_chart.t ~typ:Bar
                                                           ~options:config#options
                                                           ~data:(Js.Unsafe.inject config#data) ()

  method config = config
end
