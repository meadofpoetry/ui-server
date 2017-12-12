open Base

module Data = struct

  module Dataset = struct

    type cubic_interpolation_mode = Default | Monotone

    type stepped_line = Bool of bool | Before | After

    type fill = Bool of bool
              | Start
              | End
              | Origin
              | Absolute_index of int
              | Relative_index of signed
    and signed = Plus of int | Minus of int


    let fill_to_js = function
      | Bool b           -> Js.Unsafe.coerce @@ Js.bool b
      | Start            -> Js.Unsafe.coerce @@ Js.string "start"
      | End              -> Js.Unsafe.coerce @@ Js.string "end"
      | Origin           -> Js.Unsafe.coerce @@ Js.string "origin"
      | Absolute_index x -> Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int x
      | Relative_index x -> (match x with
                             | Plus x  -> Js.Unsafe.coerce @@ Js.string ("+" ^ string_of_int x)
                             | Minus x -> Js.Unsafe.coerce @@ Js.string ("-" ^ string_of_int x))

    let cubic_interpolation_mode_to_string = function
      | Default -> "default" | Monotone -> "monotone"

    type bool_or_string

    type 'a or_array

    class type t_js =
      object
        method data                      : 'a Js.js_array Js.t Js.prop
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

    class ['a,'b] t ~data () = object

      inherit [t_js] base_option ()

      method set_label x = obj##.label := Js.string x
      method get_label   = Js.to_string obj##.label

      method set_background_color x = obj##.backgroundColor := CSS.Color.js x
      method get_background_color   = CCOpt.map CSS.Color.ml @@ Js.Optdef.to_option obj##.backgroundColor

      method set_border_width x = obj##.borderWidth := x
      method get_border_width   = Js.Optdef.to_option obj##.borderWidth

      method set_border_color x = obj##.borderColor := CSS.Color.js x
      method get_border_color   = CCOpt.map CSS.Color.ml @@ Js.Optdef.to_option obj##.borderColor

    end

  end

  class type t =
    object
      method datasets : Dataset.t_js Js.t Js.js_array Js.t Js.prop
    end

  let to_obj ?(datasets : Dataset.t_js Js.t list option) () : t Js.t =
    Obj.map_cons_option ~f:(Array.of_list %> Js.array) "datasets" datasets []
    |> Array.of_list
    |> Js.Unsafe.obj

end

module Options = struct

  type ('a,'b) axis = ('a,'b) Axes.Cartesian.axis
  type ('a,'b) axes = ('a,'b) axis * ('a,'b) axis list option

  class ['a,'b,'c,'d] t ~(x_axes:('a,'b) axes) ~(y_axes:('c,'d) axes) () = object

    inherit Options.t ()

    val x_axes = Axes.Cartesian.create (fst x_axes)
                 |> (fun x -> match (snd x_axes) with
                              | Some tl -> x :: List.map Axes.Cartesian.create tl
                              | None    -> CCList.return x)
    val y_axes = Axes.Cartesian.create (fst y_axes)
                 |> (fun x -> match (snd y_axes) with
                              | Some tl -> x :: List.map Axes.Cartesian.create tl
                              | None    -> CCList.return x)

    method x_axes = x_axes
    method y_axes = y_axes

    initializer
      obj##.scales := (new Axes.Cartesian.t ~x_axes ~y_axes ())#get_obj

  end

end

type ('a,'b) point = { x : 'a; y : 'b }

class ['a,'b,'c,'d] t ~(options:('a,'b,'c,'d) Options.t) ~(data:('a,'c) point list list) () = object
  inherit Base_chart.t ~options ~typ:Line ~data:(Js.Unsafe.inject @@ Js.Unsafe.obj [||]) ()

  initializer
    ()
end
