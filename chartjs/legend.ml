open Base

type position = Top | Left | Bottom | Right

class type item =
  object
    method text           : Js.js_string Js.t Js.prop
    method fillStyle      : Js.js_string Js.t Js.prop (* FIXME to color type *)
    method hidden         : bool Js.t Js.prop
    method lineCap        : Js.js_string Js.t Js.prop
    method lineDash       : Js.number Js.t Js.js_array Js.t Js.prop
    method lineDashOffset : Js.number Js.t Js.prop
    method lineJoin       : Js.js_string Js.t Js.prop
    method lineWidth      : Js.number Js.t Js.prop
    method strokeStyle    : Js.js_string Js.t Js.prop (* FIXME to color type *)
    method pointStyle     : Js.js_string Js.t Js.prop
  end

class type labels =
  object
    inherit Font.t
    method boxWidth       : float Js.optdef_prop
    method padding        : int Js.optdef_prop
    method generateLabels : (unit -> unit) Js.optdef_prop (* FIXME *)
    method filter         : (unit -> unit) Js.optdef_prop (* FIXME *)
    method usePointStyle  : bool Js.t Js.optdef_prop
  end

class type t =
  object
    method display_   : bool Js.t Js.optdef_prop
    method position_  : Js.js_string Js.t Js.optdef_prop
    method fullWidth_ : bool Js.t Js.optdef_prop
    (* method onClick_   : (chart Js.t,(Dom_html.event Js.t -> item Js.t)) Js.meth_callback Js.optdef_prop *)
    (* method onHover_   : (chart Js.t,(Dom_html.event Js.t -> item Js.t)) Js.meth_callback Js.optdef_prop *)
    method reverse_   : bool Js.t Js.optdef_prop
    method labels_    : labels Js.t Js.optdef_prop
  end

let position_to_string = function
  | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"

let labels_to_obj ?box_width ?font ?padding ?use_point_style () : labels Js.t =
  Obj.cons_option "boxWidth" box_width []
  |> Obj.cons_option "padding" padding
  |> Obj.map_cons_option ~f:Js.bool "usePointStyle" use_point_style
  |> Array.of_list
  |> (fun x -> match font with
               | Some font -> Array.append x @@ Font.to_array font
               | None      -> x)
  |> Js.Unsafe.obj

let to_obj ?display ?position ?full_width ?on_click ?on_hover ?reverse ?labels () : t Js.t =
  Obj.map_cons_option ~f:Js.bool "display" display []
  |> Obj.map_cons_option ~f:(position_to_string %> Js.string) "position" position
  |> Obj.map_cons_option ~f:Js.bool "fullWidth" full_width
  |> Obj.cons_option "onClick" on_click
  |> Obj.cons_option "onHover" on_hover
  |> Obj.map_cons_option ~f:Js.bool "reverse" reverse
  |> Obj.cons_option "labels" labels
  |> Array.of_list
  |> Js.Unsafe.obj
