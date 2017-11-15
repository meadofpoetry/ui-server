open Base

type position = Top | Left | Bottom | Right

class type t =
  object
    inherit Font.t
    method display    : bool Js.t Js.optdef_prop
    method position   : Js.js_string Js.t Js.optdef_prop
    method padding    : int Js.optdef_prop
    method lineHeight : float Js.optdef_prop (* FIXME can be a string *)
    method text       : Js.js_string Js.t Js.optdef_prop (* FIXME can be a string array *)
  end

let position_to_string = function
  | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"

let to_obj ?display ?position ?font ?padding ?line_height ?text () : t Js.t =
  Obj.map_cons_option ~f:Js.bool "display" display []
  |> Obj.map_cons_option ~f:(Legend.position_to_string %> Js.string) "position" position
  |> Obj.cons_option "padding" padding
  |> Obj.cons_option "lineHeight" line_height
  |> Obj.map_cons_option ~f:Js.string "text" text
  |> Array.of_list
  |> (fun x -> match font with
               | Some font -> Array.append x @@ Font.to_array font
               | None      -> x)
  |> Js.Unsafe.obj
