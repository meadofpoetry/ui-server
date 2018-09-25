open Containers
open Base
open CSS

type render =
  [ `Label
  | `Value
  | `Percentage
  | `Image ]

type position =
  [ `Default
  | `Border
  | `Outside
  ]

let render_to_string = function
  | `Label      -> "label"
  | `Value      -> "value"
  | `Percentage -> "percentage"
  | `Image      -> "image"

let render_of_string = function
  | "label"      -> `Label
  | "value"      -> `Value
  | "percentage" -> `Percentage
  | "image"      -> `Image
  | _            -> failwith "bad render value"

let position_to_string = function
  | `Default -> "default"
  | `Border  -> "border"
  | `Outside -> "outside"

let position_of_string = function
  | "default" -> `Default
  | "border"  -> `Border
  | "outside" -> `Outside
  | _         -> failwith "bad position value"

class type t_js =
  object
    method fontColor             : CSS.Color.js_t Js.js_array Js.t Js.optdef_prop
    method render                : Js.js_string Js.t Js.prop
    method precision             : int Js.prop
    method showZero              : bool Js.t Js.prop
    method textShadow            : bool Js.t Js.prop
    method shadowBlur            : int Js.prop
    method shadowOffsetX         : int Js.prop
    method arc                   : bool Js.t Js.prop
    method position              : Js.js_string Js.t Js.prop
    method overlap               : bool Js.t Js.prop
    method showActualPercentages : bool Js.t Js.prop
    method outsidePadding        : int Js.prop
    method textMargin            : int Js.prop
  end

let color_list_to_js_array (x:Color.t list) =
  Js.array @@ Array.of_list @@ List.map Color.js x
let color_list_of_js_array (x:Color.js_t Js.js_array Js.t) =
  List.map Color.ml @@ Array.to_list @@ Js.to_array x

class t () =
  let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object(self)
    inherit base_option o ()

    method font_color : Color.t list =
      Option.map_or ~default:[] color_list_of_js_array
      @@ Js.Optdef.to_option _obj##.fontColor
    method set_font_color (x:Color.t list) : unit =
      _obj##.fontColor := color_list_to_js_array x

    method render : render =
      render_of_string @@ Js.to_string _obj##.render
    method set_render (x:render) : unit =
      _obj##.render := Js.string @@ render_to_string x

    method position : position =
      position_of_string @@ Js.to_string _obj##.position
    method set_position (x:position) : unit =
      _obj##.position := Js.string @@ position_to_string x

    initializer
      self#set_render `Value;
      self#set_position `Default;

  end

