open Base

type position = Top | Left | Bottom | Right
type string_or_string_array

class type t_js =
  object
    inherit Font.t_js
    method display    : bool Js.t Js.prop
    method position   : Js.js_string Js.t Js.prop
    method padding    : int Js.prop
    method lineHeight : float Js.prop         (* FIXME can be a string *)
    method text       : string_or_string_array Js.t Js.prop (* FIXME can be a string array *)
  end

let cast_text_to_string (x : string_or_string_array Js.t) =
  Base.Cast.to_string x |> CCOpt.map (fun x -> `String x)
let cast_text_to_string_list (x : string_or_string_array Js.t)=
  Base.Cast.to_list ~f:Js.to_string x |> CCOpt.map (fun x -> `Lines x)

let position_to_string = function
  | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"
let position_of_string_exn = function
  | "top" -> Top | "left" -> Left | "bottom" -> Bottom | "right" -> Right | _ -> failwith "Bad position string"

type text = [`String of string | `Lines of string list ]

class t () = object(self)
  inherit [t_js] base_option ()
  inherit [t_js] Font.t { size = 12
                        ; color = CSS.Color.rgb 102 102 102
                        ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                        ; style  = `Bold
                        } ()
  method private display_to_js x     = obj##.display    := Js.bool x
  method private position_to_js x    = obj##.position   := Js.string @@ position_to_string x
  method private padding_to_js x     = obj##.padding    := x
  method private line_height_to_js x = obj##.lineHeight := x
  method private text_to_js (x:text) = obj##.text := (match x with
                                                      | `String s -> Js.Unsafe.coerce @@ Js.string s
                                                      | `Lines  l -> List.map Js.string l
                                                                     |> Array.of_list
                                                                     |> Js.array
                                                                     |> Js.Unsafe.coerce)

  method set_display x = self#display_to_js x
  method get_display   = Js.to_bool obj##.display

  method set_position x = self#position_to_js x
  method get_position   = position_of_string_exn @@ Js.to_string obj##.position

  method set_padding x = self#padding_to_js x
  method get_padding   = obj##.padding

  method set_line_height x = self#line_height_to_js x
  method get_line_height   = obj##.lineHeight

  method set_text x      = self#text_to_js x
  method get_text : text = cast_text_to_string obj##.text
                           |> (function
                               | Some x -> x
                               | None   -> CCOpt.get_exn @@ cast_text_to_string_list obj##.text)

  initializer
    (* Fill object with default values *)
    self#display_to_js     false;
    self#position_to_js    Top;
    self#padding_to_js     10;
    self#line_height_to_js 1.2;
    self#text_to_js        (`String "")
end
