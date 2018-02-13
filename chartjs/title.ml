open Containers
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

let position_to_string = function
  | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"
let position_of_string_exn = function
  | "top" -> Top | "left" -> Left | "bottom" -> Bottom | "right" -> Right | _ -> failwith "Bad position string"

class t () = object(self)
  inherit [t_js] base_option ()
  inherit [t_js] Font.t { size = 12
                        ; color = CSS.Color.rgb 102 102 102
                        ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                        ; style  = `Bold
                        } ()

  method set_display x = obj##.display := Js.bool x
  method get_display   = Js.to_bool obj##.display

  method set_position x = obj##.position := Js.string @@ position_to_string x
  method get_position   = position_of_string_exn @@ Js.to_string obj##.position

  method set_padding x = obj##.padding := x
  method get_padding   = obj##.padding

  method set_line_height x = obj##.lineHeight := x
  method get_line_height   = obj##.lineHeight

  method set_text x =
    obj##.text := (match String.lines x with
                   | [s] -> Js.Unsafe.coerce @@ Js.string s
                   | l   -> List.map Js.string l |> Array.of_list |> Js.array |> Js.Unsafe.coerce)
  method get_text =
    match Cast.to_list ~f:Js.to_string obj##.text with
    | Some l -> String.unlines l
    | None   -> (match Cast.to_string obj##.text with
                 | Some s -> s
                 | None   -> failwith "Bad title text value")

  initializer
    (* Fill object with default values *)
    self#set_display     false;
    self#set_position    Top;
    self#set_padding     10;
    self#set_line_height 1.2;
    self#set_text        ""
end
