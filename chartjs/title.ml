open Containers
open Base

type position = [`Top | `Left | `Bottom | `Right]
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
  | `Top -> "top" | `Left -> "left" | `Bottom -> "bottom" | `Right -> "right"
let position_of_string_exn = function
  | "top" -> `Top | "left" -> `Left | "bottom" -> `Bottom | "right" -> `Right | _ -> failwith "Bad position string"

class t () = object(self)
  inherit [t_js] base_option ()
  inherit [t_js] Font.t { size = 12
                        ; color = CSS.Color.rgb 102 102 102
                        ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                        ; style  = `Bold
                        } ()

  (** is the title shown **)
  method display : bool = Js.to_bool obj##.display
  method set_display x = obj##.display := Js.bool x

  (** Position of title. **)
  method position : position= position_of_string_exn @@ Js.to_string obj##.position
  method set_position (x:position) = obj##.position := Js.string @@ position_to_string x

  (** Number of pixels to add above and below the title text. **)
  method padding : int = obj##.padding
  method set_padding x = obj##.padding := x

  (** Height of an individual line of text **)
  (** FIXME not only float **)
  method line_height : float = obj##.lineHeight
  method set_line_height x = obj##.lineHeight := x

  (** Title text to display. **)
  method text : string = match Cast.to_list ~f:Js.to_string obj##.text with
    | Some l -> String.unlines l
    | None   -> (match Cast.to_string obj##.text with
                 | Some s -> s
                 | None   -> failwith "Bad title text value")
  method set_text (x:string) =
    obj##.text := (match String.lines x with
                   | [s] -> Js.Unsafe.coerce @@ Js.string s
                   | l   -> List.map Js.string l |> Array.of_list |> Js.array |> Js.Unsafe.coerce)

  initializer
    self#set_display     false;
    self#set_position    `Top;
    self#set_padding     10;
    self#set_line_height 1.2;
    self#set_text        ""
end
