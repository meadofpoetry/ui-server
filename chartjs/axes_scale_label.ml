open Base

type number_or_object

type padding = [ `Number of int | `Object of padding_obj ]
and padding_obj = { top : int; bottom : int }

class type coord =
  object
    method top    : int Js.prop
    method bottom : int Js.prop
  end

class type t_js =
  object
    inherit Font.t_js
    method display     : bool Js.t Js.prop
    method labelString : Js.js_string Js.t Js.prop
    method lineHeight  : float Js.prop (* FIXME type. may be string *)
    method padding     : number_or_object Js.t Js.prop
  end

(** The scale label configuration defines options for the scale title.
 ** Note that this only applies to cartesian axes.
 **)
class t () = object(self)

  inherit [t_js] base_option ()
  inherit [t_js] Font.t { size = 12
                        ; color = CSS.Color.rgb 102 102 102
                        ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                        ; style  = `Normal
                        } ()

  (** If true, display the axis title. **)
  method display : bool = Js.to_bool obj##.display
  method set_display x = obj##.display := Js.bool x

  (** The text for the title. (i.e. "# of People" or "Response Choices"). **)
  method label_string : string = Js.to_string obj##.labelString
  method set_label_string x = obj##.labelString := Js.string x

  (** Height of an individual line of text **)
  method line_height   = obj##.lineHeight
  method set_line_height x = obj##.lineHeight := x

  (** Padding to apply around scale labels. Only top and bottom are implemented. **)
  method padding : padding =
    match Cast.to_int obj##.padding with
    | Some x -> `Number x
    | None   -> `Object { top    = (Js.Unsafe.coerce obj##.padding)##.top
                        ; bottom = (Js.Unsafe.coerce obj##.padding)##.bottom
                        }
  method set_padding : padding -> unit = function
    | `Number n -> obj##.padding := Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int n
    | `Object o -> let (v : coord Js.t) = object%js
                                            val mutable top    = o.top
                                            val mutable bottom = o.bottom
                                          end
                   in obj##.padding := Js.Unsafe.coerce v

  initializer
    self#set_display false;
    self#set_label_string "";
    self#set_line_height 1.2;
    self#set_padding @@ `Number 4
end
