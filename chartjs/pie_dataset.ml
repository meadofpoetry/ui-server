open Containers
open Base
open CSS

class type t_js =
  object
    method data                 : Js.number Js.t Js.js_array Js.t Js.prop
    method label                : Js.js_string Js.t Js.prop
    method backgroundColor      : CSS.Color.js_t Js.js_array Js.t Js.optdef_prop
    method borderColor          : CSS.Color.js_t Js.js_array Js.t Js.optdef_prop
    method borderWidth          : Js.number Js.t Js.js_array Js.t Js.optdef_prop
    method hoverBackgroundColor : CSS.Color.js_t Js.js_array Js.t Js.optdef_prop
    method hoverBorderColor     : CSS.Color.js_t Js.js_array Js.t Js.optdef_prop
    method hoverBorderWidth     : Js.number Js.t Js.js_array Js.t Js.optdef_prop
  end

let color_list_to_js_array (x:Color.t list) =
  Js.array @@ Array.of_list @@ List.map Color.js x
let color_list_of_js_array (x:Color.js_t Js.js_array Js.t) =
  List.map Color.ml @@ Array.to_list @@ Js.to_array x

class t_base ~(label:string) (o:t_js Js.t) () =
object(self)

  inherit base_option o ()

  method label : string = Js.to_string _obj##.label
  method set_label x = _obj##.label := Js.string x

  method bg_color : Color.t list =
    Option.map_or ~default:[] color_list_of_js_array
    @@ Js.Optdef.to_option _obj##.backgroundColor
  method set_bg_color (x:Color.t list) : unit =
    _obj##.backgroundColor := color_list_to_js_array x

  method border_color : Color.t list =
    Option.map_or ~default:[] color_list_of_js_array
    @@ Js.Optdef.to_option _obj##.borderColor
  method set_border_color (x:Color.t list) : unit =
    _obj##.borderColor := color_list_to_js_array x

  method border_width : float list =
    Option.map_or ~default:[]
      (fun x -> List.map Js.float_of_number @@ Array.to_list @@ Js.to_array x)
    @@ Js.Optdef.to_option _obj##.borderWidth
  method set_border_width (x:float list) : unit =
    _obj##.borderWidth := Js.array @@ Array.of_list
                          @@ List.map Js.number_of_float x

  method hover_bg_color : Color.t list =
    Option.map_or ~default:[] color_list_of_js_array
    @@ Js.Optdef.to_option _obj##.hoverBackgroundColor
  method set_hover_bg_color (x:Color.t list) : unit =
    _obj##.hoverBackgroundColor := color_list_to_js_array x

  method hover_border_color : Color.t list =
    Option.map_or ~default:[] color_list_of_js_array
    @@ Js.Optdef.to_option _obj##.hoverBorderColor
  method set_hover_border_color (x:Color.t list) : unit =
    _obj##.hoverBorderColor := color_list_to_js_array x

  method hover_border_width : float list =
    Option.map_or ~default:[]
      (fun x -> List.map Js.float_of_number @@ Array.to_list @@ Js.to_array x)
    @@ Js.Optdef.to_option _obj##.hoverBorderWidth
  method set_hover_border_width (x:float list) : unit =
    _obj##.hoverBorderWidth := Js.array @@ Array.of_list
                               @@ List.map Js.number_of_float x

  initializer
    self#set_label label
end

type 'a point =
  { value              : 'a
  ; bg_color           : Color.t
  ; border_color       : Color.t option
  ; border_width       : float option
  ; hover_bg_color     : Color.t option
  ; hover_border_color : Color.t option
  ; hover_border_width : float option
  }

let to_point ?border_color ?border_width
      ?hover_bg_color ?hover_border_color ?hover_border_width
      bg_color value =
  { value
  ; bg_color
  ; border_color
  ; border_width
  ; hover_bg_color
  ; hover_border_color
  ; hover_border_width
  }

type _ fmt =
  | Int   : int fmt
  | Int32 : int32 fmt
  | Int64 : int64 fmt
  | Float : float fmt

let to_js_number : type a. a fmt -> (a -> Js.number Js.t) =
  fun fmt ->
  let open Fun in
  match fmt with
  | Int   -> Js.number_of_float % float_of_int
  | Int32 -> Js.number_of_float % Int32.to_float
  | Int64 -> Js.number_of_float % Int64.to_float
  | Float -> Js.number_of_float

let of_js_number : type a. a fmt -> (Js.number Js.t -> a) =

  fun fmt ->
  let open Fun in
  match fmt with
  | Int   -> int_of_float % Js.float_of_number
  | Int32 -> Int32.of_float % Js.float_of_number
  | Int64 -> Int64.of_float % Js.float_of_number
  | Float -> Js.float_of_number

class ['a] t ~(label:string) (fmt:'a fmt) (data:'a point list) =
  let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object(self)

    inherit t_base ~label o ()

    method label : string =
      Js.to_string _obj##.label
    method set_label (x:string) : unit =
      _obj##.label := Js.string x

    method data =
      List.map (of_js_number fmt)
      @@ Array.to_list @@ Js.to_array _obj##.data
    method set_data x =
      let a = Js.array @@ Array.of_list @@ List.map (to_js_number fmt) x in
      _obj##.data := a

    initializer
      self#set_data @@ List.map (fun x -> x.value) data;
      self#set_bg_color @@ List.map (fun x -> x.bg_color) data;
      (* FIXME don't set if all default *)
      self#set_border_color
      @@ List.map (fun x -> Option.get_or ~default:x.bg_color
                              x.border_color) data;
      self#set_border_width
      @@ List.map (fun x -> Option.get_or ~default:1.0
                              x.border_width) data;
      self#set_hover_bg_color
      @@ List.map (fun x -> Option.get_or ~default:x.bg_color
                              x.hover_bg_color) data;
      self#set_hover_border_color
      @@ List.map (fun x -> Option.get_or ~default:x.bg_color
                              x.hover_border_color) data;
      self#set_hover_border_width
      @@ List.map (fun x -> Option.get_or ~default:1.0
                              x.hover_border_width) data;

  end

