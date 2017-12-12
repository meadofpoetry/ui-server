open Base

class type conf =
  object
    method type_   : Js.js_string Js.t Js.prop
    method data    : Js.Unsafe.any Js.t Js.prop
    method options : Options.t_js Js.t Js.optdef_prop
  end

let constr : (Dom_html.canvasElement Js.t -> conf Js.t -> Base.chart Js.t) Js.constr =
  Js.Unsafe.global##.Chart

class t ?(options:#Options.t option) ~typ ~data () =
  let elt = Tyxml_js.Html.canvas [] |> Tyxml_js.To_dom.of_canvas in
  let conf = [ "type", Js.Unsafe.inject @@ Js.string @@ Base.typ_to_string typ
             ; "data", data ]
             |> Obj.map_cons_option ~f:(fun x -> x#get_obj) "options" options
             |> Array.of_list
             |> Js.Unsafe.obj in
  object
    val chart = new%js constr elt conf
    inherit Components.Widget.widget elt () as super
    method get_canvas_element = elt
    method set_width x        = super#set_attribute "width"  @@ string_of_int x
    method set_height x       = super#set_attribute "height" @@ string_of_int x

    method pp : unit = print_endline @@ Js.to_string @@ Json.output (Js.Unsafe.coerce chart)##.options##.scales
  end
