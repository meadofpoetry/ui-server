open Base

type config =
  { duration : int option
  ; is_lazy  : bool option
  ; easing   : Base.easing option
  }

class type config_js =
  object
    method duration : int Js.writeonly_prop
    method lazy_    : bool Js.t Js.writeonly_prop
    method easing   : Js.js_string Js.t Js.optdef Js.writeonly_prop
  end

let config_to_obj (c:config) : config_js Js.t =
  Obj.cons_option "duration" c.duration []
  |> Obj.map_cons_option ~f:Js.bool "lazy" c.is_lazy
  |> Obj.map_cons_option ~f:(easing_to_string %> Js.string) "easing" c.easing
  |> Array.of_list
  |> Js.Unsafe.obj

class type chart_config =
  object
    method type_   : Js.js_string Js.t Js.prop
    method data    : Js.Unsafe.any Js.t Js.prop
    method options : Options.t_js Js.t Js.optdef_prop
  end

class type chart =
  object
    method destroy        : unit -> unit Js.meth
    method update         : unit -> unit Js.meth
    method update_conf    : config_js Js.t -> unit Js.meth
    method reset          : unit -> unit Js.meth
    method render         : unit -> unit Js.meth
    method render_conf    : config_js Js.t -> unit Js.meth
    method stop           : unit -> chart Js.t Js.meth
    method resize         : unit -> chart Js.t Js.meth
    method clear          : unit -> chart Js.t Js.meth
    method toBase64Image  : unit -> Js.js_string Js.t Js.meth
    method generateLegend : unit -> Js.js_string Js.t Js.meth
  end

let constr : (Dom_html.canvasElement Js.t -> chart_config Js.t -> chart Js.t) Js.constr =
  Js.Unsafe.global##.Chart

class ['a] t ~(options:'a) ~typ ~data () =
  let elt = Tyxml_js.Html.canvas [] |> Tyxml_js.To_dom.of_canvas in
  let conf = [ "type", Js.Unsafe.inject @@ Js.string @@ Base.typ_to_string typ
             ; "options", Js.Unsafe.inject options#get_obj
             ; "data", data ]
             |> Array.of_list
             |> Js.Unsafe.obj in
  object

    constraint 'a = #Options.t

    val chart = new%js constr elt conf
    inherit Components.Widget.widget elt () as super

    method get_canvas_element = elt
    method set_width x        = super#set_attribute "width"  @@ string_of_int x
    method set_height x       = super#set_attribute "height" @@ string_of_int x

    method destroy = chart##destroy ()
    method update  = function
      | Some c -> chart##update_conf (config_to_obj c)
      | None   -> chart##update ()
    method reset   = chart##reset ()
    method render  = function
      | Some c -> chart##render_conf (config_to_obj c)
      | None   -> chart##render ()
    method stop    = chart##stop ()   |> ignore
    method resize  = chart##resize () |> ignore
    method clear   = chart##clear ()  |> ignore

    method to_png_image    = chart##toBase64Image () |> Js.to_string
    method generate_legend = chart##generateLegend () |> Js.to_string

    method options = options

    initializer
      options#replace (Js.Unsafe.coerce chart)##.options
  end
