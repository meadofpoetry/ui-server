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
    method destroy        : unit Js.meth
    method update         : unit Js.meth
    method update_conf    : config_js Js.t -> unit Js.meth
    method reset          : unit Js.meth
    method render         : unit Js.meth
    method render_conf    : config_js Js.t -> unit Js.meth
    method stop           : chart Js.t Js.meth
    method resize         : chart Js.t Js.meth
    method clear          : chart Js.t Js.meth
    method toBase64Image  : Js.js_string Js.t Js.meth
    method generateLegend : Js.js_string Js.t Js.meth
  end

let constr : (Dom_html.canvasElement Js.t -> chart_config Js.t -> chart Js.t) Js.constr =
  Js.Unsafe.global##.Chart

class ['a,'b] t ~(options:'a) ~typ ~data () =
  let elt = Tyxml_js.Html.canvas [] |> Tyxml_js.To_dom.of_canvas in
  let conf = [ "type", Js.Unsafe.inject @@ Js.string @@ Base.typ_to_string typ
             ; "options", Js.Unsafe.inject options#get_obj
             ; "data", data ]
             |> Array.of_list
             |> Js.Unsafe.obj in
  object(self)

    constraint 'b = #Options.t_js
    constraint 'a = 'b #Options.t

    val _canvas = Components.Widget.create elt
    val _chart  = new%js constr elt conf
    inherit Components.Widget.widget elt () as super

    method canvas        = _canvas
    method set_width x   = self#canvas#set_attribute "width"  @@ string_of_int x
    method set_height x  = self#canvas#set_attribute "height" @@ string_of_int x

    method destroy () = _chart##destroy
    method update  = function
      | Some c -> _chart##update_conf (config_to_obj c)
      | None   -> _chart##update
    method reset () = _chart##reset
    method render = function
      | Some c -> _chart##render_conf (config_to_obj c)
      | None   -> _chart##render
    method stop   () = _chart##stop   |> ignore
    method layout () = _chart##resize |> ignore
    method clear  () = _chart##clear  |> ignore

    method to_png_image ()    = _chart##toBase64Image  |> Js.to_string
    method generate_legend () = _chart##generateLegend |> Js.to_string

    initializer
      options#replace (Js.Unsafe.coerce _chart)##.options
  end
