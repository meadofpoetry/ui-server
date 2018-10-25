open Base
open Containers

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

class t ~(options:#Options.t) ?width ?height ~typ ~data () =
  let elt = Tyxml_js.Html.(
      canvas ~a:(List.cons_maybe (Option.map a_width width)
                 @@ List.cons_maybe (Option.map a_height height) []) [ ])
            |> Tyxml_js.To_dom.of_canvas in
  let conf =
    [ "type", Js.Unsafe.inject @@ Js.string @@ Base.typ_to_string typ
    ; "options", Js.Unsafe.inject options#get_obj
    ; "data", data ]
    |> Array.of_list
    |> Js.Unsafe.obj in
  object(self)

    val _canvas = Components.Widget.create elt
    val _chart  = new%js constr elt conf

    inherit Components.Widget.t elt () as super

    method init () : unit =
      super#init ();
      options#replace (Js.Unsafe.coerce _chart)##.options

    method canvas = _canvas

    method set_width (x : string) : unit =
      self#canvas#set_attribute "width"  x

    method set_height (x : string) : unit =
      self#canvas#set_attribute "height" x

    method destroy () : unit =
      super#destroy ();
      _chart##destroy

    method layout () : unit =
      super#layout ();
      ignore @@ _chart##resize

    method update : config option -> unit = function
      | Some c -> _chart##update_conf (config_to_obj c)
      | None -> _chart##update

    method reset () : unit =
      _chart##reset

    method render : config option -> unit = function
      | Some c -> _chart##render_conf (config_to_obj c)
      | None -> _chart##render

    method stop () : unit =
      ignore @@ _chart##stop

    method clear () : unit =
      ignore @@ _chart##clear

    method to_png_image () : string =
      Js.to_string _chart##toBase64Image

    method generate_legend () : string =
      Js.to_string _chart##generateLegend

  end
