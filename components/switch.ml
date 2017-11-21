open Widget
open Markup
open Tyxml_js

class t ?input_id () =

  let elt = Switch.create ?input_id () |> To_dom.of_div in

  object

    inherit [Dom_html.divElement Js.t] radio_or_cb_widget elt ()

    val input = elt##querySelector (Js.string ("." ^ Switch.native_control_class))
                |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method private input = input

  end
