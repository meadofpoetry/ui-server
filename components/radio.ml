open Widget
open Markup
open Tyxml_js

class type mdc =
  object
    method checked  : bool Js.t Js.prop
    method disabled : bool Js.t Js.prop
    method value    : Js.js_string Js.t Js.prop
  end

class t ?input_id ~name () =

  let elt = Radio.create ?input_id ~name () |> To_dom.of_i in

  object

    inherit [Dom_html.element Js.t] radio_or_cb_widget elt ()

    val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.radio##.MDCRadio##attachTo x)
    val input : Dom_html.inputElement Js.t =
      elt##querySelector (Js.string ("." ^ Radio.native_control_class))
      |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method private input = input

  end
