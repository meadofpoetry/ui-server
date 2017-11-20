open Widget
open Widget.Widgets.Slider
open Tyxml_js

class type mdc =
  object
    method value          : float Js.prop
    method min            : float Js.prop
    method max            : float Js.prop
    method step           : float Js.prop
    method disabled       : bool Js.t Js.prop
    method layout         : unit -> unit Js.meth
    method stepUp         : unit -> unit Js.meth
    method stepDown       : unit -> unit Js.meth
    method stepUp_value   : float -> unit Js.meth
    method stepDown_value : float -> unit Js.meth
  end

class type event =
  object
    inherit Dom_html.event
    method detail : mdc Js.t Js.readonly_prop
  end

type events =
  { input  : event Js.t Dom_events.Typ.typ
  ; change : event Js.t Dom_events.Typ.typ
  }

let events =
  { input  = Dom_events.Typ.make "MDCSlider:input"
  ; change = Dom_events.Typ.make "MDCSlider:change"
  }

class t ?discrete ?markers () =

  let elt = create ?discrete ?markers () |> To_dom.of_div in

  object(self)

    inherit [Dom_html.divElement Js.t] widget elt ()

    val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.slider##.MDCSlider##attachTo x)

    method value       = mdc##.value
    method set_value x = mdc##.value := x
    method min         = mdc##.min
    method set_min x   = mdc##.min := x
    method max         = mdc##.max
    method set_max x   = mdc##.max := x
    method step        = mdc##.step
    method set_step x  = mdc##.step := x

    method disabled        = Js.to_bool mdc##.disabled
    method disable         = mdc##.disabled := Js._true
    method enable          = mdc##.disabled := Js._false
    method toggle_disabled = mdc##.disabled := Js.bool @@ not self#disabled

    method layout         = mdc##layout ()
    method step_down      = mdc##stepDown ()
    method step_down_by x = mdc##stepDown_value x
    method step_up        = mdc##stepUp ()
    method step_up_by x   = mdc##stepUp_value x

  end
