open Widget
open Markup
open Tyxml_js

class type change_event =
  object
    inherit Dom_html.event
    method detail : < isOn : bool Js.t Js.readonly_prop > Js.t Js.readonly_prop
  end

type events =
  { change : change_event Js.t Dom_events.Typ.typ
  }

let events =
  { change = Dom_events.Typ.make "MDCIconToggle:change"
  }

class type mdc =
  object
    method on       : bool Js.t Js.prop
    method disabled : bool Js.t Js.prop
  end

class t ~on_data ~off_data () =

  let elt = Icon_toggle.create ~on_data ~off_data () |> To_dom.of_i in

  object
    inherit widget elt ()
    val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.iconToggle##.MDCIconToggle##attachTo x)

    method disabled = Js.to_bool @@ mdc##.disabled
    method disable  = mdc##.disabled := Js._true
    method enable   = mdc##.disabled := Js._false

    method is_on = Js.to_bool @@ mdc##.on
    method on    = mdc##.on := Js._true
    method off   = mdc##.on := Js._false

  end
