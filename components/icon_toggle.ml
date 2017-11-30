class type change_event =
  object
    inherit Dom_html.event
    method detail : < isOn : bool Js.t Js.readonly_prop > Js.t Js.readonly_prop
  end

let change_event = Dom_events.Typ.make "MDCIconToggle:change"

class type mdc =
  object
    method on       : bool Js.t Js.prop
    method disabled : bool Js.t Js.prop
  end

class t ~on_data ~off_data () =

  let elt = Markup.Icon_toggle.create ~on_data ~off_data () |> Tyxml_js.To_dom.of_i in
  let s_state,s_state_push = React.S.create false in

  object(self)
    inherit Widget.widget elt ()
    val mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.iconToggle##.MDCIconToggle##attachTo x)

    method set_disabled x = mdc##.disabled := Js.bool x
    method get_disabled   = Js.to_bool mdc##.disabled

    method set_on x       = mdc##.on := Js.bool x; s_state_push x
    method get_on         = Js.to_bool mdc##.on

    method s_state = s_state

    initializer
      Dom_events.listen self#root change_event
                        (fun _ (e:change_event Js.t) -> s_state_push @@ Js.to_bool @@ e##.detail##.isOn; false)
      |> ignore

  end
