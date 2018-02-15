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

class t ?discrete ?markers ?step ?(min=0.0) ?(max=100.0) ?value  () =

  let elt = Markup.Slider.create ?discrete ?markers ?value ?step ~min ~max () |> Tyxml_js.To_dom.of_div in
  let s_input,s_input_push = React.S.create @@ CCOpt.get_or ~default:min value in
  let s_value,s_value_push = React.S.create @@ CCOpt.get_or ~default:min value in
  let mdc : mdc Js.t = elt |> (fun x -> Js.Unsafe.global##.mdc##.slider##.MDCSlider##attachTo x) in

  object(self)

    inherit Widget.widget elt ()

    method set_value x = mdc##.value := x
    method get_value   = mdc##.value

    method set_min x   = mdc##.min := x
    method get_min     = mdc##.min

    method set_max x   = mdc##.max := x
    method get_max     = mdc##.max

    method set_step x  = mdc##.step := x
    method get_step    = mdc##.step

    method get_disabled   = Js.to_bool mdc##.disabled
    method set_disabled x = mdc##.disabled := Js.bool x

    method layout         = mdc##layout ()
    method step_down      = mdc##stepDown ()
    method step_down_by x = mdc##stepDown_value x
    method step_up        = mdc##stepUp ()
    method step_up_by x   = mdc##stepUp_value x

    method s_input = s_input
    method s_value = s_value

    initializer
      Dom_events.listen self#root events.input  (fun _ _ -> s_input_push self#get_value; false) |> ignore;
      Dom_events.listen self#root events.change (fun _ _ -> s_value_push self#get_value; false) |> ignore;

  end
