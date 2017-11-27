class type mdc =
  object
    method determinate : bool Js.t Js.writeonly_prop
    method progress    : float Js.writeonly_prop
    method buffer      : float Js.writeonly_prop
    method reverse     : bool Js.t Js.writeonly_prop
    method open_       : unit -> unit Js.meth
    method close       : unit -> unit Js.meth
  end

class t () =

  let elt = Markup.Linear_progress.create () |> Tyxml_js.To_dom.of_div in

  object
    inherit Widget.widget elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.linearProgress##.MDCLinearProgress##attachTo elt

    method determinate   = mdc##.determinate := Js._true
    method indeterminate = mdc##.determinate := Js._false

    method set_progress x = mdc##.progress := x
    method set_buffer   x = mdc##.buffer := x

    method reversed = mdc##.reverse := Js._false
    method straight = mdc##.reverse := Js._true

    method show = mdc##open_ ()
    method hide = mdc##close ()
  end
