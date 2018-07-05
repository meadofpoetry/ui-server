open Tyxml_js

module Markup = Components_markup.Linear_progress.Make(Xml)(Svg)(Html)

(* TODO remove *)
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
  let elt = Markup.create () |> Tyxml_js.To_dom.of_div in
  object
    inherit Widget.widget elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.linearProgress##.MDCLinearProgress##attachTo elt

    method set_indeterminate x = mdc##.determinate := Js.bool @@ not x
    method set_progress x      = mdc##.progress    := x
    method set_buffer   x      = mdc##.buffer      := x
    method set_reversed x      = mdc##.reverse     := Js.bool x

    method show () = mdc##open_ ()
    method hide () = mdc##close ()
  end
