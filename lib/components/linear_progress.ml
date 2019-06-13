open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Linear_progress
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

(* TODO remove *)
class type mdc =
  object
    method determinate : bool Js.t Js.writeonly_prop
    method progress : float Js.writeonly_prop
    method buffer : float Js.writeonly_prop
    method reverse : bool Js.t Js.writeonly_prop
    method open_ : unit -> unit Js.meth
    method close : unit -> unit Js.meth
  end

class t () =
  let elt = Tyxml_js.To_dom.of_div @@ Markup.create () in
  object
    inherit Widget.t elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.linearProgress##.MDCLinearProgress##attachTo elt

    method set_indeterminate x = mdc##.determinate := Js.bool @@ not x
    method set_progress x = mdc##.progress := x
    method set_buffer x = mdc##.buffer := x
    method set_reversed x = mdc##.reverse := Js.bool x

    method show () = mdc##open_ ()
    method hide () = mdc##close ()
  end
