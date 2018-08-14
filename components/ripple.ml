open Tyxml_js

module Markup = Components_markup.Ripple.Make(Xml)(Svg)(Html)

(* TODO remove *)
class type mdc =
  object
    method activate   : unit -> unit Js.meth
    method deactivate : unit -> unit Js.meth
    method layout     : unit -> unit Js.meth
    method unbounded  : bool Js.t Js.prop
  end

let attach (elt:#Widget.t) : mdc Js.t =
  (* elt#add_class Markup.Ripple.base_class; *)
  Js.Unsafe.global##.mdc##.ripple##.MDCRipple##attachTo elt#root

let remove_accent (elt:#Widget.t)  = elt#remove_class Markup.accent_class
let remove_primary (elt:#Widget.t) = elt#remove_class Markup.primary_class

let set_accent (elt:#Widget.t)  = remove_primary elt; elt#add_class Markup.accent_class
let set_primary (elt:#Widget.t) = remove_accent elt; elt#add_class Markup.primary_class

let set_unbounded (elt:#Widget.t)    = elt#set_attribute "data-mdc-ripple-is-unbounded" ""
let remove_unbounded (elt:#Widget.t) = elt#remove_attribute "data-mdc-ripple-is-unbounded"
