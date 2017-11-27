open Widget
open Markup

class type mdc =
  object
    method activate   : unit -> unit Js.meth
    method deactivate : unit -> unit Js.meth
    method layout     : unit -> unit Js.meth
    method unbounded  : bool Js.t Js.prop
  end

let attach (elt:#widget) : mdc Js.t = elt#add_class Markup.Ripple.base_class;
                                      Js.Unsafe.global##.mdc##.ripple##.MDCRipple##attachTo elt#root

let remove_accent (elt:#widget)  = elt#remove_class Markup.Ripple.accent_class
let remove_primary (elt:#widget) = elt#remove_class Markup.Ripple.primary_class

let set_accent (elt:#widget)  = remove_primary elt; elt#add_class Markup.Ripple.accent_class
let set_primary (elt:#widget) = remove_accent elt; elt#add_class Markup.Ripple.primary_class

let set_unbounded (elt:#widget)    = elt#set_attribute "data-mdc-ripple-is-unbounded" ""
let remove_unbounded (elt:#widget) = elt#remove_attribute "data-mdc-ripple-is-unbounded"
