class type mdc =
  object
    method activate   : unit -> unit Js.meth
    method deactivate : unit -> unit Js.meth
    method layout     : unit -> unit Js.meth
    method unbounded  : bool Js.t Js.prop
  end

let attach (elt:#Widget.widget) : mdc Js.t = elt#add_class Markup.Ripple.base_class;
                                             Js.Unsafe.global##.mdc##.ripple##.MDCRipple##attachTo elt#root

let remove_accent (elt:#Widget.widget)  = elt#remove_class Markup.Ripple.accent_class
let remove_primary (elt:#Widget.widget) = elt#remove_class Markup.Ripple.primary_class

let set_accent (elt:#Widget.widget)  = remove_primary elt; elt#add_class Markup.Ripple.accent_class
let set_primary (elt:#Widget.widget) = remove_accent elt; elt#add_class Markup.Ripple.primary_class

let set_unbounded (elt:#Widget.widget)    = elt#set_attribute "data-mdc-ripple-is-unbounded" ""
let remove_unbounded (elt:#Widget.widget) = elt#remove_attribute "data-mdc-ripple-is-unbounded"
