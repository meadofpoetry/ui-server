open Markup

class type mdc =
  object
    method activate   : unit -> unit Js.meth
    method deactivate : unit -> unit Js.meth
    method layout     : unit -> unit Js.meth
    method unbounded  : bool Js.t Js.prop
  end

let attach elt : mdc Js.t = elt#add_class Ripple.base_class;
                            Js.Unsafe.global##.mdc##.ripple##.MDCRipple##attachTo elt#element

let remove_accent elt    = elt#remove_class Ripple.accent_class
let remove_primary elt   = elt#remove_class Ripple.primary_class

let set_accent elt       = remove_primary elt; elt#add_class Ripple.accent_class
let set_primary elt      = remove_accent elt; elt#add_class Ripple.primary_class

let set_unbounded elt    = elt#set_attribute "data-mdc-ripple-is-unbounded" ""
let remove_unbounded elt = elt#remove_attribute "data-mdc-ripple-is-unbounded"
