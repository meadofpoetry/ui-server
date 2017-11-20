open Widget
open Widget.Widgets.Dialog
open Tyxml_js

class type mdc =
  object
    method open_        : bool Js.t Js.prop
    method show         : unit -> unit Js.meth
    method close        : unit -> unit Js.meth
  end

type events =
  { accept : Dom_html.event Js.t Dom_events.Typ.typ
  ; cancel : Dom_html.event Js.t Dom_events.Typ.typ
  }

let events = { accept = Dom_events.Typ.make "MDCDialog:accept"
             ; cancel = Dom_events.Typ.make "MDCDialog:cancel"
             }

class ['a,'b] body content () =

  let inner =
    new widget (Body.create ~content:[match content with
                                      | `String s -> Html.pcdata s
                                      | `Widget w -> let root = (w : 'a :> 'b widget)#root in
                                                     Of_dom.of_element (root :> Dom_html.element Js.t) ]
                            ()
                |> To_dom.of_element) () in

  object

    inherit ['b] widget inner#root ()

    method inner_widget = inner

  end

class t ?title ~content () =

  let title_widget =
    CCOpt.map (fun x -> new widget (Header.create ~title:x () |> To_dom.of_header) ())
              title in

  let body_widget = new body content () in

  let elt = create ~content:([ Of_dom.of_element body_widget#root ]
                             |> (fun l -> (CCOpt.map_or ~default:l
                                                        (fun x -> (Of_dom.of_element x#root) :: l)
                                                        title_widget)))
                   ()
            |> To_dom.of_aside in

  object

    inherit [Dom_html.element Js.t] widget elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.dialog##.MDCDialog##attachTo elt

    method title_widget = title_widget
    method body_widget  = body_widget

    method show      = mdc##show ()
    method hide      = mdc##close ()
    method is_opened = Js.to_bool mdc##.open_

  end
