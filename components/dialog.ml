open Widget
open Tyxml_js
module Dialog = Markup.Dialog

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

module Action = struct

  class t ?ripple ~typ ~label () =

    object(self)

      inherit Button.t ~raised:false ?ripple ~label ()

      initializer
        self#add_class (match typ with
                        | `Accept  -> Dialog.Footer.accept_button_class
                        | `Decline -> Dialog.Footer.cancel_button_class) 

    end

end

module Body = struct

  class t ~content () =

    let content = (match content with
                   | `String  s -> [Html.pcdata s]
                   | `Widgets w -> List.map (fun x -> Of_dom.of_element x#element) w) in

    let elt = Dialog.Body.create ~content () |> To_dom.of_element in

    object

      inherit [Dom_html.element Js.t] widget elt ()

    end

end

class t ?title ~content () =

  let title_widget = CCOpt.map (fun x -> new widget (Dialog.Header.create ~title:x () |> To_dom.of_header) ())
                               title in

  let body_widget = new Body.t ~content () in

  let elt = Dialog.create ~content:([ Of_dom.of_element body_widget#root ]
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
