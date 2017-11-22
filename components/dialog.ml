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
        self#add_class Dialog.Footer.button_class;
        self#add_class (match typ with
                        | `Accept  -> Dialog.Footer.accept_button_class
                        | `Decline -> Dialog.Footer.cancel_button_class) 

    end

end

module Header = struct

  class t ~title () =

    let elt = Dialog.Header.create ~title () |> To_dom.of_header in

    object

      inherit [Dom_html.element Js.t] widget elt ()

      method title = title

    end

end

module Body = struct

  class t ~content () =

    let content = (match content with
                   | `String  s -> [Html.pcdata s]
                   | `Widgets w -> List.map (fun x -> Of_dom.of_element x#element) w) in

    let elt = Dialog.Body.create ~content () |> To_dom.of_element in

    object

      inherit [Dom_html.element Js.t] widget elt () as super

      method scrollable     = super#add_class Dialog.Body.scrollable_class
      method not_scrollable = super#remove_class Dialog.Body.scrollable_class

    end

end

module Footer = struct

  class t ~(actions:Action.t list) () =

    let elt = Dialog.Footer.create ~children:(List.map (fun x -> Of_dom.of_button x#root) actions) ()
              |> To_dom.of_footer in

    object

      inherit [Dom_html.element Js.t] widget elt ()

      method actions = actions

    end

end

class t ?title ?(actions:Action.t list option) ~content () =

  let header_widget = CCOpt.map (fun x -> new Header.t ~title:x ()) title in
  let body_widget   = new Body.t ~content () in
  let footer_widget = CCOpt.map (fun x -> new Footer.t ~actions:x ()) actions in

  let elt = Dialog.create
              ~content:(Of_dom.of_element body_widget#root
                        |> (fun b -> CCOpt.map_or ~default:[b] (fun x -> [b; Of_dom.of_element x#root])
                                                  footer_widget)
                        |> (fun l -> CCOpt.map_or ~default:l (fun x -> (Of_dom.of_element x#root) :: l)
                                                  header_widget))
              ()
            |> To_dom.of_aside in

  object

    inherit [Dom_html.element Js.t] widget elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.dialog##.MDCDialog##attachTo elt

    method header_widget = header_widget
    method body_widget   = body_widget
    method footer_widget = footer_widget

    method show      = mdc##show ()
    method hide      = mdc##close ()
    method is_opened = Js.to_bool mdc##.open_

  end
