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
      val h2_widget = elt##querySelector (Js.string @@ "." ^ Dialog.Header.title_class)
                      |> Js.Opt.to_option |> CCOpt.get_exn |> (fun x -> new widget x ())
      inherit widget elt ()
      method title       = h2_widget#text_content
      method set_title s = h2_widget#set_text_content s
    end

end

module Body = struct

  class t ~(content:[ `String of string | `Widgets of #widget list ]) () =
    let content = (match content with
                   | `String  s -> [Html.pcdata s]
                   | `Widgets w -> widgets_to_markup w) in
    let elt = Dialog.Body.create ~content () |> To_dom.of_element in
    object
      inherit widget elt () as super
      method scrollable     = super#add_class Dialog.Body.scrollable_class
      method not_scrollable = super#remove_class Dialog.Body.scrollable_class
    end

end

module Footer = struct

  class t ~(actions:Action.t list) () =
    let elt = Dialog.Footer.create ~children:(widgets_to_markup actions) () |> To_dom.of_footer in
    object
      val mutable actions = actions
      inherit widget elt ()
      method actions = actions
    end

end

class t ?title ?(actions:Action.t list option) ~content () =

  let header_widget = CCOpt.map (fun x -> new Header.t ~title:x ()) title in
  let body_widget   = new Body.t ~content () in
  let footer_widget = CCOpt.map (fun x -> new Footer.t ~actions:x ()) actions in

  let elt = Dialog.create
              ~content:(Of_dom.of_element body_widget#root
                        |> (fun b -> CCOpt.map_or ~default:[b] (fun x -> [b; widget_to_markup x]) footer_widget)
                        |> (fun l -> CCOpt.map_or ~default:l (fun x -> (widget_to_markup x) :: l) header_widget))
              ()
            |> To_dom.of_aside in

  object

    inherit widget elt ()

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.dialog##.MDCDialog##attachTo elt

    method header_widget = header_widget
    method body_widget   = body_widget
    method footer_widget = footer_widget

    method show      = mdc##show ()
    method hide      = mdc##close ()
    method is_opened = Js.to_bool mdc##.open_

  end
