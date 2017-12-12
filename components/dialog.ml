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

  class t ?ripple ~typ ~label () = object
    inherit Button.t ?ripple ~label () as super
    initializer
      super#add_class Markup.Dialog.Footer.button_class;
      super#add_class (match typ with
                       | `Accept  -> Markup.Dialog.Footer.accept_button_class
                       | `Decline -> Markup.Dialog.Footer.cancel_button_class)
  end

end

module Header = struct

  class t ~title () =
    let elt = Markup.Dialog.Header.create ~title () |> Tyxml_js.To_dom.of_header in
    object
      val h2_widget = elt##querySelector (Js.string @@ "." ^ Markup.Dialog.Header.title_class)
                      |> Js.Opt.to_option |> CCOpt.get_exn |> Widget.create
      inherit Widget.widget elt ()
      method get_title   = h2_widget#get_text_content |> CCOpt.get_or ~default:""
      method set_title s = h2_widget#set_text_content s
    end

end

module Body = struct

  class t ~(content:[ `String of string | `Widgets of #Widget.widget list ]) () =
    let content = (match content with
                   | `String  s -> [Tyxml_js.Html.pcdata s]
                   | `Widgets w -> Widget.widgets_to_markup w) in
    let elt = Markup.Dialog.Body.create ~content () |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.widget elt () as super
      method set_scrollable x = Markup.Dialog.Body.scrollable_class
                                |> (fun c -> if x then super#add_class c else super#remove_class c)
    end

end

module Footer = struct

  class t ~(actions:Action.t list) () =
    let elt = Markup.Dialog.Footer.create ~children:(Widget.widgets_to_markup actions) ()
              |> Tyxml_js.To_dom.of_footer in
    object
      val mutable actions = actions
      inherit Widget.widget elt ()
      method get_actions = actions
    end

end

class t ?title ?(actions:Action.t list option) ~content () =

  let header_widget = CCOpt.map (fun x -> new Header.t ~title:x ()) title in
  let body_widget   = new Body.t ~content () in
  let footer_widget = CCOpt.map (fun x -> new Footer.t ~actions:x ()) actions in

  let elt = Markup.Dialog.create
              ~content:([]
                        |> CCList.cons_maybe @@ CCOpt.map Widget.widget_to_markup footer_widget
                        |> CCList.cons @@ Widget.widget_to_markup body_widget
                        |> CCList.cons_maybe @@ CCOpt.map Widget.widget_to_markup header_widget)
              ()
            |> Tyxml_js.To_dom.of_aside in
  let e_action,e_action_push = React.E.create () in

  object

    inherit Widget.widget elt () as super

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.dialog##.MDCDialog##attachTo elt

    method get_header_widget = header_widget
    method get_body_widget   = body_widget
    method get_footer_widget = footer_widget

    method show       = mdc##show ()
    method show_await =
      let t,w = Lwt.wait () in
      mdc##show ();
      React.E.map (fun x -> Lwt.wakeup w x) @@ React.E.once e_action  |> ignore;
      t
    method hide      = mdc##close ()
    method is_opened = Js.to_bool mdc##.open_

    method e_action : [ `Accept | `Cancel ] React.event = e_action

    initializer
      Dom_events.listen super#root events.accept (fun _ _ -> e_action_push `Accept; false) |> ignore;
      Dom_events.listen super#root events.cancel (fun _ _ -> e_action_push `Cancel; false) |> ignore
  end
