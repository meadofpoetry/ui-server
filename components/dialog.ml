open Containers
open Tyxml_js

module Markup = Components_markup.Dialog.Make(Xml)(Svg)(Html)

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
      super#add_class Markup.Footer.button_class;
      super#add_class (match typ with
                       | `Accept  -> Markup.Footer.accept_button_class
                       | `Decline -> Markup.Footer.cancel_button_class)
  end

end

module Header = struct

  class t ~title () =
    let elt = Markup.Header.create ~title () |> Tyxml_js.To_dom.of_header in
    object
      val h2_widget = elt##querySelector (Js.string @@ "." ^ Markup.Header.title_class)
                      |> Js.Opt.to_option |> Option.get_exn |> Widget.create
      inherit Widget.t elt ()
      method title       = h2_widget#text_content |> Option.get_or ~default:""
      method set_title s = h2_widget#set_text_content s
    end

end

module Body = struct

  class t ?scrollable ~(content:[ `String of string | `Widgets of #Widget.t list ]) () =
    let content = (match content with
                   | `String  s -> [Tyxml_js.Html.pcdata s]
                   | `Widgets w -> List.map Widget.to_markup w) in
    let elt = Markup.Body.create ?scrollable ~content () |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.t elt () as super
      method set_scrollable x = Markup.Body.scrollable_class
                                |> (fun c -> if x then super#add_class c else super#remove_class c)
    end

end

module Footer = struct

  class t ~(actions:Action.t list) () =
    let elt = Markup.Footer.create ~children:(List.map Widget.to_markup actions) ()
              |> Tyxml_js.To_dom.of_footer in
    object
      val mutable actions = actions
      inherit Widget.t elt ()
      method actions = actions
    end

end

class t ?scrollable ?title ?(actions:Action.t list option) ~content () =

  let header_widget = Option.map (fun x -> new Header.t ~title:x ()) title in
  let body_widget   = new Body.t ?scrollable ~content () in
  let footer_widget = Option.map (fun x -> new Footer.t ~actions:x ()) actions in

  let elt = Markup.create
              ~content:([]
                        |> List.cons_maybe @@ Option.map Widget.to_markup footer_widget
                        |> List.cons @@ Widget.to_markup body_widget
                        |> List.cons_maybe @@ Option.map Widget.to_markup header_widget)
              ()
            |> Tyxml_js.To_dom.of_aside in
  let e_action,e_action_push = React.E.create () in

  object

    inherit Widget.t elt () as super

    val mdc : mdc Js.t = Js.Unsafe.global##.mdc##.dialog##.MDCDialog##attachTo elt

    method header = header_widget
    method body   = body_widget
    method footer = footer_widget

    method show ()       = mdc##show ()
    method show_await () =
      let t,w = Lwt.wait () in
      mdc##show ();
      React.E.map (fun x -> Lwt.wakeup w x) @@ React.E.once e_action  |> ignore;
      t
    method hide () = mdc##close ()
    method opened  = Js.to_bool mdc##.open_

    method e_action : [ `Accept | `Cancel ] React.event = e_action

    initializer
      Dom_events.listen super#root events.accept (fun _ _ -> e_action_push `Accept; false) |> ignore;
      Dom_events.listen super#root events.cancel (fun _ _ -> e_action_push `Cancel; false) |> ignore
  end
