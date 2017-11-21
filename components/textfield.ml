open Widget
open Markup
open Tyxml_js

module Pure = struct

  class t ?input_type ?input_id ?placeholder ?box () =

    let elt = (Textfield.create ?input_type ?input_id ?placeholder ?box () |> Tyxml_js.To_dom.of_div) in

    object

      inherit [Dom_html.divElement Js.t] text_input_widget elt ()

      val input : Dom_html.inputElement Js.t =
        elt##querySelector (Js.string ("." ^ Textfield.input_class))
        |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

      method private input = input

    end

end

class type mdc =
  object
    method disabled          : bool Js.t Js.prop
    method valid             : bool Js.t Js.writeonly_prop
    method helperTextElement : Dom_html.paragraphElement Js.t Js.prop
  end

type icon =
  { icon      : string
  ; clickable : bool
  ; pos       : [ `Trailing | `Leading ]
  }

type helptext =
  { persistent : bool
  ; validation : bool
  ; text       : string
  }

let icon_event : Dom_html.event Dom_events.Typ.typ = Dom_events.Typ.make "MDCTextfield:icon"

class t ?input_type ?input_id ?label ?placeholder ?icon ?help_text ?box () =

  let icon_widget =
    CCOpt.map (fun { clickable; icon; _ } -> new widget
                                                 (Textfield.Icon.create ~clickable ~icon () |> To_dom.of_i)
                                                 ())
              icon in

  let help_text_widget =
    CCOpt.map (fun { persistent; validation; text } ->
        new widget (Textfield.Help_text.create ~persistent ~validation ~text () |> To_dom.of_p) ())
              help_text in

  let text_field_widget =
    let get_icon pos = (match icon,icon_widget with
                        | (Some x,Some w) when x.pos = pos -> Some (Of_dom.of_element w#root)
                        | _ -> None) in
    Textfield.create ?input_type ?input_id ?label ?placeholder ?leading_icon:(get_icon `Leading)
                     ?trailing_icon:(get_icon `Trailing) ?box:(if CCOpt.is_some icon then Some true else box)  ()
    |> To_dom.of_element
    |> (fun x -> new widget x ()) in

  let elt =
    let tf = Of_dom.of_element text_field_widget#root in
    CCOpt.map_or ~default:tf (fun x -> Html.section [ tf; Of_dom.of_paragraph x#root ]) help_text_widget
    |> To_dom.of_element in

  object(self)

    inherit [Dom_html.element Js.t] text_input_widget elt ()

    val mdc : mdc Js.t =
      text_field_widget#root
      |> (fun x -> Js.Unsafe.global##.mdc##.textField##.MDCTextField##attachTo x)
      |> (fun x -> CCOpt.map_or ~default:x (fun w -> x##.helperTextElement := w#root; x) help_text_widget)

    val input : Dom_html.inputElement Js.t =
      elt##querySelector (Js.string ("." ^ Textfield.input_class))
      |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce

    method private input = input

    method text_field_widget = text_field_widget
    method icon_widget       = icon_widget

    method help_text_widget  = help_text_widget
    method set_help_text s   = CCOpt.iter (fun x -> x#set_text_content s) help_text_widget
    method get_help_text     = CCOpt.map (fun x -> x#text_content) help_text_widget

    method dense             = self#add_class Textfield.dense_class
    method full_width        = self#add_class Textfield.fullwidth_class
    method not_dense         = self#remove_class Textfield.dense_class
    method not_full_width    = self#remove_class Textfield.fullwidth_class

    method disable           = mdc##.disabled := Js._true
    method enable            = mdc##.disabled := Js._false
    method toggle_disabled   = mdc##.disabled := Js.bool @@ not self#disabled

    method valid   = mdc##.valid := Js._true
    method invalid = mdc##.valid := Js._false

  end
