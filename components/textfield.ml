module Pure = struct

  class ['a] t ?(input_type = Widget.Text) ?input_id ?placeholder ?box () =
    let elt = (Markup.Textfield.create
                 ~input_type:(Widget.input_type_of_validation input_type)
                 ?input_id
                 ?placeholder
                 ?box ()
               |> Tyxml_js.To_dom.of_div) in
    let input_elt = elt##querySelector (Js.string ("." ^ Markup.Textfield.input_class))
                    |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce in
    object
      inherit ['a] Widget.text_input_widget ~input_elt input_type elt ()
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

let icon_event : Dom_html.event Dom_events.Typ.typ = Dom_events.Typ.make "MDCTextfield:icon"

module Help_text = struct

  type helptext =
    { persistent : bool
    ; validation : bool
    ; text       : string option
    }

  class t {persistent;validation;text} () =
    let elt = Markup.Textfield.Help_text.create ~persistent ~validation ?text ()
              |> Tyxml_js.To_dom.of_p in
    object
      inherit Widget.widget elt () as super
      method is_validation    = super#has_class Markup.Textfield.Help_text.validation_msg_class
      method is_persistent    = super#has_class Markup.Textfield.Help_text.persistent_class
      method set_validation x = super#add_or_remove_class x Markup.Textfield.Help_text.validation_msg_class
      method set_persistent x = super#add_or_remove_class x Markup.Textfield.Help_text.persistent_class
      method text             = text
    end

end

class ['a] t ~input_type ?input_id ?label ?placeholder ?icon ?help_text ?box () =

  let icon_widget =
    CCOpt.map (fun { clickable; icon; _ } ->
        new Widget.widget
            (Markup.Textfield.Icon.create ~clickable ~icon () |> Tyxml_js.To_dom.of_i)
            ())
              icon in
  let help_text_widget  = CCOpt.map (fun x -> new Help_text.t x ()) help_text in
  let text_field_widget =
    let get_icon pos = (match icon,icon_widget with
                        | (Some x,Some w) when x.pos = pos -> Some (Widget.widget_to_markup w)
                        | _ -> None) in
    Markup.Textfield.create ~input_type:(Widget.input_type_of_validation input_type)
                            ?input_id
                            ?label
                            ?placeholder
                            ?leading_icon:(get_icon `Leading)
                            ?trailing_icon:(get_icon `Trailing)
                            ?box:(if CCOpt.is_some icon then Some true else box)
                            ()
    |> Tyxml_js.To_dom.of_element
    |> (fun x -> new Widget.widget x ()) in
  let elt =
    let tf = Widget.widget_to_markup text_field_widget in
    CCOpt.map_or ~default:tf
                 (fun x -> Markup.Textfield.Wrapper.create ~textfield:tf ~helptext:(Widget.widget_to_markup x) ())
                 help_text_widget
    |> Tyxml_js.To_dom.of_element in
  let input_elt = elt##querySelector (Js.string ("." ^ Markup.Textfield.input_class))
                  |> Js.Opt.to_option |> CCOpt.get_exn |> Js.Unsafe.coerce in

  object(self)

    inherit ['a] Widget.text_input_widget ~input_elt input_type elt () as super

    val mdc : mdc Js.t =
      text_field_widget#root
      |> (fun x -> Js.Unsafe.global##.mdc##.textField##.MDCTextField##attachTo x)
      |> (fun x -> CCOpt.map_or ~default:x (fun w -> x##.helperTextElement := w#root; x) help_text_widget)

    val label_widget = elt##querySelector (Js.string @@ "." ^ Markup.Textfield.label_class)
                       |> Js.Opt.to_option
                       |> CCOpt.map (fun x -> new Widget.widget x ())

    method get_text_field_widget = text_field_widget
    method get_icon_widget       = icon_widget
    method get_help_text_widget  = help_text_widget
    method get_label_widget      = label_widget

    method set_help_text s   = CCOpt.iter (fun x -> x#set_text_content s) help_text_widget
    method get_help_text     = CCOpt.map  (fun x -> x#get_text_content |> CCOpt.get_or ~default:"")
                                          self#get_help_text_widget

    method private add_or_rm_class x c = if x then self#get_text_field_widget#add_class c
                                         else self#get_text_field_widget#remove_class c
    method set_dense x       = self#add_or_rm_class x Markup.Textfield.dense_class
    method set_full_width x  = self#add_or_rm_class x Markup.Textfield.fullwidth_class

    method set_valid x = mdc##.valid := Js.bool x

    method get_label   = CCOpt.map (fun x -> x#get_text_content |> CCOpt.get_or ~default:"") self#get_label_widget
    method set_label s = CCOpt.map (fun x -> x#set_text_content s) self#get_label_widget

    method! get_disabled   = Js.to_bool mdc##.disabled
    method! set_disabled x = mdc##.disabled := Js.bool x

    method set_value x = super#set_value x;
                         self#get_text_field_widget#add_class Markup.Textfield.upgraded_class;
                         CCOpt.iter (fun x -> x#add_class Markup.Textfield.label_float_above_class)
                           self#get_label_widget
    method fill_in (x : 'a) = self#set_value (Widget.valid_to_string input_type x)
      

    initializer
      CCOpt.iter (fun x -> if x#is_validation && CCOpt.is_none x#text
                           then React.S.map (function Some _ -> ()
                                                    | None   -> x#set_text_content super#get_validation_message)
                                  super#s_input |> ignore)
        self#get_help_text_widget

  end
