open Containers
open Tyxml_js

module Markup = Components_markup.Textfield.Make(Xml)(Svg)(Html)

module Pure = struct

  class ['a] t ?(input_type = Widget.Text) ~input_id ?placeholder ?box () =
    let elt = (Markup.create
                 ~input_type:(Widget.input_type_of_validation input_type)
                 ~input_id
                 ?placeholder
                 ?box ()
               |> Tyxml_js.To_dom.of_div) in
    let input_elt = elt##querySelector (Js.string ("." ^ Markup.input_class))
                    |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in
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

let icon_event : Dom_html.event Js.t Dom_events.Typ.typ = Dom_events.Typ.make "MDCTextfield:icon"

module Help_text = struct

  type helptext =
    { persistent : bool
    ; validation : bool
    ; text       : string option
    }

  class t {persistent;validation;text} () =
    let elt = Markup.Help_text.create ~persistent ~validation ?text ()
              |> Tyxml_js.To_dom.of_p in
    object
      inherit Widget.t elt () as super
      method is_validation    = super#has_class Markup.Help_text.validation_msg_class
      method is_persistent    = super#has_class Markup.Help_text.persistent_class
      method set_validation x = super#add_or_remove_class x Markup.Help_text.validation_msg_class
      method set_persistent x = super#add_or_remove_class x Markup.Help_text.persistent_class
      method text             = text
    end

end

class ['a] t ~input_type ~input_id ?label ?placeholder ?icon ?help_text ?box ?outline () =

  let floating_label = match label with
    | None   -> None
    | Some x -> Some (Markup.Floating_label.create ~data:x ~fore:input_id ())
  in
  let icon_widget =
    Option.map (fun { clickable; icon; _ } ->
        new Widget.t
          (Markup.Icon.create ~clickable ~icon () |> Tyxml_js.To_dom.of_i)
          ())
      icon in
  let help_text_widget  = Option.map (fun x -> new Help_text.t x ()) help_text in
  let text_field_widget =
    let get_icon pos = (match icon,icon_widget with
                        | (Some x,Some w) when Equal.poly x.pos pos -> Some (Widget.to_markup w)
                        | _ -> None) in
    Markup.create ~input_type:(Widget.input_type_of_validation input_type)
      ~input_id
      ?label:floating_label
      ?placeholder
      ?leading_icon:(get_icon `Leading)
      ?trailing_icon:(get_icon `Trailing)
      ?box:(if (Option.is_some icon) && not (Option.is_some outline)
            then Some true
            else box)
      ?outline
      ()
    |> Tyxml_js.To_dom.of_element
    |> (fun x -> new Widget.t x ()) in
  let elt =
    let tf = Widget.to_markup text_field_widget in
    Option.map_or
      ~default:tf
      (fun x -> Markup.Wrapper.create ~textfield:tf
                  ~helptext:(Widget.to_markup x) ())
      help_text_widget
    |> Tyxml_js.To_dom.of_element in
  let input_elt =
    elt##querySelector (Js.string ("." ^ Markup.input_class))
    |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in

  object(self)

    inherit ['a] Widget.text_input_widget ~input_elt input_type elt () as super

    val mdc : mdc Js.t =
      text_field_widget#root
      |> (fun x ->
        Js.Unsafe.global##.mdc##.textField##.MDCTextField##attachTo x)
      |> (fun x ->
        Option.map_or ~default:x
          (fun w -> x##.helperTextElement := w#root; x)
          help_text_widget)

    val label_widget =
      elt##querySelector (Js.string @@ "." ^ Markup.label_class)
      |> Js.Opt.to_option
      |> Option.map (fun x -> new Widget.t x ())

    method text_field_widget = text_field_widget
    method icon_widget       = icon_widget
    method help_text_widget  = help_text_widget
    method label_widget      = label_widget

    method help_text       =
      Option.(map (fun x -> x#text_content
                            |> get_or ~default:"") self#help_text_widget)
    method set_help_text s =
      Option.iter (fun x -> x#set_text_content s) self#help_text_widget

    method set_dense x       =
      self#text_field_widget#add_or_remove_class x Markup.dense_class
    method set_full_width x  =
      self#add_or_remove_class x Markup.fullwidth_class

    method set_valid x =
      mdc##.valid := Js.bool x

    method label       =
      Option.(map (fun x -> x#text_content
                            |> get_or ~default:"") self#label_widget)
    method set_label s =
      Option.map (fun x -> x#set_text_content s) self#label_widget

    method! disabled       =
      Js.to_bool mdc##.disabled
    method! set_disabled x =
      mdc##.disabled := Js.bool x

    method! fill_in (x:'a) =
      super#fill_in x;
      self#text_field_widget#add_class Markup.upgraded_class;
      Option.iter (fun x -> x#add_class Markup.label_float_above_class)
        self#label_widget
    method! clear () =
      super#clear ();
      self#text_field_widget#remove_class Markup.upgraded_class;
      Option.iter (fun x -> x#remove_class Markup.label_float_above_class)
        self#label_widget

    initializer
      Option.iter (fun x ->
          if x#is_validation && Option.is_none x#text
          then React.S.map (function
                   | Some _ -> ()
                   | None   -> x#set_text_content super#validation_message)
                 super#s_input |> ignore)
        self#help_text_widget

  end
