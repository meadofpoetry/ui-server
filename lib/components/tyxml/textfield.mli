module CSS : sig
  val root : string
  (** Mandatory. *)

  val outlined : string
  (** Styles the text field as an outlined text field. *)

  val fullwidth : string
  (** Styles the text field as a full width text field. *)

  val textarea : string
  (** Indicates the text field is a <textarea>. *)

  val disabled : string
  (** Styles the text field as a disabled text field. *)

  val invalid : string

  val with_leading_icon : string
  (** Styles the text field as a text field with a leading icon. *)

  val with_trailing_icon : string
  (** Styles the text field as a text field with a trailing icon. *)

  val focused : string
  (** Styles the text field as a text field in focus. *)

  val no_label : string
  (** Styles the text field that has no label. *)

  val helper_line : string
  (** Styles the container of helper text and character counter elements. *)

  val icon : string

  val input : string

  module Helper_text : sig
    val root : string
    (** Mandatory. *)

    val persistent : string
    (** Makes the helper text permanently visible. *)

    val validation_msg : string
    (** Indicates the helper text is a validation message. *)
  end

  module Character_counter : sig
    val root : string
    (** Mandatory. *)
  end
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml with module Svg := Svg) : sig
  open Html

  module Helper_text : sig
    val create :
         ?classes:string list
      -> ?attrs:Html_types.div_attrib attrib list
      -> ?persistent:bool
      -> ?validation:bool
      -> ?text:string
      -> unit
      -> Html_types.div elt
  end

  module Character_counter : sig
    val create :
         ?classes:string list
      -> ?attrs:Html_types.div_attrib attrib list
      -> ?current_length:int
      -> ?max_length:int
      -> unit
      -> Html_types.div elt
  end

  module Textarea : sig
    val create_textarea :
         ?classes:string list
      -> ?attrs:Html_types.textarea_attrib attrib list
      -> ?id:string
      -> ?value:string
      -> ?placeholder:string
      -> ?required:bool
      -> ?min_length:int
      -> ?max_length:int
      -> ?rows:int
      -> ?cols:int
      -> ?disabled:bool
      -> unit
      -> Html_types.textarea elt

    val create :
         ?classes:string list
      -> ?attrs:Html_types.div_attrib attrib list
      -> ?disabled:bool
      -> ?no_label:bool
      -> ?fullwidth:bool
      -> ?focused:bool
      -> ?character_counter:([< Html_types.div_content_fun] as 'a) Html.elt
      -> ?outline:'a elt
      -> input:'a elt
      -> unit
      -> Html_types.div elt
  end

  val create_input :
       ?classes:string list
    -> ?attrs:Html_types.input_attrib attrib list
    -> ?id:string
    -> ?pattern:string
    -> ?min_length:int
    -> ?max_length:int
    -> ?step:float
    -> ?value:string
    -> ?placeholder:string
    -> ?required:bool
    -> ?disabled:bool
    -> ?typ:Html_types.input_type
    -> ?input_mode:
         [< `Email
         | `Full_width_latin
         | `Kana
         | `Katakana
         | `Latin
         | `Latin_name
         | `Latin_prose
         | `Numeric
         | `Tel
         | `Url
         | `Verbatim ]
    -> unit
    -> Html_types.input elt

  val create_helper_line :
       ?classes:string list
    -> ?attrs:Html_types.div_attrib attrib list
    -> [< Html_types.div_content_fun] Html.elt list
    -> [> Html_types.div] Html.elt

  val create :
       ?classes:string list_wrap
    -> ?attrs:Html_types.div_attrib attrib list
    -> ?disabled:bool
    -> ?leading_icon:([< Html_types.div_content_fun] as 'a) elt
    -> ?trailing_icon:'a elt
    -> ?no_label:bool
    -> ?fullwidth:bool
    -> ?textarea:bool
    -> ?focused:bool
    -> ?line_ripple:'a elt
    -> ?label:'a elt
    -> ?outline:'a elt
    -> input:'a elt
    -> unit
    -> Html_types.div elt
end
