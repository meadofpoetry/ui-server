open Js_of_ocaml
open Js_of_ocaml_tyxml

include module type of Components_tyxml.Textfield
module Markup : sig
  include module type of Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)
end

class type validity_state =
  object
    method badInput : bool Js.t Js.readonly_prop
    method customError : bool Js.t Js.readonly_prop
    method patternMismatch : bool Js.t Js.readonly_prop
    method rangeOverflow : bool Js.t Js.readonly_prop
    method rangeUnderflow : bool Js.t Js.readonly_prop
    method stepMismatch : bool Js.t Js.readonly_prop
    method tooLong : bool Js.t Js.readonly_prop
    method tooShort : bool Js.t Js.readonly_prop
    method typeMismatch : bool Js.t Js.readonly_prop
    method valid : bool Js.t Js.readonly_prop
    method valueMissing : bool Js.t Js.readonly_prop
  end

module Event : sig
  class type icon = [unit] Widget.custom_event

  module Typ : sig val icon : icon Js.t Dom.Event.typ end

  val icon :
       ?use_capture:bool
    -> ?passive:bool
    -> #Dom_html.eventTarget Js.t
    -> icon Js.t Lwt.t

  val icons :
       ?cancel_handler:bool
    -> ?use_capture:bool
    -> ?passive:bool
    -> #Dom_html.eventTarget Js.t
    -> (icon Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t
end

module Character_counter : sig
  class type t =
    object
      inherit Widget.t

      method set_value : max_length:int -> int -> unit
    end

  val make : ?current_length:int -> ?max_length:int -> unit -> t

  val attach : #Dom_html.element Js.t -> t
end

module Icon : sig
  module Attr : sig
    val icon_role : string
    val aria_label : string
  end
  class type t =
    object
      inherit Widget.t

      (** Updates the icon's aria-label. *)
      method set_aria_label : string -> unit

      (** Updates the icon's disabled state. *)
      method set_disabled : bool -> unit

      (** Private methods *)

      (** Emits a custom event "textfield:icon" denoting a user has clicked
          the icon, which bubbles to the top-level text field element. *)
      method private notify_action : unit -> unit

      method private handle_keydown : Dom_html.keyboardEvent Js.t
        -> unit Lwt.t
        -> unit Lwt.t

      method private handle_click : Dom_html.mouseEvent Js.t
        -> unit Lwt.t
        -> unit Lwt.t
    end

  val attach : #Dom_html.element Js.t -> t
end

module Helper_text : sig
  class type t =
    object
      inherit Widget.t

      (** Sets the content of the helper text field. *)
      method set_content : string -> unit

      method persistent : bool

      (** Sets the persistency of the helper text. *)
      method set_persistent : bool -> unit

      method validation : bool

      (** [true] makes the helper text act as an error validation message. *)
      method set_validation : bool -> unit

      (** Makes the helper text visible to the screen reader. *)
      method show_to_screen_reader : unit -> unit

      (** Sets the validity of the helper text based on the input validity. *)
      method set_validity : bool -> unit

      (** Private methods *)

      (** Hides the help text from screen readers. *)
      method private hide : unit -> unit
    end

  val make : ?persistent:bool -> ?validation:bool -> string -> t

  val attach : #Dom_html.element Js.t -> t
end

type 'a validation =
  | Email : string validation
  | Integer : (int option * int option) -> int validation
  | Float : (float option * float option) -> float validation
  | Text : string validation
  | Password : (string -> (unit, string) result) -> string validation
  | Custom : 'a custom_validation -> 'a validation
and 'a custom_validation =
  { input_type : Html_types.input_type
  ; of_string : string -> ('a, string) result
  ; to_string : 'a -> string
  }

class type ['a] t =
  object
    inherit Widget.t

    method input_element : Dom_html.inputElement Js.t

    method leading_icon : Icon.t option

    method trailing_icon : Icon.t option

    method disabled : bool

    (** Sets the Text Field disabled or enabled. *)
    method set_disabled : bool -> unit

    (** Sets the aria label of the leading icon. *)
    method set_leading_icon_aria_label : string -> unit

    (** Sets the aria label of the trailing icon. *)
    method set_trailing_icon_aria_label : string -> unit

    (** Sets the helper text element content. *)
    method set_helper_text_content : string -> unit

    (** Focuses the input element. *)
    method focus : unit -> unit

    method ripple : Ripple.t option

    (** Validation API. *)

    method required : bool

    (** Sets the Text Field to required. *)
    method set_required : bool -> unit

    method use_native_validation : bool

    (** Enables or disables the use of native validation.
        Set to [false] to ignore native input validation. *)
    method set_use_native_validation : bool -> unit

    method force_custom_validation : unit -> unit

    method check_validity : unit -> bool

    method validity : validity_state Js.t

    (** The custom validity state, if set;
        otherwise, the result of a native validity check. *)
    method valid : bool

    (** Sets the custom validity state of the Text Field. *)
    method set_valid : bool -> unit

    method pattern : string

    (** Sets the input element's validation pattern. *)
    method set_pattern : string -> unit

    method min_length : int option

    (** Sets the input element's minLength. If the provided value is [None],
        this property is removed. *)
    method set_min_length : int option -> unit

    method max_length : int option

    (** Sets the input element's maxLength. If the provided value is [None],
        a user can input unlimited number of characters. *)
    method set_max_length : int option -> unit

    method validation_message : string

    (** Value API. *)

    (** Returns [true] if the Text Field is empty. *)
    method is_empty : bool

    (** Removes content of Text Field. *)
    method clear : unit -> unit

    (** Returns Text Field value as string. *)
    method value_as_string : string

    method set_value_as_string : string -> unit

    (** Returns type-safe Text Field value. *)
    method value : 'a option

    method set_value : 'a -> unit

    (** Private methods *)

    method private should_always_float : bool

    method private should_float : bool

    method private should_shake : bool

    method private focused : bool

    (** Sets the line ripple's transform origin, so that the line ripple activate
        animation will animate out from the user's click location. *)
    method private set_transform_origin :
      'a. (#Dom_html.event as 'a) Js.t
      -> unit Lwt.t
      -> unit Lwt.t

    (** Activates the Text Field's focus state in cases when the input value
        changes without user input (e.g. programmatically). *)
    method private auto_complete_focus : unit -> unit

    (** Handles input change of text input and text area. *)
    method private handle_input : Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t

    (** Handles user interactions with the Text Field. *)
    method private handle_text_field_interaction :
      'a. (#Dom_html.event as 'a) Js.t
      -> unit Lwt.t
      -> unit Lwt.t

    (** Handles validation attribute changes. *)
    method private handle_validation_attribute_change : string list -> unit

    (** Opens/closes the notched outline. *)
    method private notch_outline : bool -> unit

    (** Activates the text field focus state. *)
    method private activate_focus : unit -> unit

    (** Deactivates the Text Field's focus state. *)
    method private deactivate_focus : unit -> unit

    (** Sets character counter values that shows characters used and
        the total character limit.
        Raise [Failure] if there is no maxLength html attribute provided. *)
    method private set_character_counter : int -> unit

    (** [true] if the Text Field input fails in converting the user-supplied value. *)
    method private is_bad_input : unit -> bool

    (** The result of native validity checking (ValidityState.valid). *)
    method private is_native_input_valid : unit -> bool

    (** Styles the component based on the validity state. *)
    method private style_validity : bool -> unit

    (** Styles the component based on the focused state. *)
    method private style_focused : bool -> unit

    (** Styles the component based on the disabled state. *)
    method private style_disabled : bool -> unit

    method private set_max_as_number : float -> unit

    method private set_min_as_number : float -> unit

    method private apply_validation_constraints : 'a validation -> unit

    method private create_ripple : unit -> Ripple.t
  end

val make_textfield :
  ?validate_on_blur:bool
  -> ?on_input:(Dom_html.event Js.t -> 'a t -> unit Lwt.t)
  -> ?disabled:bool
  -> ?fullwidth:bool
  -> ?outlined:bool
  -> ?focused:bool
  -> ?input_id:string
  -> ?pattern:string
  -> ?min_length:int
  -> ?max_length:int
  -> ?step:float (* FIXME should be of float/date type. *)
  -> ?input_mode:[< `Email
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
  -> ?value:'a
  -> ?placeholder:string
  -> ?required:bool
  -> ?helper_text:Helper_text.t
  -> ?character_counter:Character_counter.t
  -> ?leading_icon:#Widget.t
  -> ?trailing_icon:#Widget.t
  -> ?label:string
  -> ?use_native_validation:bool
  -> 'a validation -> 'a t

val make_textarea :
  ?on_input:(Dom_html.event Js.t -> string t -> unit Lwt.t)
  -> ?disabled:bool
  -> ?fullwidth:bool
  -> ?focused:bool
  -> ?input_id:string
  -> ?min_length:int
  -> ?max_length:int
  -> ?rows:int
  -> ?cols:int
  -> ?value:string
  -> ?placeholder:string
  -> ?required:bool
  -> ?helper_text:Helper_text.t
  -> ?character_counter:Character_counter.t
  -> ?label:string
  -> unit
  -> string t

val attach :
  ?validate_on_blur:bool
  -> ?on_input:(Dom_html.event Js.t -> 'a t -> unit Lwt.t)
  -> ?helper_text:Helper_text.t
  -> ?character_counter:Character_counter.t
  -> ?use_native_validation:bool
  -> ?validation:'a validation
  -> #Dom_html.element Js.t -> 'a t
