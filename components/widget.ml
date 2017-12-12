type rect =
  { top    : float
  ; right  : float
  ; bottom : float
  ; left   : float
  ; width  : float option
  ; height : float option
  }

class widget elt () = object(self)

  method root : Dom_html.element Js.t = (elt :> Dom_html.element Js.t)

  method get_attribute a    = self#root##getAttribute (Js.string a) |> Js.Opt.to_option |> CCOpt.map Js.to_string
  method set_attribute a v  = self#root##setAttribute (Js.string a) (Js.string v)
  method remove_attribute a = self#root##removeAttribute (Js.string a)
  method has_attribute a    = self#root##hasAttribute (Js.string a)

  method get_inner_html   = Js.to_string self#root##.innerHTML
  method set_inner_html s = self#root##.innerHTML := Js.string s

  method get_text_content   = self#root##.textContent |> Js.Opt.to_option |> CCOpt.map Js.to_string
  method set_text_content s = self#root##.textContent := Js.some @@ Js.string s

  method get_id    = Js.to_string self#root##.id
  method set_id id = self#root##.id := Js.string id

  method style = self#root##.style

  method get_class_string = Js.to_string @@ self#root##.className
  method set_class_string _classes = self#root##.className := (Js.string _classes)
  method cons_class   _class = self#set_class_string @@ _class ^ " " ^ self#get_class_string
  method add_class    _class = self#root##.classList##add (Js.string _class)
  method remove_class _class = self#root##.classList##remove (Js.string _class)
  method toggle_class _class = self#root##.classList##toggle (Js.string _class) |> Js.to_bool
  method has_class    _class = Js.to_bool (self#root##.classList##contains (Js.string _class))
  method get_classes         = String.split_on_char ' ' self#get_class_string
  method private add_or_remove_class x _class = if x then self#add_class _class else self#remove_class _class

  method get_client_left   = self#root##.clientLeft
  method get_client_top    = self#root##.clientTop
  method get_client_width  = self#root##.clientWidth
  method get_client_height = self#root##.clientHeight

  method get_offset_left   = self#root##.offsetLeft
  method get_offset_top    = self#root##.offsetTop
  method get_offset_width  = self#root##.offsetWidth
  method get_offset_height = self#root##.offsetHeight

  method get_scroll_left   = self#root##.scrollLeft
  method get_scroll_top    = self#root##.scrollTop
  method get_scroll_width  = self#root##.scrollWidth
  method get_scroll_height = self#root##.scrollHeight

  method get_client_rect   = (self#root##getBoundingClientRect)
                             |> (fun x -> { top    = x##.top
                                          ; right  = x##.right
                                          ; bottom = x##.bottom
                                          ; left   = x##.left
                                          ; width  = Js.Optdef.to_option x##.width
                                          ; height = Js.Optdef.to_option x##.height })

end

class input_widget ~(input_elt:Dom_html.inputElement Js.t) elt () =
  let s_disabled,s_disabled_push = React.S.create false in
  object

    inherit widget elt ()

    method set_disabled x = input_elt##.disabled := Js.bool x; s_disabled_push x
    method get_disabled   = Js.to_bool input_elt##.disabled

    method set_value x    = input_elt##.value := Js.string x
    method get_value      = Js.to_string input_elt##.value

    method s_disabled     = s_disabled

    method input_element  = input_elt

    (* method disable_signal s = React.S.map (fun d -> if d then self#disable else self#enable) s |> ignore *)

  end

class radio_or_cb_widget ~input_elt elt () =
  let s_state,s_state_push = React.S.create false in
  object(self)

    inherit input_widget ~input_elt elt ()

    method set_checked x = input_elt##.checked := Js.bool x; s_state_push x
    method get_checked   = Js.to_bool input_elt##.checked

    method s_state = s_state

    initializer
      Dom_events.listen input_elt Dom_events.Typ.change (fun _ _ -> s_state_push self#get_checked; false)
      |> ignore;

  end

class type validity_state =
  object
    method badInput        : bool Js.t Js.readonly_prop
    method customError     : bool Js.t Js.readonly_prop
    method patternMismatch : bool Js.t Js.readonly_prop
    method rangeOverflow   : bool Js.t Js.readonly_prop
    method rangeUnderflow  : bool Js.t Js.readonly_prop
    method stepMismatch    : bool Js.t Js.readonly_prop
    method tooLong         : bool Js.t Js.readonly_prop
    method tooShort        : bool Js.t Js.readonly_prop
    method typeMismatch    : bool Js.t Js.readonly_prop
    method valid           : bool Js.t Js.readonly_prop
    method valueMissing    : bool Js.t Js.readonly_prop
  end

type validity =
  { bad_input        : bool
  ; custom_error     : bool
  ; pattern_mismatch : bool
  ; range_overflow   : bool
  ; range_underflow  : bool
  ; step_mismatch    : bool
  ; too_long         : bool
  ; too_short        : bool
  ; type_mismatch    : bool
  ; valid            : bool
  ; value_missing    : bool
  } [@@deriving to_yojson]

type 'a validation =
  | Email   : string validation
  | Integer : (int * int) option -> int validation
  | Float   : (float * float) option -> float validation
  | Text    : string validation
  | Custom  : (string    -> ('a, string) result) -> 'a validation

let input_type_of_validation :
      type a. a validation -> [> `Email | `Number | `Text ]
  = function
  | Email     -> `Email
  | Integer _ -> `Number
  | Float   _ -> `Number
  | Text      -> `Text
  | Custom  _ -> `Text

let parse_valid (type a) (v : a validation) (on_fail : string -> unit) (s : string) : a option =
  match v with
  | Email        -> Some s
  | Integer None -> Some (int_of_string s)
  | Integer Some (min,max) -> let i = int_of_string s in
                              if i <= max && i >= min then Some i
                              else None
  | Float None   -> Some (float_of_string s)
  | Float Some (min,max) -> let i = float_of_string s in
                            if i <= max && i >= min then Some i
                            else None
  | Text         -> Some s
  | Custom f     ->
     match f s with
     | Ok v -> Some v
     | Error s -> on_fail s; None
   
class ['a] text_input_widget ~input_elt (v : 'a validation) elt () =
  let (s_input : 'a option React.signal), s_input_push = React.S.create None in
  object(self)

    inherit input_widget ~input_elt elt ()

    method get_max : float     = (Js.Unsafe.coerce input_elt)##.max
    method set_max (x : float) = (Js.Unsafe.coerce input_elt)##.max := x

    method get_min : float     = (Js.Unsafe.coerce input_elt)##.min
    method set_min (x : float) = (Js.Unsafe.coerce input_elt)##.min := x

    method set_required x = input_elt##.required := Js.bool x

    method get_validation_message = Js.to_string (Js.Unsafe.coerce input_elt)##.validationMessage
    method get_validity           = let (v:validity_state Js.t) = (Js.Unsafe.coerce input_elt)##.validity in
                                    { bad_input        = Js.to_bool v##.badInput
                                    ; custom_error     = Js.to_bool v##.customError
                                    ; pattern_mismatch = Js.to_bool v##.patternMismatch
                                    ; range_overflow   = Js.to_bool v##.rangeOverflow
                                    ; range_underflow  = Js.to_bool v##.rangeUnderflow
                                    ; step_mismatch    = Js.to_bool v##.stepMismatch
                                    ; too_long         = Js.to_bool v##.tooLong
                                    ; too_short        = Js.to_bool v##.tooShort
                                    ; type_mismatch    = Js.to_bool v##.typeMismatch
                                    ; valid            = Js.to_bool v##.valid
                                    ; value_missing    = Js.to_bool v##.valueMissing
                                    }

    val mutable bad_input_msg        : string option = None
    val mutable custom_error_msg     : string option = None
    val mutable pattern_mismatch_msg : string option = None
    val mutable range_overflow_msg   : string option = None
    val mutable range_underflow_msg  : string option = None
    val mutable step_mismatch_msg    : string option = None
    val mutable too_long_msg         : string option = None
    val mutable too_short_msg        : string option = None
    val mutable type_mismatch_msg    : string option = None
    val mutable invalid_msg          : string option = None
    val mutable value_missing_msg    : string option = None

    method set_bad_input_message    s = bad_input_msg        <- Some s
    method set_custom_error_msg     s = custom_error_msg     <- Some s
    method set_pattern_mismatch_msg s = pattern_mismatch_msg <- Some s
    method set_range_overflow_msg   s = range_overflow_msg   <- Some s
    method set_range_underflow_msg  s = range_underflow_msg  <- Some s
    method set_step_mismatch_msg    s = step_mismatch_msg    <- Some s
    method set_too_long_msg         s = too_long_msg         <- Some s
    method set_too_short_msg        s = too_short_msg        <- Some s
    method set_type_mismatch_msg    s = type_mismatch_msg    <- Some s
    method set_invalid_msg          s = invalid_msg          <- Some s
    method set_value_missing_msg    s = value_missing_msg    <- Some s

    method has_bad_input        = self#get_validity.bad_input
    method has_custom_error     = self#get_validity.custom_error
    method has_pattern_mismatch = self#get_validity.pattern_mismatch
    method has_range_overflow   = self#get_validity.range_overflow
    method has_range_underflow  = self#get_validity.range_underflow
    method has_step_mismatch    = self#get_validity.step_mismatch
    method is_too_long          = self#get_validity.too_long
    method is_too_short         = self#get_validity.too_short
    method has_type_mismatch    = self#get_validity.type_mismatch
    method is_valid             = self#get_validity.valid
    method is_value_missing     = self#get_validity.value_missing

    method s_input   = s_input

    method private set_custom_validity s  = (Js.Unsafe.coerce input_elt)##setCustomValidity (Js.string s)
    method private remove_custom_validity = self#set_custom_validity ""

    initializer
      Dom_events.listen input_elt Dom_events.Typ.input (fun _ _ ->
          (match parse_valid v self#set_custom_validity self#get_value with
           | Some v -> s_input_push (Some v); self#remove_custom_validity
           | None   -> s_input_push (None));
          false)
      |> ignore;
      Dom_events.listen
        input_elt
        (Dom_events.Typ.make "invalid")
        (fun _ _ -> let v = self#get_validity in
                    let set_maybe s = CCOpt.iter self#set_custom_validity s in
                    s_input_push None;
                    if v.bad_input             then set_maybe bad_input_msg
                    else if v.custom_error     then set_maybe custom_error_msg
                    else if v.pattern_mismatch then set_maybe pattern_mismatch_msg
                    else if v.range_overflow   then set_maybe range_overflow_msg
                    else if v.range_underflow  then set_maybe range_underflow_msg
                    else if v.step_mismatch    then set_maybe step_mismatch_msg
                    else if v.too_long         then set_maybe too_long_msg
                    else if v.too_short        then set_maybe too_short_msg
                    else if v.type_mismatch    then set_maybe type_mismatch_msg
                    else if v.valid            then set_maybe invalid_msg
                    else if v.value_missing    then set_maybe value_missing_msg; false)
      |> ignore

  end

let create x = new widget x ()
let coerce (x : #widget) = (x :> widget)

let widget_to_markup (x : #widget) = Tyxml_js.Of_dom.of_element x#root
let widgets_to_markup (x : #widget list) = List.map widget_to_markup x
