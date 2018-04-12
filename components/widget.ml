open Containers

(* TODO remove *)
let (<=) = Pervasives.(<=)
let (>=) = Pervasives.(>=)
   
type rect =
  { top    : float
  ; right  : float
  ; bottom : float
  ; left   : float
  ; width  : float option
  ; height : float option
  }

let to_rect (x:Dom_html.clientRect Js.t) =
  { top    = x##.top
  ; right  = x##.right
  ; bottom = x##.bottom
  ; left   = x##.left
  ; width  = Js.Optdef.to_option x##.width
  ; height = Js.Optdef.to_option x##.height }

class widget (elt:#Dom_html.element Js.t) () = object(self)

  val mutable _on_load   = None
  val mutable _on_unload = None
  val mutable _in_dom    = false
  val mutable _observer  = None

  method root   : Dom_html.element Js.t = (elt :> Dom_html.element Js.t)
  method widget : widget = (self :> widget)

  method get_child_element_by_class x = Js.Opt.to_option @@ self#root##querySelector (Js.string ("." ^ x))
  method get_child_element_by_id    x = Js.Opt.to_option @@ self#root##querySelector (Js.string ("#" ^ x))

  method get_attribute a    = self#root##getAttribute (Js.string a) |> Js.Opt.to_option |> Option.map Js.to_string
  method set_attribute a v  = self#root##setAttribute (Js.string a) (Js.string v)
  method remove_attribute a = self#root##removeAttribute (Js.string a)
  method has_attribute a    = self#root##hasAttribute (Js.string a)

  method get_inner_html   = Js.to_string self#root##.innerHTML
  method set_inner_html s = self#root##.innerHTML := Js.string s

  method get_text_content   = self#root##.textContent |> Js.Opt.to_option |> Option.map Js.to_string
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
  method add_or_remove_class x _class = if x then self#add_class _class else self#remove_class _class

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

  method get_bounding_client_rect = (self#root##getBoundingClientRect)
                                    |> (fun x -> { top    = x##.top
                                                 ; right  = x##.right
                                                 ; bottom = x##.bottom
                                                 ; left   = x##.left
                                                 ; width  = Js.Optdef.to_option x##.width
                                                 ; height = Js.Optdef.to_option x##.height })

  method set_on_load (f : (unit -> unit) option) =
    _on_load <- f; self#_observe_if_needed
  method set_on_unload (f : (unit -> unit) option) =
    _on_unload <- f; self#_observe_if_needed

  method private _observe_if_needed =
    let init () = MutationObserver.observe
                    ~node:Dom_html.document
                    ~f:(fun _ _ ->
                      let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
                      if _in_dom && (not in_dom_new)
                      then CCOpt.iter (fun f -> f ()) _on_unload
                      else if (not _in_dom) && in_dom_new
                      then CCOpt.iter (fun f -> f ()) _on_load;
                      _in_dom <- in_dom_new)
                    ~child_list:true
                    ~subtree:true
                    ()
    in
    match _on_load, _on_unload, _observer with
    | None, None, Some o -> o##disconnect; _observer <- None
    | _, _, None         -> _observer <- Some (init ())
    | _                  -> ()

end

class button_widget elt () =
  let e_click,e_click_push = React.E.create () in
  object(self)

    inherit widget elt ()

    method e_click = e_click

    initializer
      Dom_events.listen self#root Dom_events.Typ.click (fun _ e -> e_click_push e; false) |> ignore

  end

class input_widget ~(input_elt:Dom_html.inputElement Js.t) elt () =
  let s_disabled,s_disabled_push = React.S.create false in
  object

    inherit widget elt ()

    method set_disabled x = input_elt##.disabled := Js.bool x; s_disabled_push x
    method get_disabled   = Js.to_bool input_elt##.disabled

    method set_input_id x = input_elt##.id := Js.string x
    method get_input_id   = match Js.to_string input_elt##.id with
      | "" -> None
      | s  -> Some s

    method private _set_value x = input_elt##.value := Js.string x
    method private _get_value   = Js.to_string input_elt##.value

    method s_disabled     = s_disabled

    method input_element  = input_elt

  end

class radio_or_cb_widget ~input_elt elt () =
  let s_state,s_state_push = React.S.create false in
  object(self)

    inherit input_widget ~input_elt elt ()

    method set_checked x = input_elt##.checked := Js.bool x; s_state_push x
    method checked       = Js.to_bool input_elt##.checked

    method s_state = s_state

    initializer
      Dom_events.listen input_elt Dom_events.Typ.change (fun _ _ -> s_state_push self#checked; false)
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
  }

type email_v_msgs   =
  { mismatch : string option
  ; too_long : string option
  }
type integer_v_msgs =
  { overflow  : string option
  ; underflow : string option
  ; step      : string option
  ; mismatch  : string option
  }
type float_v_msg = integer_v_msgs
type text_v_msgs =
  { too_long  : string option
  ; too_short : string option
  ; pattern   : string option
  }
type ipv4_v_msgs =
  { mismatch : string option
  }
type multicastv4_v_msgs = ipv4_v_msgs
type custom_v_msgs =
  { mismatch  : string option
  ; too_long  : string option
  ; too_short : string option
  }

type 'a validation =
  | Email       : string validation
  | Integer     : (int option * int option) -> int validation
  | Float       : (float option * float option) -> float validation
  | Text        : string validation
  | IPV4        : Ipaddr.V4.t validation
  | MulticastV4 : Ipaddr.V4.t validation
  | Password    : (string -> (unit, string) result) -> string validation
  | Custom      : ((string -> ('a, string) result) * ('a -> string)) -> 'a validation

let input_type_of_validation :
      type a. a validation -> [> `Email | `Number | `Text ]
  = function
  | Email       -> `Email
  | Integer _   -> `Number
  | Float   _   -> `Number
  | Text        -> `Text
  | IPV4        -> `Text
  | MulticastV4 -> `Text
  | Password _  -> `Password
  | Custom  _   -> `Text

let parse_valid (type a) (v : a validation) (on_fail : string -> unit) (s : string) : a option =
  match v with
  | Email        -> Some s
  | Integer integer -> (match integer with
                        | None, None -> CCInt.of_string s
                        | Some min, Some max ->
                           CCOpt.flat_map (fun i -> if i <= max && i >= min then Some i else None)
                             (CCInt.of_string s)
                        | Some min, None ->
                           CCOpt.flat_map (fun i -> if i >= min then Some i else None)
                             (CCInt.of_string s)
                        | None, Some max ->
                           CCOpt.flat_map (fun i -> if i <= max then Some i else None)
                             (CCInt.of_string s))
  | Float float -> (let num = float_of_string s in
                    match float with
                    | None, None -> Some num
                    | Some min, Some max -> if num <= max && num >= min then Some num else None
                    | Some min, None -> if num >= min then Some num else None
                    | None, Some max -> if num <= max then Some num else None)
  | Text         -> Some s
  | IPV4         -> Ipaddr.V4.of_string s
  | MulticastV4  -> Option.(Ipaddr.V4.of_string s >>= (fun x ->
                              if Ipaddr.V4.is_multicast x then Some x else None))
  | Password vf  ->
     (match vf s with
      | Ok () -> Some s
      | Error e -> on_fail e; None)
  | Custom (f,_) ->
     (match f s with
      | Ok v -> Some v
      | Error s -> on_fail s; None)

let valid_to_string (type a) (v : a validation) (e : a) : string =
  match v with
  | Custom (_,conv) -> conv e
  | Float   _       -> string_of_float e |> (fun x -> if String.suffix ~suf:"." x then x ^ "0" else x)
  | Integer _       -> string_of_int e
  | Email           -> e
  | IPV4            -> Ipaddr.V4.to_string e
  | MulticastV4     -> Ipaddr.V4.to_string e
  | Password _      -> e
  | Text            -> e

class ['a] text_input_widget ?v_msg ~input_elt (v : 'a validation) elt () =
  let (s_input : 'a option React.signal), s_input_push = React.S.create None in
  object(self)

    inherit input_widget ~input_elt elt ()

    val mutable v_msg = v_msg
    val mutable req   = false

    method get_v_msg : string option = v_msg
    method set_v_msg x = v_msg <- x

    method set_required x = req <- x; input_elt##.required := Js.bool x
    method get_required   = req

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

    method s_input   = s_input

    method fill_in (x : 'a) = s_input_push (Some x); self#_set_value (valid_to_string v x)
    method clear            = s_input_push None; self#_set_value ""

    method private set_max (x : float) = (Js.Unsafe.coerce input_elt)##.max := x
    method private set_min (x : float) = (Js.Unsafe.coerce input_elt)##.min := x

    method private set_max_length (x : int) = input_elt##.maxLength := x
    method private set_min_length (x : int) = (Js.Unsafe.coerce input_elt)##.minLength := x

    method private set_custom_validity s  = (Js.Unsafe.coerce input_elt)##setCustomValidity (Js.string s)
    method private remove_custom_validity = self#set_custom_validity ""

    initializer
      let apply_border (type a) (v : a validation) : unit =
        (match v with
         | Float (min,max)   -> Option.iter (fun min -> self#set_min min) min;
                                Option.iter (fun max -> self#set_max max) max
         | Integer (min,max) -> Option.iter (fun min -> self#set_min @@ float_of_int min) min;
                                Option.iter (fun max -> self#set_max @@ float_of_int max) max
         | _ -> ())
      in
      let apply_pattern (type a) (v : a validation) : unit =
        let set p = (Js.Unsafe.coerce input_elt)##.pattern := Js.string p in
        (match v with
         | IPV4        -> let p = "^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.)\
                                   {3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$" in
                          set p
         | MulticastV4 -> let p = "2(?:2[4-9]|3\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]\\d?|0)){3}" in
                          set p
         | _ -> ())
      in
      apply_border v;
      apply_pattern v;
      Dom_events.listen input_elt Dom_events.Typ.input (fun _ _ ->
          (match parse_valid v self#set_custom_validity self#_get_value with
           | Some v -> s_input_push (Some v); self#remove_custom_validity
           | None   -> s_input_push (None));
          false)
      |> ignore;
      Dom_events.listen
        input_elt
        (Dom_events.Typ.make "invalid")
        (fun _ _ -> (* let v = self#get_validity in
                     * let set_maybe s = Option.iter self#set_custom_validity s in *)
                    s_input_push None;
                    (* if v.bad_input             then set_maybe bad_input_msg
                     * else if v.custom_error     then set_maybe custom_error_msg
                     * else if v.pattern_mismatch then set_maybe pattern_mismatch_msg
                     * else if v.range_overflow   then set_maybe range_overflow_msg
                     * else if v.range_underflow  then set_maybe range_underflow_msg
                     * else if v.step_mismatch    then set_maybe step_mismatch_msg
                     * else if v.too_long         then set_maybe too_long_msg
                     * else if v.too_short        then set_maybe too_short_msg
                     * else if v.type_mismatch    then set_maybe type_mismatch_msg
                     * else if v.valid            then set_maybe invalid_msg
                     * else if v.value_missing    then set_maybe value_missing_msg; *)
                    false)
      |> ignore

  end

let create x = new widget x ()
let coerce (x : #widget) = (x :> widget)

let widget_to_markup (x : #widget) = Tyxml_js.Of_dom.of_element x#root
let widgets_to_markup (x : #widget list) = List.map widget_to_markup x
