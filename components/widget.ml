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
  object

    inherit widget elt ()

    method set_disabled x = input_elt##.disabled := Js.bool x
    method get_disabled   = Js.to_bool input_elt##.disabled

    method set_value x    = input_elt##.value := Js.string x
    method get_value      = Js.to_string input_elt##.value

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

class text_input_widget ~input_elt elt () =
  object

    inherit input_widget ~input_elt elt ()

    method get_max : float     = (Js.Unsafe.coerce input_elt)##.max
    method set_max (x : float) = (Js.Unsafe.coerce input_elt)##.max := x

    method get_min : float     = (Js.Unsafe.coerce input_elt)##.min
    method set_min (x : float) = (Js.Unsafe.coerce input_elt)##.min := x

    method get_pattern   = Js.to_string (Js.Unsafe.coerce input_elt)##.pattern
    method set_pattern s = (Js.Unsafe.coerce input_elt)##.pattern := Js.string s

    method set_required x = input_elt##.required := Js.bool x

  end

let create x = new widget x ()
let coerce (x : #widget) = (x :> widget)

let widget_to_markup (x : #widget) = Tyxml_js.Of_dom.of_element x#root
let widgets_to_markup (x : #widget list) = List.map widget_to_markup x
