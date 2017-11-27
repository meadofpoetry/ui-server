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

  method get_attribute a    = self#root##getAttribute (Js.string a)
                              |> Js.Opt.to_option
                              |> CCOpt.map Js.to_string
  method set_attribute a v  = self#root##setAttribute (Js.string a) (Js.string v)
  method remove_attribute a = self#root##removeAttribute (Js.string a)
  method has_attribute a    = self#root##hasAttribute (Js.string a)

  method inner_html       = Js.to_string self#root##.innerHTML
  method set_inner_html s = self#root##.innerHTML := Js.string s

  method text_content = self#root##.textContent
                        |> Js.Opt.to_option
                        |> CCOpt.map Js.to_string
  method set_text_content s = self#root##.textContent := Js.some @@ Js.string s

  method id = Js.to_string self#root##.id
  method set_id id = self#root##.id := Js.string id

  method style = self#root##.style

  method class_string = Js.to_string @@ self#root##.className
  method set_class_string _classes = self#root##.className := (Js.string _classes)

  method cons_class   _class = self#set_class_string @@ _class ^ " " ^ self#class_string
  method add_class    _class = self#root##.classList##add (Js.string _class)
  method remove_class _class = self#root##.classList##remove (Js.string _class)
  method toggle_class _class = self#root##.classList##toggle (Js.string _class) |> Js.to_bool
  method has_class    _class = Js.to_bool (self#root##.classList##contains (Js.string _class))
  method classes             = String.split_on_char ' ' self#class_string

  method client_left   = self#root##.clientLeft
  method client_top    = self#root##.clientTop
  method client_width  = self#root##.clientWidth
  method client_height = self#root##.clientHeight

  method offset_left   = self#root##.offsetLeft
  method offset_top    = self#root##.offsetTop
  method offset_width  = self#root##.offsetWidth
  method offset_height = self#root##.offsetHeight

  method scroll_left   = self#root##.scrollLeft
  method scroll_top    = self#root##.scrollTop
  method scroll_width  = self#root##.scrollWidth
  method scroll_height = self#root##.scrollHeight

  method client_rect   = (self#root##getBoundingClientRect)
                         |> (fun x -> { top    = x##.top
                                      ; right  = x##.right
                                      ; bottom = x##.bottom
                                      ; left   = x##.left
                                      ; width  = Js.Optdef.to_option x##.width
                                      ; height = Js.Optdef.to_option x##.height })

end

class virtual input_widget elt () =
        object(self)

          inherit widget elt ()

          method virtual input_element : Dom_html.inputElement Js.t

          method disabled        = Js.to_bool self#input_element##.disabled
          method disable         = self#input_element##.disabled := Js._true
          method enable          = self#input_element##.disabled := Js._false
          method toggle_disabled = self#input_element##.disabled := Js.bool @@ not self#disabled

          method value       = Js.to_string self#input_element##.value
          method set_value v = self#input_element##.value := Js.string v


        end

class virtual radio_or_cb_widget elt () =
        object(self)

          inherit input_widget elt ()

          method set_check x    = self#input_element##.checked := Js.bool x
          method checked        = Js.to_bool self#input_element##.checked
          method check          = self#input_element##.checked := Js._true
          method uncheck        = self#input_element##.checked := Js._false
          method toggle_checked = self#input_element##.checked := Js.bool @@ not self#checked

        end

class virtual text_input_widget elt () =
        object(self)

          inherit input_widget elt ()

          method required     = self#input_element##.required := Js._true
          method not_required = self#input_element##.required := Js._false

        end

let create x = new widget x ()
let coerce (x : #widget) = (x :> widget)

let widget_to_markup (x : #widget) = Tyxml_js.Of_dom.of_element x#root
let widgets_to_markup (x : #widget list) = List.map widget_to_markup x
