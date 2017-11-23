type rect =
  { top    : float
  ; right  : float
  ; bottom : float
  ; left   : float
  ; width  : float option
  ; height : float option
  }

class ['a] widget (elt : 'a) () = object(self)

  method root : 'a = elt
  method element : Dom_html.element Js.t = (elt :> Dom_html.element Js.t)

  method get_attribute a    = self#element##getAttribute (Js.string a)
                              |> Js.Opt.to_option
                              |> CCOpt.map Js.to_string
  method set_attribute a v  = self#element##setAttribute (Js.string a) (Js.string v)
  method remove_attribute a = self#element##removeAttribute (Js.string a)
  method has_attribute a    = self#element##hasAttribute (Js.string a)

  method inner_html       = Js.to_string self#element##.innerHTML
  method set_inner_html s = self#element##.innerHTML := Js.string s

  method text_content = self#element##.textContent
                        |> Js.Opt.to_option
                        |> CCOpt.map Js.to_string
  method set_text_content s = self#element##.textContent := Js.some @@ Js.string s

  method id = Js.to_string self#element##.id
  method set_id id = self#element##.id := Js.string id

  method style = self#element##.style

  method class_string = Js.to_string @@ self#element##.className
  method set_class_string _classes = self#element##.className := (Js.string _classes)

  method cons_class   _class = self#set_class_string @@ _class ^ " " ^ self#class_string
  method add_class    _class = self#element##.classList##add (Js.string _class)
  method remove_class _class = self#element##.classList##remove (Js.string _class)
  method toggle_class _class = self#element##.classList##toggle (Js.string _class)
  method has_class    _class = Js.to_bool (self#element##.classList##contains (Js.string _class))
  method classes             = String.split_on_char ' ' self#class_string

  method client_left   = self#element##.clientLeft
  method client_top    = self#element##.clientTop
  method client_width  = self#element##.clientWidth
  method client_height = self#element##.clientHeight

  method offset_left   = self#element##.offsetLeft
  method offset_top    = self#element##.offsetTop
  method offset_width  = self#element##.offsetWidth
  method offset_height = self#element##.offsetHeight

  method scroll_left   = self#element##.scrollLeft
  method scroll_top    = self#element##.scrollTop
  method scroll_width  = self#element##.scrollWidth
  method scroll_height = self#element##.scrollHeight

  method client_rect   = (self#element##getBoundingClientRect)
                         |> (fun x -> { top    = x##.top
                                      ; right  = x##.right
                                      ; bottom = x##.bottom
                                      ; left   = x##.left
                                      ; width  = Js.Optdef.to_option x##.width
                                      ; height = Js.Optdef.to_option x##.height })

end

class virtual ['a] input_widget (elt : 'a) () =
        object(self)

          inherit ['a] widget elt ()

          method virtual input : Dom_html.inputElement Js.t

          method disabled        = Js.to_bool self#input##.disabled
          method disable         = self#input##.disabled := Js._true
          method enable          = self#input##.disabled := Js._false
          method toggle_disabled = self#input##.disabled := Js.bool @@ not self#disabled

          method value       = Js.to_string self#input##.value
          method set_value v = self#input##.value := Js.string v


        end

class virtual ['a] radio_or_cb_widget (elt : 'a) () =
        object(self)

          inherit ['a] input_widget elt ()

          method checked        = Js.to_bool self#input##.checked
          method check          = self#input##.checked := Js._true
          method uncheck        = self#input##.checked := Js._false
          method toggle_checked = self#input##.checked := Js.bool @@ not self#checked

        end

class virtual ['a] text_input_widget (elt : 'a) () =
        object(self)

          inherit ['a] input_widget elt ()

          method required     = self#input##.required := Js._true
          method not_required = self#input##.required := Js._false

        end
