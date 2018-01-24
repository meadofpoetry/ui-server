class t ?(inset=false) () =

  let elt = Markup.Divider.create () |> Tyxml_js.To_dom.of_element in

  object(self)

    inherit Widget.widget elt ()

    method set_inset x = self#add_or_remove_class x Markup.Divider.inset_class
    method get_inset   = self#has_class Markup.Divider.inset_class

    initializer
      self#set_inset inset

  end
