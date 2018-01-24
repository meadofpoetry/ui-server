class t () =

  let elt = Markup.Expansion_panel.create ~content:(Dom_html.createDiv Dom_html.document
                                                    |> Tyxml_js.Of_dom.of_element)
                                          ()
            |> Tyxml_js.To_dom.of_element
  in

  object

    inherit Widget.widget elt ()

  end
