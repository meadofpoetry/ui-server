module Primary = struct

  class t ~(title   : string)
          ~(details : #Widget.widget list)
          () =
    let elt = Markup.Expansion_panel.Primary.create ~title
                                                    ~details:(CCList.map Widget.widget_to_markup details)
                                                    ()
              |> Tyxml_js.To_dom.of_element
    in

    object

      inherit Widget.widget elt ()

    end

end

type primary =
  { title   : string
  ; details : Widget.widget list
  }

class t ~(primary: primary) () =

  let panel   = Dom_html.createDiv Dom_html.document |> Tyxml_js.Of_dom.of_element in
  let primary = new Primary.t ~title:primary.title ~details:primary.details () in
  let elt     = Markup.Expansion_panel.create ~primary:(Widget.widget_to_markup primary) ~panel ()
                |> Tyxml_js.To_dom.of_element
  in

  object

    inherit Widget.widget elt ()

  end
