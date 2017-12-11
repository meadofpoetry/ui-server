module Font = struct

  class t ~icon () =
    let elt = Markup.Icon.Font.create ~icon () |> Tyxml_js.To_dom.of_i in
    object
      inherit Widget.widget elt () as super
      method get_icon   = super#get_text_content |> CCOpt.get_or ~default:""
      method set_icon i = super#set_text_content i
    end

end
