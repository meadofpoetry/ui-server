module Image = struct

end

module Font_icon = struct

end

module Letter = struct

  class t ~text () =
    let elt = Markup.Avatar.Letter.create ~text () |> Tyxml_js.To_dom.of_div in
    object
      inherit Widget.widget elt () as super
      method set_dense x = Markup.Avatar.dense_class
                           |> (fun c -> if x then super#add_class c  else super#remove_class c)
    end

end
