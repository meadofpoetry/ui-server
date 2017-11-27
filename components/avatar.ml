open Widget
open Markup
open Tyxml_js

module Image = struct

end

module Font_icon = struct

end

module Letter = struct

  class t ~text () =

    let elt = Avatar.Letter.create ~text () |> To_dom.of_div in

    object
      inherit widget elt () as super
      method dense     = super#add_class Avatar.dense_class
      method not_dense = super#remove_class Avatar.dense_class
    end

end
