open Widget
open Markup
open Tyxml_js

module Font = struct

  class t ~icon () =

    let elt = Icon.Font.create ~icon () |> To_dom.of_i in

    object

      inherit [Dom_html.element Js.t] widget elt () as super

      method icon       = super#text_content
      method set_icon i = super#set_text_content i

    end

end
