open Widget
open Markup
open Tyxml_js

module Font = struct

  class t ~icon () =

    let elt = Icon.Font.create ~icon () |> To_dom.of_i in

    object

      inherit [Dom_html.element Js.t] widget elt ()

    end

end
