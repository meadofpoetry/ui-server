open Components

module Options = struct

  include Options

  module Axes      = Axes
  module Hover     = Hover
  module Title     = Title
  module Layout    = Layout
  module Animation = Animation
  module Elements  = Elements
  module Legend    = Legend
  module Tooltip   = Tooltip

end

module Line = Line

class t () =
  let elt = Tyxml_js.Html.canvas [] |> Tyxml_js.To_dom.of_canvas in
  object
    inherit Widget.widget elt () as super
    method get_canvas_element = elt
    method set_width x        = super#set_attribute "width"  @@ string_of_int x
    method set_height x       = super#set_attribute "height" @@ string_of_int x
  end
