open Containers

module Markup = Box.Markup

class t ?tag ?(gap=0) ?halign ?valign ~(widgets:#Widget.t list) () =
object(self)
  inherit Box.t ?tag ~gap ~direction:`Row ~widgets ()

  method! set_direction = failwith "not implemented for horizontal box"

  method set_halign = self#set_justify_content
  method halign     = self#justify_content

  method set_valign = self#set_align_items
  method valign     = self#align_items
  initializer
    Option.iter self#set_halign halign;
    Option.iter self#set_valign valign
end
