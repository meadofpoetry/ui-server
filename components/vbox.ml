open Containers

module Markup = Box.Markup

class t ?tag ?(gap=0) ?wrap ?halign ?valign ~(widgets:#Widget.t list) () =
object(self)
  inherit Box.t ?tag ?wrap ~gap ~direction:`Column ~widgets ()

  method! set_direction _ = failwith "not implemented for vertical box"

  method set_valign = self#set_justify_content
  method valign     = self#justify_content

  method set_halign = self#set_align_items
  method halign     = self#align_items
  initializer
    Option.iter self#set_halign halign;
    Option.iter self#set_valign valign
end
