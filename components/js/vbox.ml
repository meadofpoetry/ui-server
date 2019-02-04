open Containers

module Markup = Box.Markup

class t ?tag ?(gap = 0) ?wrap ?halign ?valign ~(widgets : #Widget.t list) () =
object
  inherit Box.t ?tag ?wrap ~gap ~direction:`Column ~widgets () as super

  method! init () : unit =
    super#init ();
    Option.iter super#set_align_items halign;
    Option.iter super#set_justify_content valign

  method! set_direction _ = failwith "not implemented for vertical box"

  method set_valign = super#set_justify_content
  method valign = super#justify_content

  method set_halign = super#set_align_items
  method halign = super#align_items

end
