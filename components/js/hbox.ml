open Containers

module Markup = Box.Markup

class t ?tag ?(gap=0) ?wrap ?halign ?valign ~(widgets:#Widget.t list) () =
object
  inherit Box.t ?tag ?wrap ~gap ~direction:`Row ~widgets () as super

  method! init () : unit =
    super#init ();
    Option.iter super#set_justify_content halign;
    Option.iter super#set_align_items valign

  method! set_direction = failwith "not implemented for horizontal box"

  method set_halign = super#set_justify_content
  method halign = super#justify_content

  method set_valign = super#set_align_items
  method valign = super#align_items
end
