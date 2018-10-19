open Containers
open Board_types
open Components
open Common

class settings control () =
object

  inherit Widget.t Dom_html.(createDiv document) () as super

  method init () : unit =
    super#init ();
    ignore control

end
