open Components

class settings control () =
object

  inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super

  method! init () : unit =
    super#init ();
    ignore control

end
