open Js_of_ocaml_tyxml
open Components

module Divider = struct

  class t elt () =
    object
      inherit Widget.t elt ()
    end

  let make () : t =
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_divider () in
    new t elt ()

end

module Item = struct

  class t elt () =
    object
      inherit Widget.t elt ()
    end

  let make () : t =
    let elt = Tyxml_js.To_dom.of_element
      @@ Markup.create_table_item () in
    new t elt ()

end

class t ?(items = []) elt () =
  object
    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      List.iter super#append_child items;
      super#add_class Markup.CSS.table

    method! layout () : unit =
      List.iter Widget.layout items;
      super#layout ()
  end

let make () =
  let items =
    [ Resizable.make ()
    ] in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Tyxml_js.Html.(div []) in
  new t ~items elt ()
