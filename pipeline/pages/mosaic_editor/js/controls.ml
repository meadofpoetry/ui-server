open Js_of_ocaml
open Components
open Wm_types

module Make(I : Item) = struct

  module It = List_of_items.Make(I)

  class t ~layers ~selected ~candidates ~set_candidates () =
    let items, sel = It.make ~selected ~candidates ~set_candidates () in
    let layers = List_of_layers.make ~init:layers ~max:I.max_layers in
    let _class = "wm-right-toolbar" in
    object
      inherit Widget.t Dom_html.(createDiv document) () as super
      val mutable _s = None

      method! init () : unit =
        super#init ();
        super#add_class Box.CSS.root;
        super#add_class Box.CSS.vertical;
        super#append_child items;
        super#append_child layers;
        super#add_class _class;
        let s =
          React.S.map (fun i -> if Utils.Option.is_some i then sel `Props)
            selected in
        _s <- Some s

      method! destroy () : unit =
        super#destroy ();
        Utils.Option.iter (React.S.stop ~strong:true) _s;
        _s <- None

      method! layout () : unit =
        super#layout ();
        items#layout ();
        layers#layout ()

      method e_layers_action : List_of_layers.action React.event = layers#grid#e_layer
      method initialize_layers = layers#grid#initialize
    end

  let make ~layers ~selected = new t ~layers ~selected ()
end
