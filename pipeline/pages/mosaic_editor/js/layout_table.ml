open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

module Selector = struct
  let item = ".resizable" (* FIXME *)
end

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
  object(self)
    inherit Widget.t elt () as super
    val width = 300

    method! init () : unit =
      super#init ();
      List.iter super#append_child items;
      super#add_class Markup.CSS.table

    method! initial_sync_with_dom () : unit =
      let _ = Js_of_ocaml.MutationObserver.observe
          ~attributes:true
          ~f:(fun _ _ ->
              List.iter (fun item ->
                  let w = float_of_int @@ Position.get_width item in
                  let h = Position.get_height item in
                  print_endline @@ Printf.sprintf "old width: %gpx" w;
                  print_endline @@ Printf.sprintf "old height: %dpx" h;
                  let left = float_of_int @@ Position.get_left item in
                  let top = Position.get_top item in
                  let perc = (w *. 100.) /. (float_of_int width) in
                  print_endline @@ Printf.sprintf "percent: %g%%" perc;
                  let cur = float_of_int super#root##.offsetWidth in
                  let new_w = (cur *. perc) /. 100. in
                  let new_h = Position.get_height_for_width item new_w in
                  print_endline @@ Printf.sprintf "new width: %gpx" new_w;
                  print_endline @@ Printf.sprintf "new height: %dpx" new_h; 
                  let new_left = (left *. new_w) /. w in
                  let new_top = (top * new_h) / h in
                  item##.style##.top := Utils.px_js new_top;
                  item##.style##.left := Utils.px_js @@ Float.to_int @@ Float.floor new_left;
                  item##.style##.width := Utils.px_js @@ Float.to_int @@ Float.floor new_w;
                  item##.style##.height := Utils.px_js new_h)
                self#items)
          ~node:super#root
          () in
      super#initial_sync_with_dom ()

    method! layout () : unit =
      List.iter Widget.layout items;
      super#layout ()

    method fit () : unit =
      ()

    method items : Dom_html.element Js.t list =
      Element.query_selector_all super#root Selector.item
  end

let make_item x y w h =
  let item = Resizable.make () in
  let ar = (float_of_int w) /. (float_of_int h) in
  item#set_attribute Position.Attr.width (string_of_int w);
  item#set_attribute Position.Attr.height (string_of_int h);
  item#set_attribute Position.Attr.left (string_of_int x);
  item#set_attribute Position.Attr.top (string_of_int y);
  item#set_attribute Position.Attr.aspect_ratio (Printf.sprintf "%g" ar);
  item#root##.style##.width := Utils.px_js w;
  item#root##.style##.height := Utils.px_js h;
  item#root##.style##.top := Utils.px_js y;
  item#root##.style##.left := Utils.px_js x;
  item

let make () =
  let items =
    [ make_item 0 0 111 100
    ; make_item 111 0 189 100
    ; make_item 0 100 200 100
    ; make_item 200 100 100 100
    ] in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Tyxml_js.Html.(div []) in
  new t ~items elt ()
