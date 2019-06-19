open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

let ( >>= ) = Lwt.bind

let set_tab_index ?prev
    (items_lazy : unit -> Dom_html.element Js.t list)
    (item : Dom_html.element Js.t) : unit =
  let set (i : int) (elt : Dom_html.element Js.t) =
    Element.set_attribute elt "tabindex" (string_of_int i) in
  (match prev with
   | Some prev -> if not @@ Element.equal item prev then set (-1) prev
   | None ->
     (* If no list item was selected, set first list item's tabindex to -1.
        Generally, tabindex is set to 0 on first list item of list that has
        no preselected items *)
     match items_lazy () with
     | first :: _ -> if not @@ Element.equal first item then (set (-1) first)
     | _ -> ());
  set 0 item

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

class t ~width ~height elt () =
  object(self)
    inherit Widget.t elt () as super
    val grid_overlay = Grid_overlay.make 10
    val aspect = float_of_int width /. float_of_int height
    val width = width
    val height = height

    val mutable _listeners = []
    val mutable _focused_item = None

    method! init () : unit =
      super#init ();
      super#append_child grid_overlay

    method! initial_sync_with_dom () : unit =
      let _ = Ui_templates.Resize_observer.observe
          ~f:(fun _ -> self#fit ())
          ~node:super#root
          () in
      _listeners <- Events.(
        [ listen_lwt super#root Resizable.Event.input self#handle_item_drag
        ; listen_lwt super#root Resizable.Event.change self#handle_item_change
        ; listen_lwt super#root Resizable.Event.selected self#handle_item_selected
        ; keydowns super#root self#handle_keydown
        ]);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel _listeners;
      _listeners <- [];
      super#destroy ()

    method! layout () : unit =
      self#fit ();
      grid_overlay#layout ();
      super#layout ()

    method show_grid (x : bool) : unit =
      grid_overlay#set_show_grid x

    method show_snap_lines (x : bool) : unit =
      grid_overlay#set_show_snap_lines x

    method fit () : unit =
      let scale_factor = self#scale_factor in
      let width' = int_of_float @@ float_of_int width *. scale_factor in
      let height' = int_of_float @@ float_of_int height *. scale_factor in
      super#root##.style##.width := Utils.px_js width';
      super#root##.style##.height := Utils.px_js height';
      List.iter (fun item ->
          let w = float_of_int @@ Position.get_original_width item in
          let h = float_of_int @@ Position.get_original_height item in
          let left = float_of_int @@ Position.get_original_left item in
          let top = float_of_int @@ Position.get_original_top item in
          let new_w, new_h =
            let w' = w *. scale_factor in
            (* XXX maybe use item aspect ratio to calculate new height? *)
            let h' = h *. scale_factor in
            w', h' in
          let new_left = (left *. new_w) /. w in
          let new_top = (top *. new_h) /. h in
          item##.style##.top := Utils.px_js @@ Float.to_int @@ Float.floor new_top;
          item##.style##.left := Utils.px_js @@ Float.to_int @@ Float.floor new_left;
          item##.style##.width := Utils.px_js @@ Float.to_int @@ Float.floor new_w;
          item##.style##.height := Utils.px_js @@ Float.to_int @@ Float.floor new_h)
        self#items

    method items : Dom_html.element Js.t list =
      Element.query_selector_all super#root Selector.item

    (* Private methods *)

    method private handle_keydown e _ =
      (* Navigation keys *)
      (* TODO Implement as described in https://www.w3.org/TR/wai-aria-practices/#layoutGrid *)
      (match Events.Key.of_event e with
       | `Arrow_left -> ()
       | `Arrow_right -> ()
       | `Arrow_down -> ()
       | `Arrow_up -> ()
       | `Page_up -> () (* XXX optional *)
       | `Page_down -> () (* XXX optional *)
       | `Home -> ()
       | `End -> ()
       (* Other keys *)
       | `Enter | `Space -> () (* XXX maybe move to the next layer here? *)
       | `Delete -> ()
       | _ -> ());
      Lwt.return_unit

    method private handle_item_selected e _ =
      let target = Dom_html.eventTarget e in
      target##focus;
      set_tab_index ?prev:_focused_item (fun () -> self#items) target;
      _focused_item <- Some target;
      Lwt.async (fun () ->
          Events.blur target
          >>= fun _ -> (* TODO do smth *) Lwt.return_unit);
      Lwt.return_unit

    method private handle_item_drag e _ =
      let target = Dom_html.eventTarget e in
      let position =
        Position.of_client_rect
        @@ Widget.event_detail e in
      let aspect_ratio =
        match Element.get_attribute target Position.Attr.keep_aspect_ratio with
        | Some "true" -> Position.get_original_aspect_ratio target
        | _ -> None in
      let adjusted, lines =
        Resizable.Sig.adjust_position
          ?aspect_ratio
          target
          position
          self#items
          (super#root##.offsetWidth, super#root##.offsetHeight) in
      grid_overlay#set_snap_lines lines;
      Position.apply_to_element adjusted target;
      Lwt.return_unit

    method private handle_item_change e _ =
      grid_overlay#set_snap_lines [];
      Lwt.return_unit

    method private parent_rect : float * float * float =
      Js.Opt.case (Element.get_parent super#root)
        (fun () -> 0., 0., 1.)
        (fun x ->
           let width = float_of_int x##.offsetWidth in
           let height = float_of_int x##.offsetHeight in
           width, height, width /. height)

    method private scale_factor : float =
      let cur_width, cur_height, cur_aspect = self#parent_rect in
      if cur_aspect > aspect
      then cur_height /. float_of_int height
      else cur_width /. float_of_int width
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
    [ make_item 0 0 111 150
    ; make_item 111 0 189 150
    ; make_item 0 150 200 150
    ; make_item 210 150 90 150
    ] in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.create_grid ~content:(List.map Widget.to_markup items) () in
  new t ~width:1920 ~height:1080 elt ()
