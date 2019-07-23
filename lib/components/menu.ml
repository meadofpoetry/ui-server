open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Utils

(* TODO
   - selected item is indexed only between active (not disabled) items.
     Is it ok?
*)

include Components_tyxml.Menu
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( >>= ) = Lwt.bind

module Attr = struct
  let aria_selected = "aria-selected"
end

module Selector = struct
  let selected_item = "." ^ CSS.item_selected
  let list = "." ^ Item_list.CSS.root
end

module Event = struct
  include Menu_surface.Event

  class type selected =
    object
      method index : int Js.readonly_prop
      method item : Dom_html.element Js.t Js.readonly_prop
    end

  let selected : selected Js.t Widget.custom_event Js.t Dom_html.Event.typ =
    Dom_html.Event.make "menu:selected"
end

class t ?body ?viewport ?list ?(focus_on_open = true) (elt : Dom_html.element Js.t) () =
  object(self)
    val _list : Item_list.t option =
      match list with
      | Some x -> Some x
      | None ->
        Option.map Item_list.attach
        @@ Element.query_selector elt Selector.list
    val mutable _default_focus_item_index : int option = None
    val mutable _action_listener = None

    inherit Menu_surface.t ?body ?viewport elt () as super

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      Option.iter (fun x -> x#set_wrap_focus true) _list;
      let action =
        Lwt_js_events.seq_loop
          (Lwt_js_events.make_event Item_list.Event.action)
          super#root (fun e _ ->
            match Js.Opt.to_option e##.detail with
            | Some a -> self#handle_item_action a
            | None -> Lwt.return ()) in
      _action_listener <- Some action

    method! destroy () : unit =
      super#destroy ();
      (* Detach event listeners. *)
      Option.iter Lwt.cancel _action_listener;
      _action_listener <- None

    method wrap_focus : bool =
      match _list with
      | None -> false
      | Some list -> list#wrap_focus

    method set_wrap_focus (x : bool) : unit =
      match _list with
      | None -> ()
      | Some list -> list#set_wrap_focus x

    method list : Item_list.t option = _list

    method items : Dom_html.element Js.t list =
      match _list with
      | None -> []
      | Some (list : Item_list.t) -> list#items

    method set_default_focus_item_index (i : int option) : unit =
      _default_focus_item_index <- i

    (* Private methods *)

    method! private notify_open () : unit =
      super#notify_open ();
      (* Focus some item when opened. *)
      if focus_on_open
      then match _default_focus_item_index with
        | None -> super#root##focus
        | Some i ->
          match List.nth_opt self#items i with
          | None -> ()
          | Some item -> item##focus

    method private notify_selected (item : Dom_html.element Js.t) : unit =
      let index =
        List.find_mapi (fun i x ->
            if Element.equal x item
            then Some i else None) self#items
        |> function None -> raise Not_found | Some x -> x in
      let (detail : Event.selected Js.t) =
        object%js
          val index = index
          val item = item
        end in
      super#emit ~detail Event.selected

    method! private handle_keydown (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      match Dom_html.Keyboard_code.of_event e with
      | Tab | Escape -> super#close ()
      | ArrowUp ->
        if Element.is_focused super#root
        then (Dom.preventDefault e;
              match List.rev self#items with
              | x :: _ -> x##focus
              | _ -> ());
        Lwt.return ()
      | ArrowDown ->
        if Element.is_focused super#root
        then (Dom.preventDefault e;
              match self#items with
              | x :: _ -> x##focus
              | _ -> ());
        Lwt.return ()
      | _ -> Lwt.return ()

    method private handle_item_action (item : Dom_html.element Js.t) : unit Lwt.t =
      self#notify_selected item;
      super#close ()
      >>= fun () -> Lwt_js.sleep Menu_surface.Const.transition_close_duration_s
      >>= fun () ->
      let selection_group = self#get_selection_group item in
      Js.Opt.iter selection_group (self#handle_selection_group ~item);
      Lwt.return ()

    method private handle_selection_group ~(item : Dom_html.element Js.t)
        (group : Dom_html.element Js.t) : unit =
      (* De-select the previous selection in this group. *)
      let selected = group##querySelector (Js.string Selector.selected_item) in
      Js.Opt.iter selected (fun (selected : Dom_html.element Js.t) ->
          Element.remove_attribute selected Attr.aria_selected;
          Element.remove_class selected CSS.item_selected);
      (* Select the new list item in this group. *)
      Element.set_attribute item Attr.aria_selected "true";
      Element.add_class item CSS.item_selected

    method private get_selection_group (item : Dom_html.element Js.t) :
      Dom_html.element Js.t Js.opt =
      let rec aux (elt : Dom_html.element Js.t Js.opt) =
        Js.Opt.bind elt (fun (parent : Dom_html.element Js.t) ->
            if Element.has_class parent CSS.selection_group
            then Js.some parent
            else if Element.has_class parent CSS.root
            then Js.null
            else aux (Element.get_parent parent)) in
      aux (Element.get_parent item)
  end

let make_of_item_list ?body ?viewport ?focus_on_open
    ?fixed ?open_ (list : Item_list.t) : t =
  let body = (body :> Dom_html.element Js.t option) in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?fixed ?open_ (Widget.to_markup list) () in
  new t ?body ?viewport ?focus_on_open ~list elt ()

let attach ?body ?viewport ?focus_on_open (elt : #Dom_html.element Js.t) : t =
  let body = (body :> Dom_html.element Js.t option) in
  new t ?body ?viewport ?focus_on_open (Element.coerce elt) ()
