open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Menu
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

(* TODO
   - selected item is indexed only between active (not disabled) items.
     Is it ok?
*)

let ( >>= ) = Lwt.bind

let find_mapi f l =
  let rec aux f i = function
    | [] -> None
    | x :: l' -> (
      match f i x with
      | Some _ as res -> res
      | None -> aux f (i + 1) l')
  in
  aux f 0 l

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

  let select : selected Js.t Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (CSS.root ^ ":select")
end

module Lwt_js_events = struct
  open Js_of_ocaml_lwt.Lwt_js_events
  include Menu_surface.Lwt_js_events

  let select ?use_capture ?passive t = make_event ?use_capture ?passive Event.select t

  let selects ?cancel_handler ?use_capture ?passive t =
    seq_loop ?cancel_handler ?use_capture ?passive select t
end

class t ?body ?viewport ?(focus_on_open = true) (elt : Dom_html.element Js.t) () =
  object (self)
    val item_list : Item_list.t option =
      Option.map Item_list.attach @@ Element.query_selector elt Selector.list

    val mutable default_focus_item_index : int option = None

    inherit Menu_surface.t ?body ?viewport elt () as super

    method! init () : unit =
      Option.iter (fun x -> x#set_wrap_focus true) item_list;
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        [ Item_list.Lwt_js_events.actions super#root (fun e _ ->
              match Js.Opt.to_option e##.detail with
              | Some a -> self#handle_item_action a
              | None -> Lwt.return ()) ]
        @ listeners;
      super#initial_sync_with_dom ()

    method wrap_focus : bool =
      match item_list with
      | None -> false
      | Some list -> list#wrap_focus

    method set_wrap_focus (x : bool) : unit =
      match item_list with
      | None -> ()
      | Some list -> list#set_wrap_focus x

    method list : Item_list.t option = item_list

    method items : Dom_html.element Js.t list =
      match item_list with
      | None -> []
      | Some (list : Item_list.t) -> list#items

    method set_default_focus_item_index (i : int option) : unit =
      default_focus_item_index <- i

    (* Private methods *)
    method! private notify_open () : unit =
      super#notify_open ();
      (* Focus some item when opened. *)
      if focus_on_open
      then
        match default_focus_item_index with
        | None -> super#root##focus
        | Some i -> (
          match List.nth_opt self#items i with
          | None -> ()
          | Some item -> item##focus)

    method private notify_selected (item : Dom_html.element Js.t) : unit =
      let index =
        find_mapi (fun i x -> if Element.equal x item then Some i else None) self#items
        |> function
        | None -> raise Not_found
        | Some x -> x
      in
      let (detail : Event.selected Js.t) =
        object%js
          val index = index

          val item = item
        end
      in
      super#emit ~detail Event.select

    method! private handle_keydown
        (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      match Dom_html.Keyboard_code.of_event e with
      | Tab | Escape -> super#close ()
      | ArrowUp ->
          if Element.is_focused super#root
          then (
            Dom.preventDefault e;
            match List.rev self#items with
            | x :: _ -> x##focus
            | _ -> ());
          Lwt.return ()
      | ArrowDown ->
          if Element.is_focused super#root
          then (
            Dom.preventDefault e;
            match self#items with
            | x :: _ -> x##focus
            | _ -> ());
          Lwt.return ()
      | _ -> Lwt.return ()

    method private handle_item_action detail : unit Lwt.t =
      let item = detail##.item in
      self#notify_selected item;
      super#close ()
      >>= fun () ->
      Js.Opt.iter (self#get_selection_group item) (self#handle_selection_group ~item);
      Lwt.return ()

    method private handle_selection_group
        ~(item : Dom_html.element Js.t)
        (group : Dom_html.element Js.t)
        : unit =
      (* De-select the previous selection in this group. *)
      let selected = group##querySelector (Js.string Selector.selected_item) in
      Js.Opt.iter selected (fun (selected : Dom_html.element Js.t) ->
          Element.remove_attribute selected Attr.aria_selected;
          Element.remove_class selected CSS.item_selected);
      (* Select the new list item in this group. *)
      Element.set_attribute item Attr.aria_selected "true";
      Element.add_class item CSS.item_selected

    method private get_selection_group
        (item : Dom_html.element Js.t)
        : Dom_html.element Js.t Js.opt =
      let rec aux (elt : Dom_html.element Js.t Js.opt) =
        Js.Opt.bind elt (fun (parent : Dom_html.element Js.t) ->
            if Element.has_class parent CSS.selection_group
            then Js.some parent
            else if Element.has_class parent CSS.root
            then Js.null
            else aux (Element.get_parent parent))
      in
      aux (Element.get_parent item)
  end

let attach ?body ?viewport ?focus_on_open (elt : #Dom_html.element Js.t) : t =
  let body = (body :> Dom_html.element Js.t option) in
  new t ?body ?viewport ?focus_on_open (Element.coerce elt) ()

let make
    ?classes
    ?a
    ?fixed
    ?open_
    ?list_children
    ?list
    ?children
    ?body
    ?viewport
    ?focus_on_open
    () : t =
  D.menu ?classes ?a ?fixed ?open_ ?list_children ?list ?children ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?body ?viewport ?focus_on_open
