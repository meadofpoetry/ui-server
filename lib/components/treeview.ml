open Js_of_ocaml

include Components_tyxml.Treeview

type 'a node =
  { name : string (* node label *)
  ; value : 'a option (* Some - render checkbox, None - no checkbox needed *)
  ; load_children : (unit -> ('a node list, string) result Lwt.t)
  ; mutable children : 'a node list (* node children *)
  }

module Selector = struct
  let checkbox = "input[type=\"checkbox\"]:not(:disabled)"
  let enabled_items = Printf.sprintf ".%s:not(%s)" CSS.node CSS.node_disabled
end

let get_nodes (elt : Dom_html.element Js.t) =
  elt##querySelectorAll (Js.string Selector.enabled_items)

let has_checkbox (item : Dom_html.element Js.t) : bool =
  Element.matches item Selector.checkbox

let is_item_checked (item : Dom_html.element Js.t) : bool =
  match Element.query_selector item Selector.checkbox with
  | None -> false
  | Some x ->
    let (checkbox : Dom_html.inputElement Js.t) = Js.Unsafe.coerce x in
    Js.to_bool checkbox##.checked

(* TODO storing wrapped objects in an internal variable is
   a bad idea - we need to keep in sync internal variable and
   an actual DOM structure *)

class t () =
  let elt = Dom_html.(createDiv document) in
  object(self)

    val mutable _expand_on_click = false

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ()

    method! initial_sync_with_dom () : unit =
      (* TODO maybe search for children here and
         instantiate new objects? *)
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      super#destroy ()

    method has_children =
      self#nodes##.length <> 0

    method has_selection = false

    method all_descendant_leafs_selected =
      let rec aux = function
        | [] -> true
        | hd :: tl ->
          if not @@ is_item_checked hd
          then false else aux tl in
      self#has_children
      && self#has_selection
      && aux self#all_descendant_leafs

    method some_descendant_leafs_selected =
      let rec aux = function
        | [] -> false
        | hd :: tl ->
          if is_item_checked hd
          then true else aux tl in
      self#has_selection
      && aux self#all_descendant_leafs

    method all_descendant_leafs =
      let rec search acc = function
        | [] -> acc
        | (hd : Dom_html.element Js.t) :: tl ->
          search (acc @ Dom.list_of_nodeList @@ get_nodes hd) tl in
      search [] @@ Dom.list_of_nodeList self#nodes

    method indeterminate =
      self#has_selection
      && self#has_children
      && self#some_descendant_leafs_selected
      && not self#all_descendant_leafs_selected

    method nodes : Dom_html.element Dom.nodeList Js.t =
      get_nodes super#root
  end
