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
  let children = "." ^ CSS.node_children
end

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

class t elt () =
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

    method get_all_descendant_leafs (node : Dom_html.element Js.t)
      : Dom_html.element Js.t list =
      let rec aux acc node' =
        let children_wrapper = node'##querySelector (Js.string Selector.children) in
        Js.Opt.case children_wrapper
          (fun () -> [])
          (fun w ->
             let children =
               List.filter (fun x -> Element.has_class x CSS.node)
               @@ Element.children w in
             match children with
             | [] ->
               if not @@ Element.equal node' node
               then node' :: acc
               else acc
             | x -> List.fold_left aux acc children) in
      aux [] node

    method get_node_children (node : Dom_html.element Js.t)
      : Dom_html.element Js.t list =
      let rec aux acc node =
        let children_wrapper = node##querySelector (Js.string Selector.children) in
        Js.Opt.case children_wrapper
          (fun () -> [])
          (fun w ->
             let children =
               List.filter (fun x -> Element.has_class x CSS.node)
               @@ Element.children w in
             match children with
             | [] -> acc
             | x ->
               let x = List.fold_left aux acc children in
               children @ x) in
      aux [] node

    method update_parent () =
      let rec update () =

        () in
      update ()

    (* TODO *)

    (* method has_children =
     *   self#nodes##.length <> 0
     * 
     * method has_selection = false *)

    (* method all_descendant_leafs_selected =
     *   let rec aux = function
     *     | [] -> true
     *     | hd :: tl ->
     *       if not @@ is_item_checked hd
     *       then false else aux tl in
     *   self#has_children
     *   && self#has_selection
     *   && aux self#all_descendant_leafs *)

    (* method some_descendant_leafs_selected =
     *   let rec aux = function
     *     | [] -> false
     *     | hd :: tl ->
     *       if is_item_checked hd
     *       then true else aux tl in
     *   self#has_selection
     *   && aux self#all_descendant_leafs *)

    (* method indeterminate =
     *   self#has_selection
     *   && self#has_children
     *   && self#some_descendant_leafs_selected
     *   && not self#all_descendant_leafs_selected *)

    (* method nodes : Dom_html.element Dom.nodeList Js.t =
     *   get_nodes super#root *)
  end
