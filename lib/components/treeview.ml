open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Treeview
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( % ) f g x = f (g x)

(* TODO
   - fix initial selection
   - implement "update" method
   - implement make/attach functions - DONE *)

module Js = struct
  include Js
  let pp _ ppf _ = Format.pp_print_string ppf "js"
end

module Dom_html = struct
  include Dom_html
  let pp_element ppf _ = Format.pp_print_string ppf "element"
end

type node =
  { label : string
  ; secondary_text : string option
  ; graphic : Dom_html.element Js.t option
  ; meta : Dom_html.element Js.t option
  ; value : string option
  ; checked : bool option
  ; indeterminate : bool option
  ; expanded : bool
  ; children : node list
  } [@@deriving show]

let make_node ?secondary_text ?value ?graphic ?meta
    ?(children = [])
    ?checked
    ?indeterminate
    ?(expanded = false)
    label =
  { label
  ; secondary_text
  ; value
  ; children
  ; graphic
  ; checked
  ; indeterminate
  ; expanded
  ; meta
  }

let elements_key_allowed_in =
  ["input"; "button"; "textarea"; "select"]

module Selector = struct
  let node = Printf.sprintf ".%s" CSS.node
  let checkbox = "input[type=\"checkbox\"]:not(:disabled)"
  let single_selected_node = Printf.sprintf ".%s, .%s"
      CSS.node_activated CSS.node_selected
  let aria_checked = "[aria-checked=\"true\"]"
  let children = "[role=\"group\"]"
  let node_without_tabindex = Printf.sprintf ".%s:not([tabindex])" CSS.node
  let focusable_child_elements =
    Printf.sprintf
      ".%s button:not(:disabled), \
       .%s a, \
       .%s input[type=\"checkbox\"]:not(:disabled)"
      CSS.node CSS.node CSS.node
end

module Attr = struct
  let value = "data-value"
  let aria_expanded = "aria-expanded"
  let aria_checked = "aria-checked"
  let aria_current = "aria-current"
  let aria_orientation = "aria-orientation"
  let aria_selected = "aria-selected"
end

module Event = struct
  let (action : Dom_html.element Js.t Widget.custom_event Js.t Events.Typ.typ) =
    Events.Typ.make "treeview:action"
end

let get_exn (i : int) (list : Dom_html.element Dom.nodeList Js.t) =
  Js.Opt.get (list##item i) (fun () -> assert false)

let has_checkbox_at_index (i : int)
    (items : Dom_html.element Dom.nodeList Js.t) : bool =
  Js.Opt.(test @@ bind (items##item i) (fun (item : Dom_html.element Js.t) ->
      item##querySelector (Js.string Selector.checkbox)))

let prevent_default_event (e : #Dom_html.event Js.t) : unit =
  Js.Opt.iter e##.target (fun (elt : Dom_html.element Js.t) ->
      if not @@ List.exists (String.equal (Js.to_string elt##.tagName##toLowerCase))
          elements_key_allowed_in
      then Dom.preventDefault e)

let get_node_checked_state (item : Dom_html.element Js.t) =
  match Element.query_selector item Selector.checkbox with
  | None -> `Unavailable
  | Some x ->
    let (checkbox : Dom_html.inputElement Js.t) = Js.Unsafe.coerce x in
    if Js.to_bool (Js.Unsafe.coerce checkbox)##.indeterminate
    then `Indeterminate
    else if Js.to_bool checkbox##.checked
    then `Checked
    else `Unchecked

let is_node_checked (item : Dom_html.element Js.t) : bool option =
  match Element.query_selector item Selector.checkbox with
  | None -> None
  | Some x ->
    let (checkbox : Dom_html.inputElement Js.t) = Js.Unsafe.coerce x in
    Some (Js.to_bool checkbox##.checked)

let set_tab_index ?prev (items : Dom_html.element Dom.nodeList Js.t)
    (item : Dom_html.element Js.t) : unit =
  let set (i : int) (elt : Dom_html.element Js.t) =
    elt##setAttribute (Js.string "tabindex") (Js.string (string_of_int i)) in
  (match prev with
   | None ->
     (* If no tree node was selected, set first list item's tabindex to -1.
        Generally, tabindex is set to 0 on first list item of list that has
        no preselected items *)
     Js.Opt.iter (items##item 0) (fun first ->
         if not @@ Element.equal first item
         then (set (-1) first))
   | Some prev -> if not @@ Element.equal item prev then set (-1) prev);
  set 0 item

let set_node_checked (x : bool) (item : Dom_html.element Js.t) : unit =
  match Element.query_selector item Selector.checkbox with
  | None -> ()
  | Some elt ->
    let (input : Dom_html.inputElement Js.t) = Js.Unsafe.coerce elt in
    input##.checked := Js.bool x;
    (Js.Unsafe.coerce input)##.indeterminate := Js._false;
    let event =
      (Js.Unsafe.coerce Dom_html.document)##createEvent
        (Js.string "Event") in
    ignore @@ event##initEvent (Js.string "change") Js._true Js._true;
    (Js.Unsafe.coerce input)##dispatchEvent event

let set_node_indeterminate (x : bool) (item : Dom_html.element Js.t) : unit =
  match Element.query_selector item Selector.checkbox with
  | None -> ()
  | Some elt ->
    let (input : Dom_html.inputElement Js.t) = Js.Unsafe.coerce elt in
    (Js.Unsafe.coerce input)##.indeterminate := Js.bool x

let loop_nodes f (list : Dom_html.element Dom.nodeList Js.t) =
  let length = list##.length in
  let rec loop = function
    | x when x = length -> ()
    | i -> f i (get_exn i list); loop (succ i) in
  loop 0

let find_node f (list : Dom_html.element Dom.nodeList Js.t) =
  let rec find = function
    | 0 -> Js.null
    | i ->
      let item =
        Js.Opt.bind (list##item (i - 1)) (fun e ->
            if f e then Js.some e else Js.null) in
      if Js.Opt.test item
      then item else find (pred i) in
  find list##.length

let tree_node_of_event (items : Dom_html.element Dom.nodeList Js.t)
    (e : Dom_html.event Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option
  @@ Js.Opt.bind e##.target (fun (target : Dom_html.element Js.t) ->
      let selector = Printf.sprintf ".%s, .%s" CSS.node CSS.root in
      let nearest_parent = Element.closest target selector in
      Js.Opt.bind nearest_parent (fun (parent : Dom_html.element Js.t) ->
          if not @@ Element.matches parent ("." ^ CSS.node)
          then Js.null
          else find_node (Element.equal parent) items))

let content_of_node (node : Dom_html.element Js.t) =
  Element.query_selector node ("." ^ CSS.node_content)

(* TODO needs optimization, obviously we don't need to iter over all nodes *)
let focus_prev_node
    (active : Dom_html.element Js.t)
    (nodes : Dom_html.element Dom.nodeList Js.t) =
  let rec aux ?(found_active = false) = function
    | i when i < 0 -> None
    | i ->
      let item = get_exn i nodes in
      if (not found_active) && not @@ Element.equal item active
      then aux (pred i)
      else match Js.Opt.to_option @@ nodes##item (pred i) with
        | None -> None
        | Some prev ->
          if Tabbable.is_focusable prev
          then (prev##focus; Some prev)
          else aux ~found_active:true (pred i) in
  aux (nodes##.length - 1)

let focus_next_node
    (active : Dom_html.element Js.t)
    (nodes : Dom_html.element Dom.nodeList Js.t) =
  let length = nodes##.length in
  let rec aux ?(found_active = false) = function
    | i when i = length -> None
    | i ->
      let item = get_exn i nodes in
      if (not found_active) && not @@ Element.equal item active
      then aux (succ i)
      else match Js.Opt.to_option @@ nodes##item (succ i) with
        | None -> None
        | Some next ->
          if Tabbable.is_focusable next
          then (next##focus; Some next)
          else aux ~found_active:true (succ i) in
  aux 0

let focus_first_node
    (active : Dom_html.element Js.t)
    (nodes : Dom_html.element Dom.nodeList Js.t) =
  let rec aux i =
    match Js.Opt.to_option @@ nodes##item i with
    | None -> None
    | Some x ->
      if Element.equal active x
      then None
      else if Tabbable.is_focusable x
      then Some x
      else aux (succ i) in
  let first = aux 0 in
  Utils.Option.iter (fun x -> x##focus) first;
  first

let focus_last_node
    (active : Dom_html.element Js.t)
    (nodes : Dom_html.element Dom.nodeList Js.t) =
  let rec aux i =
    match Js.Opt.to_option @@ nodes##item i with
    | None -> None
    | Some x ->
      if Element.equal active x
      then None
      else if Tabbable.is_focusable x
      then Some x
      else aux (pred i) in
  let last = aux (nodes##.length - 1) in
  Utils.Option.iter (fun x -> x##focus) last;
  last

class t elt () =
  object(self)

    val mutable _aria_current_value = None
    val mutable _use_activated_class = false
    val mutable _focused_node = None
    val mutable _selected_items = []
    val mutable _is_single_selection = false
    val mutable _is_vertical = false
    val mutable _listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ()

    method! initial_sync_with_dom () : unit =
      (* TODO maybe search for children here and
         instantiate new objects? *)
      super#initial_sync_with_dom ();
      let click = Events.clicks super#root self#handle_click in
      let keydown = Events.keydowns super#root self#handle_keydown in
      _listeners <- [click; keydown];
      self#layout ();
      self#initialize_tree_type ()

    method! layout () : unit =
      super#layout ();
      (match Element.get_attribute super#root Attr.aria_orientation with
       | Some "horizontal" -> self#set_vertical false
       | _ -> self#set_vertical true);
      (* Treeview nodes need to have at least tabindex=-1 to be focusable *)
      loop_nodes (fun _ node -> Element.set_attribute node "tabindex" "-1")
      @@ super#root##querySelectorAll (Js.string Selector.node_without_tabindex);
      (* Child button/a elements are not tabbable until the list item is focused *)
      loop_nodes (fun _ item -> Element.set_attribute item "tabindex" "-1")
      @@ super#root##querySelectorAll (Js.string Selector.focusable_child_elements);
      let nodes = self#nodes_ in
      Js.Opt.iter (nodes##item 0) (fun x -> Element.set_attribute x "tabindex" "0")

    method! destroy () : unit =
      super#destroy ();
      List.iter Lwt.cancel _listeners;
      _listeners <- []

    method initialize_tree_type () : unit =
      let single_selected_node =
        super#root##querySelector (Js.string Selector.single_selected_node) in
      let preselected_items =
        Element.query_selector_all super#root
          Selector.aria_checked in
      _selected_items <- preselected_items;
      if Js.Opt.test single_selected_node
      then (
        let item = Js.Opt.get single_selected_node (fun () -> assert false) in
        if Element.has_class item CSS.node_activated
        then self#set_use_activated true;
        self#set_single_selection true;
        _selected_items <- [item])

    method set_vertical (x : bool) : unit =
      _is_vertical <- x

    method set_use_activated (x : bool) : unit =
      _use_activated_class <- x

    method set_single_selection (x : bool) : unit =
      _is_single_selection <- x

    method is_empty : bool =
      match Element.children super#root with
      | [] -> true | _ -> false

    method selected_nodes : Dom_html.element Js.t list =
      _selected_items

    method selected_leafs : Dom_html.element Js.t list =
      List.filter self#is_leaf _selected_items

    method node_value (node : Dom_html.element Js.t) : string option =
      Element.get_attribute node Attr.value

    (* Returns node's parent, if any *)
    method node_parent (node : Dom_html.element Js.t) =
      let rec aux node =
        Js.Opt.bind (Element.get_parent node)
          (fun parent ->
             if Element.has_class parent CSS.node
             then Js.some parent
             else aux parent) in
      aux node

    method nodes =
      Dom.list_of_nodeList self#nodes_

    (* Private methods *)

    (* Returns all nodes of a treeview *)
    method private nodes_ : Dom_html.element Dom.nodeList Js.t =
      super#root##querySelectorAll (Js.string Selector.node)

    method private get_node_siblings (node : Dom_html.element Js.t)
      : Dom_html.element Js.t list =
      let rec loop acc node =
        let next = Element.get_next_sibling node in
        match Js.Opt.to_option next with
        | None -> acc
        | Some x -> loop (x :: acc) x in
      List.rev @@ loop [] node

    (* Returns node's children, if any *)
    method private get_node_children (node : Dom_html.element Js.t)
      : Dom_html.element Js.t list =
      let rec aux acc node =
        let children_wrapper = node##querySelector (Js.string Selector.children) in
        Js.Opt.case children_wrapper
          (fun () -> [])
          (fun w ->
             List.filter (fun x -> Element.has_class x CSS.node)
             @@ Element.children w) in
      aux [] node

    (* Check if a tree node is an end node *)
    method private is_leaf (node : Dom_html.element Js.t) : bool =
      match Element.has_class node CSS.node, self#get_node_children node with
      | true, [] -> true
      | _ -> false

    method private node_expanded (node : Dom_html.element Js.t) =
      match Element.get_attribute node Attr.aria_expanded with
      | Some "true" -> true
      | _ -> false

    method private set_node_expanded (node : Dom_html.element Js.t) x =
      Element.set_attribute node Attr.aria_expanded (string_of_bool x)

    method private toggle_expanded_state (node : Dom_html.element Js.t) : unit =
      self#set_node_expanded node (not @@ self#node_expanded node)

    method private handle_keydown
        (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      let nodes = self#nodes_ in
      match tree_node_of_event nodes (e :> Dom_html.event Js.t) with
      | None -> Lwt.return_unit
      | Some item ->
        match Js.Opt.to_option Dom_html.document##.activeElement with
        | None -> Lwt.return_unit
        | Some active ->
          let next, stop =
            match Events.Key.of_event e, _is_vertical with
            | `Arrow_left, true | `Arrow_up, false ->
              prevent_default_event e;
              if self#node_expanded active
              then (
                self#set_node_expanded active false;
                None, false)
              else begin match Js.Opt.to_option @@ self#node_parent active with
                | None -> None, false
                | Some p -> p##focus; Some p, false
              end
            | `Arrow_right, true | `Arrow_down, false ->
              prevent_default_event e;
              begin match self#node_expanded active, self#get_node_children active with
                | _, [] -> None, false
                | false, _ -> self#set_node_expanded active true; None, false
                | true, x :: _ -> x##focus; Some x, false
              end
            | `Arrow_up, true | `Arrow_left, false ->
              prevent_default_event e;
              focus_prev_node active nodes, false
            | `Arrow_down, true | `Arrow_right, false ->
              prevent_default_event e;
              focus_next_node active nodes, false
            | `Home, _ ->
              prevent_default_event e;
              focus_first_node active nodes, false
            | `End, _ ->
              prevent_default_event e;
              focus_last_node active nodes, false
            | `Numpad_multiply, _ ->
              prevent_default_event e;
              List.iter (fun x -> self#set_node_expanded x true)
              @@ active :: self#get_node_siblings active;
              None, false
            | (`Enter as k), _ | (`Space as k), _ ->
              if Element.has_class item CSS.node
              then (
                (* Return early if enter key is pressed on anchor element
                   which triggers synthetic mouseEvent event *)
                let is_a_tag =
                  Js.Opt.map e##.target (fun e ->
                      String.equal "A" (Js.to_string e##.tagName))
                  |> fun x -> Js.Opt.get x (fun () -> false) in
                let is_enter = match k with `Enter -> true | _ -> false in
                if is_a_tag && is_enter then None, true else (
                  prevent_default_event e;
                  self#handle_action active;
                  self#notify_action active;
                  None, false))
              else None, false
            | _ -> None, false in
          if not stop
          then (
            match next with
            | None -> ()
            | Some next ->
              set_tab_index ~prev:active nodes next;
              _focused_node <- Some next);
          Lwt.return_unit

    method private handle_click (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      Utils.Option.iter (fun node ->
          let target = Dom.eventTarget e in
          let is_checkbox = Element.matches target Selector.checkbox in
          self#handle_action ~is_checkbox node;
          self#notify_action node)
      @@ tree_node_of_event self#nodes_ (e :> Dom_html.event Js.t);
      Lwt.return_unit

    method private handle_action
        ?(is_checkbox = false)
        (node : Dom_html.element Js.t) =
      if self#is_leaf node || is_checkbox
      then (
        (* Toggle the checkbox only if it's not the target of the event,
           or the checkbox will have 2 change events. *)
        self#toggle_checkbox ~origin:true ~toggle:(not is_checkbox) node)
      else self#toggle_expanded_state node

    method private notify_action (node : Dom_html.element Js.t) : unit =
      super#emit ~detail:node ~should_bubble:true Event.action

    method private get_node_checked_state (node : Dom_html.element Js.t) :
      [`Checked | `Unchecked | `Indeterminate] =
      let children = self#get_node_children node in
      let rec aux ((checked, unchecked, indeterminate) as acc)= function
        | [] ->
          if (not indeterminate) && (not unchecked)
          then `Checked
          else if (not indeterminate && (not checked))
          then `Unchecked
          else `Indeterminate
        | hd :: tl ->
          match get_node_checked_state hd with
          | `Checked -> aux (true, unchecked, indeterminate) tl
          | `Unchecked -> aux (checked, true, indeterminate) tl
          | `Indeterminate -> aux (checked, unchecked, true) tl
          | `Unavailable -> aux acc tl
      in
      aux (false, false, false) children

    method private update_children (checked : bool) (node : Dom_html.element Js.t) : unit =
      List.iter (self#toggle_checkbox ~checked)
      @@ self#get_node_children node

    method private update_parent (node : Dom_html.element Js.t) : unit =
      let rec aux parent =
        (match self#get_node_checked_state parent with
         | `Indeterminate -> set_node_indeterminate true parent
         | `Checked ->
           set_node_checked true parent;
           set_node_indeterminate false parent
         | `Unchecked ->
           set_node_checked false parent;
           set_node_indeterminate false parent);
        Js.Opt.iter (self#node_parent parent) aux in
      Js.Opt.iter (self#node_parent node) aux

    method private toggle_checkbox
        ?(origin = false)
        ?checked
        ?(toggle = true)
        (node : Dom_html.element Js.t) : unit =
      match is_node_checked node with
      | None -> ()
      | Some state ->
        let checked = match checked with
          | Some x -> x
          | None -> if toggle then not state else state in
        if toggle then set_node_checked checked node;
        self#update_children checked node;
        if origin then self#update_parent node;
        if checked
        then (
          if not @@ List.exists (Element.equal node) _selected_items
          then _selected_items <- node :: _selected_items)
        else _selected_items <- List.filter (not % Element.equal node) _selected_items

  end

let make ?classes ?attrs ?dense ?two_line (nodes : node list) : t =
  let rec loop acc = function
    | [] -> List.rev acc
    | node :: tl ->
      Utils.Option.iter (fun x ->
          Element.add_class x Item_list.CSS.item_graphic)
        node.graphic;
      Utils.Option.iter (fun x ->
          Element.add_class x Item_list.CSS.item_meta)
        node.meta;
      let node =
        Markup.create_node
          ?value:node.value
          ?secondary_text:node.secondary_text
          ?meta:(Utils.Option.map Tyxml_js.Of_dom.of_element node.meta)
          ?graphic:(Utils.Option.map Tyxml_js.Of_dom.of_element node.graphic)
          ?checked:node.checked
          ?indeterminate:node.indeterminate
          ~expanded:node.expanded
          ~children:(loop [] node.children)
          node.label in
      loop (node :: acc) tl in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?classes ?attrs ?dense ?two_line (loop [] nodes) in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
