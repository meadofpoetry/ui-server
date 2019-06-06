open Js_of_ocaml

include Components_tyxml.Treeview

type 'a node =
  { name : string (* node label *)
  ; value : 'a option (* Some - render checkbox, None - no checkbox needed *)
  ; load_children : (unit -> ('a node list, string) result Lwt.t)
  ; mutable children : 'a node list (* node children *)
  }

let elements_key_allowed_in =
  ["input"; "button"; "textarea"; "select"]

module Selector = struct
  let nodes = Printf.sprintf ".%s" CSS.node
  let checkbox = "input[type=\"checkbox\"]:not(:disabled)"
  let radio = "input[type=\"radio\"]:not(:disabled)"
  let single_selected_node =
    Printf.sprintf ".%s, .%s" CSS.node_activated CSS.node_selected
  let aria_role_checkbox = "[role=\"checkbox\"]"
  let aria_checked_checkbox = "[role=\"checkbox\"][aria-checked=\"true\"]"
  let checkbox_radio = Printf.sprintf "%s, %s" checkbox radio
  let children = "." ^ CSS.node_children
  let node_without_tabindex = Printf.sprintf ".%s:not([tabindex])" CSS.node
  let focusable_child_elements =
    Printf.sprintf
      ".%s button:not(:disabled), \
       .%s a, \
       .%s input[type=\"radio\"]:not(:disabled), \
       .%s input[type=\"checkbox\"]:not(:disabled)"
      CSS.node CSS.node CSS.node CSS.node
end

module Attr = struct
  let aria_checked = "aria-checked"
  let aria_current = "aria-current"
  let aria_orientation = "aria-orientation"
  let aria_selected = "aria-selected"
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

let is_node_checked (item : Dom_html.element Js.t) : bool =
  match Element.query_selector item Selector.checkbox with
  | None -> false
  | Some x ->
    let (checkbox : Dom_html.inputElement Js.t) = Js.Unsafe.coerce x in
    Js.to_bool checkbox##.checked

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
  match Element.query_selector item Selector.checkbox_radio with
  | None -> ()
  | Some elt ->
    let (input : Dom_html.inputElement Js.t) = Js.Unsafe.coerce elt in
    input##.checked := Js.bool x;
    let event =
      (Js.Unsafe.coerce Dom_html.document)##createEvent
        (Js.string "Event") in
    ignore @@ event##initEvent (Js.string "change") Js._true Js._true;
    (Js.Unsafe.coerce input)##dispatchEvent event

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

let focus_prev_element ?(wrap = false)
    (active : Dom_html.element Js.t)
    (items : Dom_html.element Dom.nodeList Js.t) =
  let length = items##.length in
  let rec aux = function
    | i when i = length || length = 1 -> None
    (* check if first item is focused *)
    | i when i = 0 ->
      let item = get_exn i items in
      if not @@ Element.equal item active then aux (succ i) else (
        if not wrap then None
        else (
          let prev = Js.Opt.to_option @@ items##item (length - 1) in
          Utils.Option.iter (fun x -> x##focus) prev;
          prev))
    | i ->
      let item = get_exn i items in
      if not @@ Element.equal item active then aux (succ i) else (
        let prev = get_exn (pred i) items in
        prev##focus;
        Some prev) in
  aux 0

let focus_next_element ?(wrap = false)
    (active : Dom_html.element Js.t)
    (items : Dom_html.element Dom.nodeList Js.t) =
  let length = items##.length in
  let rec aux = function
    | i when i = length || length = 1 -> None
    (* check if last item is focused *)
    | i when i = length - 1 ->
      let item = get_exn i items in
      if not @@ Element.equal item active then aux (succ i) else (
        if not wrap then None
        else (
          let next = Js.Opt.to_option @@ items##item 0 in
          Utils.Option.iter (fun x -> x##focus) next;
          next))
    | i ->
      let item = get_exn i items in
      if not @@ Element.equal item active then aux (succ i) else (
        let next = get_exn (succ i) items in
        next##focus;
        Some next) in
  aux 0

(* TODO storing wrapped objects in an internal variable is
   a bad idea - we need to keep in sync internal variable and
   an actual DOM structure *)

class t elt () =
  object(self)

    val mutable _aria_current_value = None
    val mutable _use_activated_class = false
    val mutable _focused_item = None
    val mutable _wrap_focus = false
    val mutable _selected_items = []
    val mutable _is_single_selection = false
    val mutable _is_checkbox_tree = true
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
      (* List items need to have at least tabindex=-1 to be focusable *)
      loop_nodes (fun _ item -> Element.set_attribute item "tabindex" "-1")
      @@ super#root##querySelectorAll (Js.string Selector.node_without_tabindex);
      (* Child button/a elements are not tabbable until the list item is focused *)
      loop_nodes (fun _ item -> Element.set_attribute item "tabindex" "-1")
      @@ super#root##querySelectorAll (Js.string Selector.focusable_child_elements);
      let nodes = self#nodes_ in
      (match nodes##.length with
       | 0 -> ()
       | _ ->
         if has_checkbox_at_index 0 nodes
         then _is_checkbox_tree <- true);

    method! destroy () : unit =
      super#destroy ();
      List.iter Lwt.cancel _listeners;
      _listeners <- []

    method initialize_tree_type () : unit =
      let checkbox_list_items =
        super#root##querySelectorAll (Js.string Selector.aria_role_checkbox) in
      let single_selected_node =
        super#root##querySelector (Js.string Selector.single_selected_node) in
      if checkbox_list_items##.length > 0
      then (
        let preselected_items =
          Element.query_selector_all super#root
            Selector.aria_checked_checkbox in
        _selected_items <- preselected_items)
      else if Js.Opt.test single_selected_node
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
             List.filter (fun x -> Element.has_class x CSS.node)
             @@ Element.children w) in
      aux [] node

    method update_children checked (elt : Dom_html.element Js.t) =
      let children = self#get_node_children elt in
      match children with
      | [] -> ()
      | nodes ->
        print_endline @@ Printf.sprintf "got %d children to update (%B)"
          (List.length nodes) checked;
        List.iter (self#toggle_checkbox ~toggle:checked) nodes

    method update_parent () =
      let rec update () =
        () in
      update ()

    method private nodes_ : Dom_html.element Dom.nodeList Js.t =
      super#root##querySelectorAll (Js.string Selector.nodes)

    method private handle_keydown (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      let items = self#nodes_ in
      match tree_node_of_event items (e :> Dom_html.event Js.t) with
      | None -> Lwt.return_unit
      | Some item ->
        match Js.Opt.to_option Dom_html.document##.activeElement with
        | None -> Lwt.return_unit
        | Some active ->
          let next, stop =
            match Events.Key.of_event e, _is_vertical with
            | `Arrow_down, true |  `Arrow_right, false ->
              prevent_default_event e;
              focus_next_element ~wrap:_wrap_focus active items, false
            | `Arrow_up, true | `Arrow_left, false ->
              prevent_default_event e;
              focus_prev_element ~wrap:_wrap_focus active items, false
            | `Home, _ ->
              prevent_default_event e;
              let first = Js.Opt.to_option (items##item 0) in
              Utils.Option.iter (fun x -> x##focus) first;
              first, false
            | `End, _ ->
              prevent_default_event e;
              let last = Js.Opt.to_option (items##item (items##.length - 1)) in
              Utils.Option.iter (fun x -> x##focus) last;
              last, false
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
                  if self#is_selectable_list
                  then self#set_selected_node_on_action active;
                  (* self#notify_action active; *)
                  None, false))
              else None, false
            | _ -> None, false in
          if not stop
          then (
            match next with
            | None -> ()
            | Some next ->
              set_tab_index ~prev:active items next;
              _focused_item <- Some next);
          Lwt.return_unit

    method private handle_click (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      Utils.Option.iter (fun node ->
          let target = Dom.eventTarget e in
          (* Toggle the checkbox only if it's not the target of the event,
             or the checkbox will have 2 change events. *)
          let toggle = not @@ Element.matches target Selector.checkbox_radio in
          if Element.has_class node CSS.node_leaf || not toggle
          then self#toggle_checkbox ~toggle node
          else (
            if toggle
            then ignore @@ Element.toggle_class node CSS.node_expanded))
      @@ tree_node_of_event self#nodes_ (e :> Dom_html.event Js.t);
      Lwt.return_unit

    method private is_selectable_list : bool = true
      (* _is_single_selection || _is_checkbox_list || _is_radio_list *)

    method private set_selected (items : Dom_html.element Js.t list) : unit =
      self#layout ();
      if _is_checkbox_tree
      then self#set_checkbox items
      else self#set_single_selection_ (List.hd items)

    method private set_single_selection_ (item : Dom_html.element Js.t) : unit =
      List.iter (fun i ->
          if not @@ Element.equal i item then (
            Element.remove_class i CSS.node_selected;
            Element.remove_class i CSS.node_activated)) _selected_items;
      let _class =
        if _use_activated_class
        then CSS.node_activated else CSS.node_selected in
      Element.add_class item _class;
      self#set_aria_for_single_selection item;
      _selected_items <- [item]

    method private set_aria_for_single_selection (item : Dom_html.element Js.t) : unit =
      (* Detect the presence of aria-current and get the value only during list
         initialization when it is in unset state. *)
      (match _selected_items with
       | [] -> _aria_current_value <- Element.get_attribute item Attr.aria_current
       | _ -> ());
      let aria_attribute = match _aria_current_value with
        | None -> Attr.aria_selected
        | Some _ -> Attr.aria_current in
      (match _selected_items with
       | [] -> ()
       | l -> List.iter (fun e -> Element.set_attribute e aria_attribute "false") l);
      let value = match _aria_current_value with
        | None -> "true"
        | Some x -> x in
      Element.set_attribute item aria_attribute value

    method private set_selected_node_on_action ?toggle
        (item : Dom_html.element Js.t) : unit =
      if _is_checkbox_tree
      then self#toggle_checkbox ?toggle item
      else self#set_selected [item]

    method private set_checkbox (selected : Dom_html.element Js.t list) =
      loop_nodes (fun _ (item : Dom_html.element Js.t) ->
          let checked = List.exists (Element.equal item) selected in
          set_node_checked checked item;
          Element.set_attribute item Attr.aria_checked (string_of_bool checked))
        self#nodes_;
      _selected_items <- selected

    method private toggle_checkbox ?(toggle = true)
        (node : Dom_html.element Js.t) : unit =
      let checked = not @@ is_node_checked node in
      print_endline @@ Printf.sprintf "checked: %B" checked;
      if toggle then set_node_checked checked node;
      self#update_children (not toggle || checked) node
      (* if checked
       * then _selected_items <- List.add_nodup ~eq:Element.equal item _selected_items
       * else _selected_items <- List.remove ~eq:Element.equal item _selected_items *)

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
