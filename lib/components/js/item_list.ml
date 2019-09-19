open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Item_list
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

module Attr = struct
  let aria_checked = "aria-checked"

  let aria_current = "aria-current"

  let aria_orientation = "aria-orientation"

  let aria_selected = "aria-selected"
end

module Selector = struct
  let enabled_items = Printf.sprintf ".%s:not(.%s)" CSS.item CSS.item_disabled

  let single_selected_item =
    Printf.sprintf ".%s, .%s" CSS.item_activated CSS.item_selected

  let aria_role_checkbox = "[role=\"checkbox\"]"

  let aria_checked_checkbox = "[role=\"checkbox\"][aria-checked=\"true\"]"

  let aria_checked_radio = "[role=\"radio\"][aria-checked=\"true\"]"

  let radio = "input[type=\"radio\"]:not(:disabled)"

  let checkbox = "input[type=\"checkbox\"]:not(:disabled)"

  let checkbox_radio = Printf.sprintf "%s, %s" checkbox radio

  let item_without_tabindex = Printf.sprintf ".%s:not([tabindex])" CSS.item

  let focusable_child_elements =
    Printf.sprintf
      ".%s button:not(:disabled), .%s a, .%s input[type=\"radio\"]:not(:disabled), .%s \
       input[type=\"checkbox\"]:not(:disabled)"
      CSS.item
      CSS.item
      CSS.item
      CSS.item
end

module Event = struct
  class type detail =
    object
      method item : Dom_html.element Js.t Js.readonly_prop

      method originalEvent : Dom_html.event Js.t Js.readonly_prop
    end

  let (action : detail Js.t Dom_html.customEvent Js.t Dom_html.Event.typ) =
    Dom_html.Event.make (CSS.root ^ ":action")
end

module Lwt_js_events = struct
  open Js_of_ocaml_lwt.Lwt_js_events

  let action ?use_capture ?passive t = make_event ?use_capture ?passive Event.action t

  let actions ?cancel_handler ?use_capture ?passive t =
    seq_loop ?cancel_handler ?use_capture ?passive action t
end

let elements_key_allowed_in = ["input"; "button"; "textarea"; "select"]

let get_exn (i : int) (list : Dom_html.element Dom.nodeList Js.t) =
  Js.Opt.get (list##item i) (fun () -> assert false)

let index (elt : #Dom.node Js.t) =
  let rec aux i node =
    match Js.Opt.to_option node##.previousSibling with
    | None -> i
    | Some x -> aux (succ i) x
  in
  aux 0 (elt :> Dom.node Js.t)

let loop_nodes f (list : Dom_html.element Dom.nodeList Js.t) =
  let length = list##.length in
  let rec loop = function
    | x when x = length -> ()
    | i ->
        f i (get_exn i list);
        loop (succ i)
  in
  loop 0

let prevent_default_event (e : #Dom_html.event Js.t) : unit =
  Js.Opt.iter e##.target (fun (elt : Dom_html.element Js.t) ->
      if not
         @@ List.exists
              (String.equal (Js.to_string elt##.tagName##toLowerCase))
              elements_key_allowed_in
      then Dom.preventDefault e)

let list_item_of_event
    (items : Dom_html.element Dom.nodeList Js.t)
    (e : Dom_html.event Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option
  @@ Js.Opt.bind e##.target (fun (target : Dom_html.element Js.t) ->
         let selector = Printf.sprintf ".%s, .%s" CSS.item CSS.root in
         let nearest_parent = Element.closest target selector in
         Js.Opt.bind nearest_parent (fun (parent : Dom_html.element Js.t) ->
             if not @@ Element.matches parent ("." ^ CSS.item)
             then Js.null
             else Element.find (Element.equal parent) items))

let set_tab_index_for_list_item_children (index : int) (item : Dom_html.element Js.t) :
    unit =
  List.iter (fun (elt : Dom_html.element Js.t) ->
      Element.set_attribute elt "tabindex" (string_of_int index))
  @@ Element.query_selector_all item
  @@ Printf.sprintf ".%s button:not(:disabled), .%s a" CSS.item CSS.item

let focus_prev_element
    ?(wrap = false)
    (active : Dom_html.element Js.t)
    (items : Dom_html.element Dom.nodeList Js.t) =
  let length = items##.length in
  let rec aux = function
    | i when i = length || length = 1 -> None
    (* check if first item is focused *)
    | i when i = 0 ->
        let item = get_exn i items in
        if not @@ Element.equal item active
        then aux (succ i)
        else if not wrap
        then None
        else
          let prev = Js.Opt.to_option @@ items##item (length - 1) in
          Option.iter (fun x -> x##focus) prev;
          prev
    | i ->
        let item = get_exn i items in
        if not @@ Element.equal item active
        then aux (succ i)
        else
          let prev = get_exn (pred i) items in
          prev##focus;
          Some prev
  in
  aux 0

let focus_next_element
    ?(wrap = false)
    (active : Dom_html.element Js.t)
    (items : Dom_html.element Dom.nodeList Js.t) =
  let length = items##.length in
  let rec aux = function
    | i when i = length || length = 1 -> None
    (* check if last item is focused *)
    | i when i = length - 1 ->
        let item = get_exn i items in
        if not @@ Element.equal item active
        then aux (succ i)
        else if not wrap
        then None
        else
          let next = Js.Opt.to_option @@ items##item 0 in
          Option.iter (fun x -> x##focus) next;
          next
    | i ->
        let item = get_exn i items in
        if not @@ Element.equal item active
        then aux (succ i)
        else
          let next = get_exn (succ i) items in
          next##focus;
          Some next
  in
  aux 0

let has_radio_at_index (i : int) (items : Dom_html.element Dom.nodeList Js.t) : bool =
  Js.Opt.(
    test
    @@ bind
         (items##item i)
         (fun (item : Dom_html.element Js.t) ->
           item##querySelector (Js.string Selector.radio)))

let set_item_checked (x : bool) (item : Dom_html.element Js.t) : unit =
  match Element.query_selector item Selector.checkbox_radio with
  | None -> ()
  | Some elt ->
      let (input : Dom_html.inputElement Js.t) = Js.Unsafe.coerce elt in
      input##.checked := Js.bool x;
      let event =
        (Js.Unsafe.coerce Dom_html.document)##createEvent (Js.string "Event")
      in
      ignore @@ event##initEvent (Js.string "change") Js._true Js._true;
      (Js.Unsafe.coerce input)##dispatchEvent event

let has_checkbox_at_index (i : int) (items : Dom_html.element Dom.nodeList Js.t) : bool =
  Js.Opt.(
    test
    @@ bind
         (items##item i)
         (fun (item : Dom_html.element Js.t) ->
           item##querySelector (Js.string Selector.checkbox)))

let is_item_checked (item : Dom_html.element Js.t) : bool =
  match Element.query_selector item Selector.checkbox with
  | None -> false
  | Some x ->
      let (checkbox : Dom_html.inputElement Js.t) = Js.Unsafe.coerce x in
      Js.to_bool checkbox##.checked

let set_tab_index
    ?prev
    (items : Dom_html.element Dom.nodeList Js.t)
    (item : Dom_html.element Js.t) : unit =
  let set (i : int) (elt : Dom_html.element Js.t) =
    elt##setAttribute (Js.string "tabindex") (Js.string (string_of_int i))
  in
  (match prev with
  | None ->
      (* If no list item was selected, set first list item's tabindex to -1.
        Generally, tabindex is set to 0 on first list item of list that has
        no preselected items *)
      Js.Opt.iter
        (items##item 0)
        (fun first -> if not @@ Element.equal first item then set (-1) first)
  | Some prev -> if not @@ Element.equal item prev then set (-1) prev);
  set 0 item

let set_tab_index_to_first_selected_item ~selected items : unit =
  match selected with
  | [] -> ()
  | [x] -> set_tab_index items x
  | l ->
      (* XXX seems that getting indexes for every selected item is not very
           efficient. But storing indexes seems wrong too as list items may
           change (added/removed), so the numbering may change too *)
      let _, item =
        List.hd
        @@ List.sort (fun a b -> compare (fst a) (fst b))
        @@ List.map (fun x -> index x, x) l
      in
      set_tab_index items item

module Item = struct
  module Selector = struct
    let text = Printf.sprintf ".%s" CSS.item_text

    let primary_text = "." ^ CSS.item_primary_text

    let secondary_text = "." ^ CSS.item_secondary_text
  end

  class t ?(ripple = false) (elt : Dom_html.element Js.t) () =
    object (self)
      val ripple_ : Ripple.t option =
        if not ripple then None else Some (Ripple.attach elt)

      inherit Widget.t elt () as super

      method! layout () : unit =
        Option.iter Ripple.layout ripple_;
        super#layout ()

      method activated : bool = self#has_class CSS.item_activated

      method selected : bool = self#has_class CSS.item_selected

      method ripple : Ripple.t option = ripple_
    end

  let attach ?ripple (elt : #Dom_html.element Js.t) : t =
    new t ?ripple (Element.coerce elt) ()

  let make
      ?classes
      ?a
      ?graphic
      ?meta
      ?role
      ?tabindex
      ?activated
      ?selected
      ?checked
      ?primary_text
      ?secondary_text
      ?force_wrap
      ?children
      ?ripple
      () =
    D.list_item
      ?classes
      ?a
      ?graphic
      ?meta
      ?role
      ?tabindex
      ?activated
      ?selected
      ?checked
      ?primary_text
      ?secondary_text
      ?force_wrap
      ?children
      ()
    |> Tyxml_js.To_dom.of_li
    |> attach ?ripple
end

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val mutable _selected_items : Dom_html.element Js.t list = []

    val mutable _is_checkbox_list = false

    val mutable _is_radio_list = false

    val mutable _is_single_selection = false

    val mutable _wrap_focus = false

    val mutable _is_vertical = false

    val mutable _focused_item = None

    val mutable _use_activated_class = false

    val mutable _aria_current_value = None

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      (* Attach event listeners *)
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [ clicks super#root self#handle_click
          ; keydowns super#root self#handle_keydown
          ; seq_loop
              (make_event (Dom_html.Event.make "focusin"))
              super#root
              self#handle_focus_in
          ; seq_loop
              (make_event (Dom_html.Event.make "focusout"))
              super#root
              self#handle_focus_out ]
        @ listeners;
      (* Other initialization *)
      self#layout ();
      self#initialize_list_type ()

    method! destroy () : unit =
      super#destroy ();
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- []

    method! layout () : unit =
      super#layout ();
      (match Element.get_attribute super#root Attr.aria_orientation with
      | Some "horizontal" -> self#set_vertical false
      | _ -> self#set_vertical true);
      (* List items need to have at least tabindex=-1 to be focusable *)
      loop_nodes (fun _ item -> Element.set_attribute item "tabindex" "-1")
      @@ super#root##querySelectorAll (Js.string Selector.item_without_tabindex);
      (* Child button/a elements are not tabbable until the list item is focused *)
      loop_nodes (fun _ item -> Element.set_attribute item "tabindex" "-1")
      @@ super#root##querySelectorAll (Js.string Selector.focusable_child_elements);
      let items = self#items_ in
      match items##.length with
      | 0 -> ()
      | _ ->
          if has_checkbox_at_index 0 items
          then _is_checkbox_list <- true
          else if has_radio_at_index 0 items
          then _is_radio_list <- true

    method initialize_list_type () : unit =
      let checkbox_list_items =
        super#root##querySelectorAll (Js.string Selector.aria_role_checkbox)
      in
      let single_selected_list_item =
        super#root##querySelector (Js.string Selector.single_selected_item)
      in
      let radio_selected_list_item =
        super#root##querySelector (Js.string Selector.aria_checked_radio)
      in
      if checkbox_list_items##.length > 0
      then
        let preselected_items =
          Element.query_selector_all super#root Selector.aria_checked_checkbox
        in
        _selected_items <- preselected_items
      else if Js.Opt.test single_selected_list_item
      then (
        let item = Js.Opt.get single_selected_list_item (fun () -> assert false) in
        if Element.has_class item CSS.item_activated then self#set_use_activated true;
        self#set_single_selection true;
        _selected_items <- [item])
      else if Js.Opt.test radio_selected_list_item
      then
        let item = Js.Opt.get radio_selected_list_item (fun () -> assert false) in
        _selected_items <- [item]

    method wrap_focus : bool = _wrap_focus

    method set_wrap_focus (x : bool) : unit = _wrap_focus <- x

    method vertical : bool = _is_vertical

    method set_vertical (x : bool) : unit = _is_vertical <- x

    method use_activated : bool = _use_activated_class

    method set_use_activated (x : bool) : unit = _use_activated_class <- x

    method single_selection : bool = _is_single_selection

    method set_single_selection (x : bool) : unit = _is_single_selection <- x

    method dense : bool = super#has_class CSS.dense

    method set_dense (x : bool) : unit = super#toggle_class ~force:x CSS.dense

    method non_interactive : bool = super#has_class CSS.non_interactive

    method set_non_interactive (x : bool) : unit =
      super#toggle_class ~force:x CSS.non_interactive

    method selected_elements : Dom_html.element Js.t list = _selected_items

    method selected_indexes : int list = List.map index _selected_items

    method set_selected_item (item : Item.t) : unit = self#set_selected_items [item]

    method set_selected_items (items : Item.t list) : unit =
      self#set_selected @@ List.map Widget.root items

    method set_selected_index (i : int) : unit =
      Js.Opt.iter (self#items_##item i) (fun e -> self#set_selected [e])

    method set_selected_indexes (i : int list) : unit =
      let items = self#items_ in
      let items =
        List.filter_map (fun (i : int) -> Js.Opt.to_option @@ items##item i) i
      in
      self#set_selected items

    method items : Dom_html.element Js.t list = Dom.list_of_nodeList self#items_

    (* Private methods *)
    method private items_ : Dom_html.element Dom.nodeList Js.t =
      super#root##querySelectorAll (Js.string Selector.enabled_items)

    method private is_selectable_list : bool =
      _is_single_selection || _is_checkbox_list || _is_radio_list

    method private set_selected (items : Dom_html.element Js.t list) : unit =
      self#layout ();
      if _is_checkbox_list
      then self#set_checkbox items
      else if _is_radio_list
      then (
        match items with
        | [] -> ()
        | [x] -> self#set_radio x
        | x :: _ ->
            let err =
              "Single item is expected for radio based list, setting first one"
            in
            ignore @@ Js.Unsafe.global##.console##error (Js.string err);
            self#set_radio x)
      else self#set_single_selection_ (List.hd items)

    method private set_single_selection_ (item : Dom_html.element Js.t) : unit =
      List.iter
        (fun i ->
          if not @@ Element.equal i item
          then (
            Element.remove_class i CSS.item_selected;
            Element.remove_class i CSS.item_activated))
        _selected_items;
      let _class =
        if _use_activated_class then CSS.item_activated else CSS.item_selected
      in
      Element.add_class item _class;
      self#set_aria_for_single_selection item;
      _selected_items <- [item]

    method private set_aria_for_single_selection (item : Dom_html.element Js.t) : unit =
      (* Detect the presence of aria-current and get the value only during list
         initialization when it is in unset state. *)
      (match _selected_items with
      | [] -> _aria_current_value <- Element.get_attribute item Attr.aria_current
      | _ -> ());
      let aria_attribute =
        match _aria_current_value with
        | None -> Attr.aria_selected
        | Some _ -> Attr.aria_current
      in
      (match _selected_items with
      | [] -> ()
      | l -> List.iter (fun e -> Element.set_attribute e aria_attribute "false") l);
      let value =
        match _aria_current_value with
        | None -> "true"
        | Some x -> x
      in
      Element.set_attribute item aria_attribute value

    method private set_selected_item_on_action
        ?toggle
        (item : Dom_html.element Js.t)
        : unit =
      if _is_checkbox_list
      then self#toggle_checkbox ?toggle item
      else self#set_selected [item]

    method private notify_action e (item : Dom_html.element Js.t) : unit =
      let (detail : Event.detail Js.t) =
        object%js
          val item = item

          val originalEvent = e
        end
      in
      super#emit ~detail ~should_bubble:true Event.action

    method private handle_keydown
        (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      let items = self#items_ in
      match list_item_of_event items (e :> Dom_html.event Js.t) with
      | None -> Lwt.return_unit
      | Some item -> (
        match Js.Opt.to_option Dom_html.document##.activeElement with
        | None -> Lwt.return_unit
        | Some active ->
            let next, stop =
              match Dom_html.Keyboard_code.of_event e, _is_vertical with
              | ArrowDown, true | ArrowRight, false ->
                  prevent_default_event e;
                  focus_next_element ~wrap:_wrap_focus active items, false
              | ArrowUp, true | ArrowLeft, false ->
                  prevent_default_event e;
                  focus_prev_element ~wrap:_wrap_focus active items, false
              | Home, _ ->
                  prevent_default_event e;
                  let first = Js.Opt.to_option (items##item 0) in
                  Option.iter (fun x -> x##focus) first;
                  first, false
              | End, _ ->
                  prevent_default_event e;
                  let last = Js.Opt.to_option (items##item (items##.length - 1)) in
                  Option.iter (fun x -> x##focus) last;
                  last, false
              | (Enter as k), _ | (Space as k), _ ->
                  if Element.has_class item CSS.item
                  then
                    (* Return early if enter key is pressed on anchor element
                   which triggers synthetic mouseEvent event *)
                    let is_a_tag =
                      Js.Opt.map e##.target (fun e ->
                          String.equal "A" (Js.to_string e##.tagName))
                      |> fun x -> Js.Opt.get x (fun () -> false)
                    in
                    let is_enter =
                      match k with
                      | Enter -> true
                      | _ -> false
                    in
                    if is_a_tag && is_enter
                    then None, true
                    else (
                      prevent_default_event e;
                      if self#is_selectable_list
                      then self#set_selected_item_on_action active;
                      self#notify_action (e :> Dom_html.event Js.t) active;
                      None, false)
                  else None, false
              | _ -> None, false
            in
            (if not stop
            then
              match next with
              | None -> ()
              | Some next ->
                  set_tab_index ~prev:active items next;
                  _focused_item <- Some next);
            Lwt.return_unit)

    method private handle_click
        (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      let items = self#items_ in
      Option.iter (fun item ->
          (* Toggle the checkbox only if it's not the target of the event,
             or the checkbox will have 2 change events. *)
          let target = Dom.eventTarget e in
          let toggle = not @@ Element.matches target Selector.checkbox_radio in
          if self#is_selectable_list then self#set_selected_item_on_action ~toggle item;
          self#notify_action (e :> Dom_html.event Js.t) item;
          set_tab_index ?prev:_focused_item items item;
          _focused_item <- Some item)
      @@ list_item_of_event items (e :> Dom_html.event Js.t);
      Lwt.return_unit

    method private handle_focus_in
        (e : Dom_html.event Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      Option.iter (set_tab_index_for_list_item_children 0)
      @@ list_item_of_event self#items_ e;
      Lwt.return_unit

    method private handle_focus_out
        (e : Dom_html.event Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      let items = self#items_ in
      Option.iter (set_tab_index_for_list_item_children (-1))
      @@ list_item_of_event items e;
      (* Between `focusout` and `focusin` some browsers do not have focus on any
         element. Setting a delay to wait till the focus is moved to next element *)
      Js_of_ocaml_lwt.Lwt_js.yield ()
      >>= fun () ->
      if not @@ Element.is_focus_inside super#root
      then set_tab_index_to_first_selected_item ~selected:_selected_items items;
      Lwt.return_unit

    method private set_radio (selected : Dom_html.element Js.t) =
      set_item_checked true selected;
      List.iter
        (fun e -> Element.set_attribute e Attr.aria_checked "false")
        _selected_items;
      Element.set_attribute selected Attr.aria_checked "true";
      _selected_items <- [selected]

    method private set_checkbox (selected : Dom_html.element Js.t list) =
      loop_nodes
        (fun _ (item : Dom_html.element Js.t) ->
          let checked = List.exists (Element.equal item) selected in
          set_item_checked checked item;
          Element.set_attribute item Attr.aria_checked (string_of_bool checked))
        self#items_;
      _selected_items <- selected

    method private toggle_checkbox ?(toggle = true) (item : Dom_html.element Js.t) : unit
        =
      let checked = not @@ is_item_checked item in
      if toggle then set_item_checked checked item;
      let eq = Element.equal in
      let add x l = if List.exists (eq x) l then l else x :: l in
      if checked
      then _selected_items <- add item _selected_items
      else _selected_items <- List.filter (fun x -> not @@ eq item x) _selected_items
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?avatar_list ?dense ?two_line ?non_interactive ?role ?children () =
  let two_line =
    match two_line, children with
    | (Some _ as x), _ -> x
    | None, None -> None
    | None, Some children ->
        Option.some
        @@ Option.is_some
        @@ List.find_opt
             (fun i ->
               let i = Tyxml_js.To_dom.of_element i in
               let selector = Js.string ("." ^ CSS.item_secondary_text) in
               Js.Opt.test @@ i##querySelector selector)
             children
  in
  D.list ?classes ?a ?avatar_list ?dense ?two_line ?non_interactive ?role ?children ()
  |> Tyxml_js.To_dom.of_ul
  |> attach
