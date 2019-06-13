open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Select
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let flip f x y = f y x

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.( >>= )

type 'a custom_validation =
  { of_string : string -> ('a, string) result
  ; to_string : 'a -> string
  }

type 'a validation =
  | Integer : int validation
  | Float : float validation
  | Text : string validation
  | Custom : 'a custom_validation -> 'a validation

let valid_to_string (type a) (v : a validation) (x : a) : string =
  match v with
  | Integer -> string_of_int x
  | Float -> Printf.sprintf "%g" x
  | Text -> x
  | Custom c -> c.to_string x

let parse_valid (type a) (v : a validation) (s : string) : a option =
  match v with
  | Integer -> int_of_string_opt s
  | Float -> float_of_string_opt s
  | Text -> Some s
  | Custom c ->
    match c.of_string s with
    | Ok x -> Some x
    | Error _ -> None

module Attr = struct
  let required = "required"
  let aria_controls = "aria-controls"
  let aria_required = "aria-required"
  let aria_expanded = "aria-expanded"
  let aria_invalid = "aria-invalid"
  let aria_disabled = "aria-disabled"
  let aria_selected = "aria-selected"
  let enhanced_value = "data-value"

  let validation_attr_whitelist = [required; aria_required]
end

module Const = struct
  let label_scale = 0.75
end

module Selector = struct
  let hidden_input = "input[type=\"hidden\"]"
  let label = "." ^ Floating_label.CSS.root
  let leading_icon = "." ^ CSS.icon
  let line_ripple = "." ^ Line_ripple.CSS.root
  let menu = "." ^ CSS.menu
  let native_control = "." ^ CSS.native_control
  let outline = "." ^ Notched_outline.CSS.root
  let selected_item = "." ^ Item_list.CSS.item_selected
  let selected_text = "." ^ CSS.selected_text
end

module Event = struct
  class type change =
    object
      method index : int option Js.readonly_prop
      method value : string Js.readonly_prop
    end

  let icon : unit Widget.custom_event Js.t Events.Typ.t =
    Events.Typ.make "select:icon"

  let change : change Js.t Widget.custom_event Js.t Events.Typ.typ =
    Events.Typ.make "select:change"
end

module Icon = struct
  module Attr = struct
    let icon_role = "button"
    let aria_label = "aria-label"
  end

  (* XXX Should be inherited from Icon? *)
  class t (elt : Dom_html.element Js.t) () =
    object(self)
      val mutable _saved_tab_index = None
      val mutable _click_listener = None
      val mutable _keydown_listener = None
      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        _saved_tab_index <- Element.get_attribute super#root "tabindex";
        (* Attach event listeners *)
        let click = Events.clicks super#root self#handle_click in
        let keydown = Events.keydowns super#root self#handle_keydown in
        _click_listener <- Some click;
        _keydown_listener <- Some keydown

      method! destroy () : unit =
        super#destroy ();
        (* Detach event listeners *)
        Option.iter Lwt.cancel _click_listener;
        Option.iter Lwt.cancel _keydown_listener;
        _click_listener <- None;
        _keydown_listener <- None

      method set_aria_label (label : string) : unit =
        Element.set_attribute super#root Attr.aria_label label

      method set_disabled (x : bool) : unit =
        match _saved_tab_index with
        | None -> ()
        | Some tabindex ->
          if x then (
            Element.set_attribute super#root "tabindex" "-1";
            Element.remove_attribute super#root "role")
          else (
            Element.set_attribute super#root "tabindex" tabindex;
            Element.set_attribute super#root "role" Attr.icon_role)

      (* Private methods *)

      method private notify_action () : unit =
        super#emit ~should_bubble:true Event.icon

      method private handle_keydown e _ : unit Lwt.t =
        (match Events.Key.of_event e with
         | `Enter -> self#notify_action ()
         | _ -> ());
        Lwt.return_unit

      method private handle_click _ _  : unit Lwt.t =
        self#notify_action ();
        Lwt.return_unit
    end

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()

end

module Helper_text = struct

  module Attr = struct
    let aria_hidden = "aria-hidden"
  end

  class t (elt : Dom_html.element Js.t) () =
    object(self)
      inherit Widget.t elt () as super

      method set_content (s : string) : unit =
        super#root##.textContent := Js.some @@ Js.string s

      method persistent : bool =
        super#has_class CSS.Helper_text.persistent

      method set_persistent (x : bool) : unit =
        super#toggle_class ~force:x CSS.Helper_text.persistent

      method validation : bool =
        super#has_class CSS.Helper_text.validation_msg

      method set_validation (x : bool) : unit =
        super#toggle_class ~force:x CSS.Helper_text.validation_msg

      method show_to_screen_reader () : unit =
        super#remove_attribute Attr.aria_hidden

      method set_validity (is_valid : bool) : unit =
        let needs_display = self#validation && not is_valid in
        if needs_display
        then super#set_attribute "role" "alert"
        else super#remove_attribute "role";
        if not self#persistent && not needs_display
        then self#hide ()

      method private hide () : unit =
        super#set_attribute Attr.aria_hidden "true"

    end

  let make ?persistent ?validation text : t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_div
      @@ Markup.Helper_text.create ?persistent ?validation ~text () in
    new t elt ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()

end

type target =
  | Native of Dom_html.selectElement Js.t
  | Enhanced of { text : Dom_html.element Js.t
                ; menu : Menu.t
                ; hidden_input : Dom_html.inputElement Js.t option
                }

let target_element = function
  | Native x -> Element.coerce x
  | Enhanced { text; _ } -> text

class ['a] t
    ?(on_change : ('a t -> unit) option)
    ?(line_ripple : Line_ripple.t option)
    ?(floating_label : Floating_label.t option)
    ?(notched_outline : Notched_outline.t option)
    ?(helper_text : Helper_text.t option)
    ?(validation : 'a validation option)
    (elt : Dom_html.element Js.t) () =
  let native_control : Dom_html.selectElement Js.t option =
    Option.map (fun x ->
        match Js.to_string x##.tagName with
        | "SELECT" -> Js.Unsafe.coerce x
        | s ->
          let err = Printf.sprintf
              "select: native control should have a `select` tag, \
               but got `%s`" (String.lowercase_ascii s) in
          failwith err)
    @@ Element.query_selector elt Selector.native_control in
  let selected_text = Element.query_selector elt Selector.selected_text in
  let target = match native_control, selected_text with
    | Some x, _ -> Native x
    | None, Some text ->
      let is_disabled = Element.has_class elt CSS.disabled in
      Element.set_attribute text "tabindex" (if is_disabled then "-1" else "0");
      let hidden_input =
        Option.map (fun x ->
            match Js.to_string x##.tagName with
            | "INPUT" -> Js.Unsafe.coerce x
            | s ->
              let err = Printf.sprintf
                  "select: hidden input should have an `input` tag, \
                   but got `%s`" (String.lowercase_ascii s) in
              failwith err)
        @@ Element.query_selector elt Selector.hidden_input in
      let menu_elt = Element.query_selector_exn elt Selector.menu in
      let menu = Menu.attach menu_elt in
      menu#hoist_menu_to_body ();
      menu#set_anchor_element elt;
      menu#set_anchor_corner Bottom_start;
      menu#set_wrap_focus false;
      Enhanced { text; menu; hidden_input }
    | None, None ->
      let err =
        Printf.sprintf "select: missing required element: one of the following \
                        selectors must be present: %s or %s"
          CSS.native_control CSS.selected_text in
      failwith err in
  object(self)

    val line_ripple : Line_ripple.t option =
      match line_ripple with
      | Some x -> Some x
      | None ->
        Option.map Line_ripple.attach
        @@ Element.query_selector elt Selector.line_ripple

    val notched_outline : Notched_outline.t option =
      match notched_outline with
      | Some x -> Some x
      | None ->
        Option.map Notched_outline.attach
        @@ Element.query_selector elt Selector.outline

    val floating_label : Floating_label.t option =
      match floating_label with
      | Some x -> Some x
      | None ->
        Option.map Floating_label.attach
        @@ Element.query_selector elt Selector.label

    val leading_icon =
      Option.map (fun x ->
          Element.add_class elt CSS.with_leading_icon;
          (match target with
           | Enhanced { menu; _ } -> menu#add_class CSS.with_leading_icon
           | Native _ -> ());
          Icon.attach x)
      @@ Element.query_selector elt Selector.leading_icon

    val helper_text =
      match helper_text with
      | Some x -> Some x
      | None ->
        match Element.get_attribute (target_element target) Attr.aria_controls with
        | None -> None
        | Some id -> Option.map Helper_text.attach @@ Dom_html.getElementById_opt id

    (* Event listeners *)
    val mutable _focus_listener = None
    val mutable _blur_listener = None
    val mutable _change_listener = None
    val mutable _click_listener = None
    val mutable _keydown_listener = None
    val mutable _opened_listener = None
    val mutable _closed_listener = None
    val mutable _selected_listener = None

    val mutable _is_menu_open = false
    val mutable _selected_index = None
    val mutable _validation_observer = None
    val mutable _ripple : Ripple.t option = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      if not @@ super#has_class CSS.outlined
      then _ripple <- Some (self#create_ripple ());
      (* The required state need to be sync'd before the mutation observer is added *)
      self#initial_sync_required_state ();
      self#add_mutation_observer_for_required ()

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      (* Attach event listeners *)
      let change = Events.changes self#target_element (fun _ _ ->
          self#handle_change ~did_change:true ();
          Lwt.return_unit) in
      let focus = Events.focuses self#target_element self#handle_focus in
      let blur = Events.blurs self#target_element self#handle_blur in
      let click = Events.clicks self#target_element self#handle_click in
      _change_listener <- Some change;
      _focus_listener <- Some focus;
      _blur_listener <- Some blur;
      _click_listener <- Some click;
      (match target with
       | Native _ -> ()
       | Enhanced { text; menu; hidden_input } ->
         let keydown = Events.keydowns text self#handle_keydown in
         let closed = Events.listen_lwt menu#root Menu_surface.Event.closed (fun e t ->
             self#handle_menu_closed e t
             >>= fun () ->
             (* _is_menu_open is used to track the state of the menu opening
                or closing since the menu#reveal function will return false
                if the menu is still closing and this method listens to the
                closed event which occurs after the menu is already closed. *)
             _is_menu_open <- false;
             Element.remove_attribute text Attr.aria_expanded;
             if Dom_html.document##.activeElement != Js.some text
             then self#handle_blur (e :> Dom_html.event Js.t) t
             else Lwt.return_unit) in
         let opened = Events.listen_lwt menu#root Menu_surface.Event.opened (fun e t ->
             self#handle_menu_opened e t
             >>= fun () ->
             match menu#items with
             | [] -> Lwt.return_unit
             | items ->
               (* Menu should open to the last selected element, should open to
                  first menu item otherwise *)
               let focus_index = match _selected_index with
                 | None -> 0
                 | Some x when x < 0 -> 0
                 | Some x -> x in
               (match List.nth_opt items focus_index with
                | None -> ()
                | Some x -> x##focus);
               Lwt.return_unit) in
         let selected = Events.listen_lwt menu#root Menu.Event.selected (fun e _ ->
             (match Js.Opt.to_option e##.detail with
              | None -> ()
              | Some d -> _selected_index <- Some d##.index);
             Lwt.return_unit) in
         _keydown_listener <- Some keydown;
         _opened_listener <- Some opened;
         _closed_listener <- Some closed;
         _selected_listener <- Some selected;
         match hidden_input,
               Option.map (fun x -> Js.to_string x##.value) hidden_input,
               Element.query_selector menu#root Selector.selected_item with
         (* If the hidden input already has a value, use it to restore the
            select's value. This can happen e.g. if the user goes back or
            (in some browsers) refreshes the page. *)
         | None, Some s, _ when String.length s > 0 -> self#set_value_ s
         (* If an element is selected, the select should set the initial selected text. *)
         | _, _, Some _ -> self#set_value_as_string self#value_as_string
         | _ -> ());
      self#handle_change ~did_change:false (); (* Initially sync floating label *)
      match super#has_class CSS.disabled, self#native_control with
      | true, _ -> self#set_disabled true
      | _, Some x when x##.disabled = Js._true -> self#set_disabled true
      | _ -> ()

    method! layout () : unit =
      super#layout ();
      self#handle_change ~did_change:false ()

    method! destroy () : unit =
      super#destroy ();
      (* Detach event listeners *)
      Option.iter Lwt.cancel _change_listener;
      Option.iter Lwt.cancel _focus_listener;
      Option.iter Lwt.cancel _blur_listener;
      Option.iter Lwt.cancel _keydown_listener;
      Option.iter Lwt.cancel _click_listener;
      Option.iter Lwt.cancel _closed_listener;
      Option.iter Lwt.cancel _opened_listener;
      Option.iter Lwt.cancel _selected_listener;
      _focus_listener <- None;
      _blur_listener <- None;
      _change_listener <- None;
      _click_listener <- None;
      _closed_listener <- None;
      _opened_listener <- None;
      _selected_listener <- None;
      (* Destroy internal components *)
      Option.iter Ripple.destroy _ripple;
      Option.iter Widget.destroy notched_outline;
      Option.iter Widget.destroy line_ripple;
      Option.iter Widget.destroy floating_label;
      Option.iter Widget.destroy leading_icon;
      Option.iter Widget.destroy helper_text;
      (match target with
       | Native _ -> ()
       | Enhanced { menu; _ } -> menu#destroy ());
      (* Destroy other objects *)
      Option.iter (fun x -> x##disconnect) _validation_observer;
      _validation_observer <- None;

    method set_helper_text_content (s : string) : unit =
      Option.iter (fun x -> x#set_content s) helper_text

    method selected_index : int option =
      match target with
      | Native x ->
        let idx = x##.selectedIndex in
        if idx >= 0 then Some idx else None
      | Enhanced { menu; _ } ->
        let elt = Element.query_selector menu#root Selector.selected_item in
        match elt with
        | None -> None
        | Some elt ->
          (* XXX maybe just read menu#selected? *)
          List.find_mapi (fun i item ->
              if Element.equal elt item then Some i else None) menu#items

    method set_selected_index (i : int) : unit =
      self#set_selected_index_ i;
      self#close_menu ();
      self#handle_change ~did_change:true ();

    method required : bool =
      match target with
      | Native x -> Js.to_bool (Js.Unsafe.coerce x)##.required
      | Enhanced { text; _ } ->
        match Element.get_attribute text Attr.aria_required with
        | Some "true" -> true
        | _ -> false

    method set_required (is_required : bool) : unit =
      match target with
      | Native x -> x##.required := Js.bool is_required
      | Enhanced { text; _ } ->
        if is_required
        then Element.set_attribute text Attr.aria_required "true"
        else Element.remove_attribute text Attr.aria_required

    method disabled : bool =
      super#has_class CSS.disabled
      && begin match native_control with
        | None -> false
        | Some x -> Js.to_bool x##.disabled
      end

    method set_disabled (is_disabled : bool) : unit =
      super#toggle_class ~force:is_disabled CSS.disabled;
      (match target with
       | Native x -> x##.disabled := Js.bool is_disabled
       | Enhanced { text; hidden_input; _ } ->
         let tabindex = if is_disabled then "-1" else "0" in
         Element.set_attribute text "tabindex" tabindex;
         Element.set_attribute text Attr.aria_disabled (string_of_bool is_disabled);
         Option.iter (fun x -> x##.disabled := Js.bool is_disabled) hidden_input);
      self#close_menu ();
      match leading_icon with
      | None -> ()
      | Some x -> x#set_disabled is_disabled

    method value_as_string : string =
      match target with
      | Native x -> Js.to_string x##.value
      | Enhanced { menu; _ } ->
        match Element.query_selector menu#root Selector.selected_item with
        | None -> ""
        | Some item ->
          match Element.get_attribute item Attr.enhanced_value with
          | None -> ""
          | Some s -> s

    method value : 'a option =
      match validation with
      | None -> None
      | Some validation ->
        parse_valid validation self#value_as_string

    method set_value_as_string (v : string) : unit =
      self#set_value_ v;
      self#handle_change ~did_change:true ()

    method set_value (v : 'a) =
      match validation with
      | None -> failwith "select: type validation is not set"
      | Some validation ->
        let v' = valid_to_string validation v in
        self#set_value_as_string v'

    method is_valid : bool =
      match target with
      | Native x -> Js.to_bool (Js.Unsafe.coerce x)##checkValidity
      | Enhanced _ ->
        if super#has_class CSS.required
        && not @@ super#has_class CSS.disabled
        then begin match _selected_index, self#value_as_string with
          | None, _ -> false
          | Some x, "" when x > 0 -> false
          | _ -> true
        end
        else true

    method set_valid (is_valid : bool) : unit =
      (match target with
       | Native _ -> ()
       | Enhanced { text; _ } ->
         Element.set_attribute text Attr.aria_invalid
         @@ string_of_bool (not is_valid));
      super#toggle_class ~force:(not is_valid) CSS.invalid

    method clear () : unit =
      match target with
      | Native elt -> Element.remove_children elt
      | _ -> (* TODO implement *) ()

    (* TODO make this class parameterized *)
    method append_item : 'a. (#Dom_html.element as 'a) Js.t -> unit = fun item ->
      match target with
      | Native elt -> Dom.appendChild elt item
      | Enhanced _ -> (* TODO implement *) ()

    method! append_child child =
      match target with
      | Native elt -> Dom.appendChild elt child#node
      | Enhanced _ -> (* TODO implement *) ()

    (* Private methods. *)

    method private native_control : Dom_html.selectElement Js.t option =
      native_control

    method private target_element =
      target_element target

    method private handle_menu_opened _ _ : unit Lwt.t =
      super#add_class CSS.activated;
      Lwt.return_unit

    method private handle_menu_closed _ _ : unit Lwt.t =
      super#remove_class CSS.activated;
      Lwt.return_unit

    method private handle_change ?(did_change = true) () : unit =
      let value = self#value_as_string in
      let has_value = String.length value > 0 in
      self#notch_outline has_value;
      if not @@ super#has_class CSS.focused
      then Option.iter (flip Floating_label.float has_value) floating_label;
      if did_change
      then (
        let detail = object%js
          val index = self#selected_index
          val value = value
        end in
        super#emit ~should_bubble:true ~detail Event.change;
        Option.iter (fun f -> f (self :> 'a t)) on_change;
        let is_valid = self#is_valid in
        if super#has_class CSS.required
        then (
          self#set_valid is_valid;
          Option.iter (fun x -> x#set_validity is_valid) helper_text))

    method private handle_focus _ _ : unit Lwt.t =
      super#add_class CSS.focused;
      Option.iter (flip Floating_label.float true) floating_label;
      self#notch_outline true;
      Option.iter Line_ripple.activate line_ripple;
      Option.iter (fun x -> x#show_to_screen_reader ()) helper_text;
      Lwt.return_unit

    method private handle_blur _ _ : unit Lwt.t =
      if not self#is_menu_open then (
        super#remove_class CSS.focused;
        self#handle_change ~did_change:false ();
        Option.iter Line_ripple.deactivate line_ripple;
        if super#has_class CSS.required
        then (
          let is_valid = self#is_valid in
          self#set_valid is_valid;
          Option.iter (fun x -> x#set_validity is_valid) helper_text));
      Lwt.return_unit

    method private handle_click (evt : Dom_html.mouseEvent Js.t) _ : unit Lwt.t =
      Option.iter (fun x -> x##focus) selected_text;
      if not self#is_menu_open then (
        let rect = (Dom_html.eventTarget evt)##getBoundingClientRect in
        let client_x = match Js.Optdef.test (Js.Unsafe.coerce evt)##.touches with
          | true ->
            let (evt : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce evt in
            let touch = (evt##.touches##item 0) in
            Js.Optdef.case touch (fun () -> 0) (fun touch -> touch##.clientX)
          | false -> evt##.clientX in
        let normalized = float_of_int client_x -. rect##.left in
        self#set_ripple_center normalized;
        self#open_menu ())
      else Lwt.return_unit

    method private handle_keydown (event : Dom_html.keyboardEvent Js.t) _ : unit Lwt.t =
      match Events.Key.of_event event with
      | `Enter | `Space | `Arrow_up | `Arrow_down ->
        if not @@ super#has_class CSS.focused then Lwt.return_unit
        else (Dom.preventDefault event; self#open_menu ())
      | _ -> Lwt.return_unit

    method private is_menu_open : bool =
      match target with
      | Native _ -> false
      | Enhanced { menu; _ } -> menu#is_open

    method private close_menu () : unit =
      match target with
      | Native _ -> ()
      | Enhanced { menu; _ } -> Lwt.async menu#close

    method private open_menu () : unit Lwt.t =
      match target with
      | Native _ -> Lwt.return_unit
      | Enhanced { menu; text; _ } ->
        _is_menu_open <- true;
        Element.set_attribute text Attr.aria_expanded "true";
        menu#reveal ()

    method private set_value_ v =
      match target with
      | Native x -> x##.value := Js.string v
      | Enhanced { menu; _ } ->
        let selector = Printf.sprintf "[%s=%s]" Attr.enhanced_value v in
        let element = Element.query_selector menu#root selector in
        let index = match element with
          | None -> -1
          | Some item ->
            Option.get
            @@ List.find_mapi (fun i x ->
                if Element.equal item x then Some i else None) menu#items in
        self#set_enhanced_selected_index index

    method private set_selected_index_ (i : int) : unit =
      match target with
      | Native x -> x##.selectedIndex := i
      | Enhanced _ -> self#set_enhanced_selected_index i

    method private notch_outline (open_notch : bool) : unit =
      Option.iter (fun (outline : Notched_outline.t) ->
          if open_notch
          then
            let label_scale = Const.label_scale in
            let label_width = match floating_label with
              | None -> 0
              | Some (label : Floating_label.t) -> label#width in
            outline#notch (float_of_int label_width *. label_scale)
          else if not @@ super#has_class CSS.focused
          then outline#close_notch ())
        notched_outline

    method private set_enhanced_selected_index (i : int) =
      match target with
      | Native _ -> ()
      | Enhanced { menu; hidden_input; _ } ->
        let selected = List.nth_opt menu#items i in
        Option.iter (fun x ->
            Element.remove_class x Item_list.CSS.item_selected;
            Element.remove_attribute x Attr.aria_selected)
        @@ Element.query_selector menu#root Selector.selected_item;
        Option.iter (fun x ->
            Element.add_class x Item_list.CSS.item_selected;
            Element.set_attribute x Attr.aria_selected "true")
          selected;
        (* Synchronize hidden input's value with data-value attribute of selected item.
           This code path is also followed when setting value directly, so this covers
           all cases. *)
        Option.iter (fun x ->
            let value = match selected with
              | None -> ""
              | Some item ->
                match Element.get_attribute item Attr.enhanced_value with
                | None -> ""
                | Some v -> v in
            x##.value := Js.string value) hidden_input;
        self#layout ()

    method private initial_sync_required_state () =
      let is_required =
        (match target with
         | Native x -> Js.to_bool (Js.Unsafe.coerce x)##.required
         | Enhanced _ -> false)
        || (match Element.get_attribute self#target_element Attr.aria_required with
            | Some "true" -> true | _ -> false)
        || super#has_class CSS.required in
      if is_required
      then (
        (match target with
         | Native x -> x##.required := Js._true
         | Enhanced { text; _ } ->
           Element.set_attribute text Attr.aria_required "true");
        super#add_class CSS.required)

    method private add_mutation_observer_for_required () =
      let open MutationObserver in
      let handler = fun attr_list ->
        List.iter (fun (attr : string) ->
            if List.exists (String.equal attr) Attr.validation_attr_whitelist
            then (
              match target with
              | Native x ->
                if Js.to_bool (Js.Unsafe.coerce x)##.required
                then super#add_class CSS.required
                else super#remove_class CSS.required
              | Enhanced { text; _ } ->
                match Element.get_attribute text Attr.aria_required with
                | Some "true" -> super#add_class CSS.required
                | _ -> super#remove_class CSS.required)) attr_list in
      let get_attributes_list (mutations : mutationRecord Js.t Js.js_array Js.t) =
        List.filter_map (fun m ->
            match Js.Opt.to_option @@ m##.attributeName with
            | None -> None
            | Some x -> Some (Js.to_string x))
        @@ Array.to_list
        @@ Js.to_array mutations in
      let observer =
        MutationObserver.observe
          ~node:self#target_element
          ~attributes:true
          ~f:(fun x _ -> handler @@ get_attributes_list x)
          () in
      _validation_observer <- Some observer

    method private set_ripple_center (x : float) =
      Option.iter (fun (line_ripple : Line_ripple.t) ->
          line_ripple#set_ripple_center x) line_ripple

    method private create_ripple () : Ripple.t =
      let adapter = Ripple.make_default_adapter super#root in
      let adapter = { adapter with event_target = self#target_element } in
      new Ripple.t adapter ()

  end

let make ?on_change ?disabled ?label
    ?(outlined = false)
    ?(icon : #Widget.t option)
    ?(helper_text : Helper_text.t option)
    ?hidden_input
    ?value
    typ
    validation : 'a t =
  Option.iter (fun x -> x#add_class CSS.icon) icon;
  let floating_label = match label with
    | None -> None
    | Some label -> Some (Floating_label.make label) in
  let notched_outline = match outlined with
    | false -> None
    | true -> Some (Notched_outline.make ?label:floating_label ()) in
  let line_ripple = match notched_outline with
    | Some _ -> None
    | None -> Some (Line_ripple.make ()) in
  (* Should we include label to the core component? *)
  let label = match notched_outline with
    | Some _ -> None
    | None -> floating_label in
  let elt = match typ with
    | `Native items ->
      let select =
        Markup.Native.create_select ?disabled ~items () in
      Tyxml_js.To_dom.of_div
      @@ Markup.Native.create ?disabled
        ?line_ripple:(Option.map Widget.to_markup line_ripple)
        ?icon:(Option.map Widget.to_markup icon)
        ?label:(Option.map Widget.to_markup label)
        ?outline:(Option.map Widget.to_markup notched_outline)
        ~select
        ()
    | `Enhanced menu ->
      Tyxml_js.To_dom.of_div
      @@ Markup.Enhanced.create ?disabled
        ?line_ripple:(Option.map Widget.to_markup line_ripple)
        ?icon:(Option.map Widget.to_markup icon)
        ?label:(Option.map Widget.to_markup label)
        ?outline:(Option.map Widget.to_markup notched_outline)
        ?hidden_input
        ~menu
        () in
  let t = new t ?on_change ?helper_text ?line_ripple ?notched_outline
    ?floating_label ~validation elt () in
  Option.iter (fun x -> t#set_value x) value;
  t

let native_options_of_values
    (type a)
    ?(with_empty = false)
    ?(label : (a -> string) option)
    (validation : a validation)
    (values : a list) =
  let label = match label with
    | None -> valid_to_string validation
    | Some f -> f in
  let options =
    List.map (fun (x : a) ->
        Markup.Native.create_option
          ~value:(valid_to_string validation x)
          ~text:(label x)
          ())
      values in
  if not with_empty then options else
    let empty = Markup.Native.create_option ~selected:true ~disabled:true ~text:"" () in
    empty :: options

type ('a, 'b) items =
  [ `Markup of 'a Tyxml_js.Html.elt list
  | `Data of ('b * string option) list
  ]

let make_native ?on_change ?disabled ?label
    ?outlined
    ?(icon : #Widget.t option)
    ?(helper_text : Helper_text.t option)
    ?value
    ~(items : ('b, 'a) items)
    validation : 'a t =
  let items = match items with
    | `Markup x -> `Native x
    | `Data l ->
      let items = List.map (fun (v, s) ->
          let value = valid_to_string validation v in
          let text = match s with
            | None -> value
            | Some s -> s in
          Markup.Native.create_option ~text ~value ()) l in
      `Native items in
  make ?on_change ?disabled ?label ?outlined ?icon ?helper_text ?value
    items validation

let make_enhanced ?on_change ?disabled ?label
    ?outlined
    ?(icon : #Widget.t option)
    ?(helper_text : Helper_text.t option)
    ?value
    ~menu
    validation : 'a t =
  make ?on_change ?disabled ?label ?outlined ?icon ?helper_text ?value
    (`Enhanced menu) validation

let attach ?helper_text ?validation (elt : #Dom_html.element Js.t) : 'a t =
  new t ?helper_text ?validation (Element.coerce elt) ()
