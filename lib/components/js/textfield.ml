open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Textfield
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

(* TODO
   - add 'onchange' callback
   - add custom validation messages when a custom validation was provided
*)
let ( % ) f g x = f (g x)

module Id = struct
  let id_ref = ref (Unix.time () |> int_of_float)

  let get () =
    incr id_ref;
    Printf.sprintf "%s-%d" CSS.root !id_ref
end

module Event = struct
  let icon : unit Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (CSS.root ^ ":icon")
end

module Lwt_js_events = struct
  open Js_of_ocaml_lwt.Lwt_js_events

  let icon ?use_capture ?passive x = make_event ?use_capture ?passive Event.icon x

  let icons ?cancel_handler ?use_capture ?passive x =
    seq_loop ?cancel_handler ?use_capture ?passive icon x
end

module Character_counter = struct
  class t (elt : Dom_html.element Js.t) () =
    object
      inherit Widget.t elt () as super

      method set_value ~(max_length : int) (cur : int) : unit =
        let cur = min cur max_length in
        let s = Printf.sprintf "%d / %d" cur max_length in
        super#root##.textContent := Js.some (Js.string s)
    end

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

  let make ?classes ?a ?current_length ?max_length () =
    D.Character_counter.character_counter ?classes ?a ?current_length ?max_length ()
    |> Tyxml_js.To_dom.of_div
    |> attach
end

module Icon = struct
  module Attr = struct
    let icon_role = "button"

    let aria_label = "aria-label"
  end

  (* XXX Should be inherited from Icon? *)
  class t (elt : Dom_html.element Js.t) () =
    object (self)
      val mutable saved_tab_index = None

      val mutable listeners = []

      val mutable ripple_ = None

      inherit Widget.t elt () as super

      method! init () : unit =
        saved_tab_index <- Element.get_attribute super#root "tabindex";
        ripple_ <- Some (self#create_ripple ());
        super#init ()

      method! initial_sync_with_dom () : unit =
        (* Attach event listeners *)
        listeners <-
          Js_of_ocaml_lwt.Lwt_js_events.(
            [clicks super#root self#handle_click; keydowns super#root self#handle_keydown]
            @ listeners);
        super#initial_sync_with_dom ()

      method! destroy () : unit =
        Option.iter Ripple.destroy ripple_;
        ripple_ <- None;
        (* Detach event listeners *)
        List.iter Lwt.cancel listeners;
        listeners <- [];
        super#destroy ()

      method set_aria_label (label : string) : unit =
        Element.set_attribute super#root Attr.aria_label label

      method set_disabled (x : bool) : unit =
        match saved_tab_index with
        | None -> ()
        | Some tabindex ->
            if x
            then (
              Element.set_attribute super#root "tabindex" "-1";
              Element.remove_attribute super#root "role")
            else (
              Element.set_attribute super#root "tabindex" tabindex;
              Element.set_attribute super#root "role" Attr.icon_role)

      method private notify_action () : unit = super#emit ~should_bubble:true Event.icon

      method private handle_keydown e _ : unit Lwt.t =
        (match Dom_html.Keyboard_code.of_event e with
        | Enter -> self#notify_action ()
        | _ -> ());
        Lwt.return_unit

      method private handle_click _ _ : unit Lwt.t =
        self#notify_action ();
        Lwt.return_unit

      method private create_ripple () : Ripple.t =
        Ripple.attach ~unbounded:true super#root
    end

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()
end

module Helper_text = struct
  module Attr = struct
    let aria_hidden = "aria-hidden"
  end

  class t (elt : Dom_html.element Js.t) () =
    object (self)
      inherit Widget.t elt () as super

      method set_content (s : string) : unit =
        super#root##.textContent := Js.some @@ Js.string s

      method persistent : bool = super#has_class CSS.Helper_text.persistent

      method set_persistent (x : bool) : unit =
        super#toggle_class ~force:x CSS.Helper_text.persistent

      method validation : bool = super#has_class CSS.Helper_text.validation_msg

      method set_validation (x : bool) : unit =
        super#toggle_class ~force:x CSS.Helper_text.validation_msg

      method show_to_screen_reader () : unit = super#remove_attribute Attr.aria_hidden

      method set_validity (is_valid : bool) : unit =
        let needs_display = self#validation && not is_valid in
        if needs_display
        then super#set_attribute "role" "alert"
        else super#remove_attribute "role";
        if (not self#persistent) && not needs_display then self#hide ()

      method private hide () : unit = super#set_attribute Attr.aria_hidden "true"
    end

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

  let make ?classes ?a ?persistent ?validation ?text ?children () =
    D.Helper_text.helper_text ?classes ?a ?persistent ?validation ?text ?children ()
    |> Tyxml_js.To_dom.of_div
    |> attach
end

module Const = struct
  let label_scale = 0.75

  let always_float_types =
    ["color"; "date"; "datetime-local"; "month"; "range"; "time"; "week"]

  let validation_attr_whitelist =
    ["pattern"; "min"; "max"; "required"; "step"; "minlength"; "maxlength"]
end

module Selector = struct
  let input = "." ^ CSS.input

  let icon = "." ^ CSS.icon

  let character_counter = "." ^ CSS.Character_counter.root

  let helper_text = "." ^ CSS.Helper_text.root

  let floating_label = "." ^ Floating_label.CSS.root

  let notched_outline = "." ^ Notched_outline.CSS.root

  let line_ripple = "." ^ Line_ripple.CSS.root
end

class type validity_state =
  object
    method badInput : bool Js.t Js.readonly_prop

    method customError : bool Js.t Js.readonly_prop

    method patternMismatch : bool Js.t Js.readonly_prop

    method rangeOverflow : bool Js.t Js.readonly_prop

    method rangeUnderflow : bool Js.t Js.readonly_prop

    method stepMismatch : bool Js.t Js.readonly_prop

    method tooLong : bool Js.t Js.readonly_prop

    method tooShort : bool Js.t Js.readonly_prop

    method typeMismatch : bool Js.t Js.readonly_prop

    method valid : bool Js.t Js.readonly_prop

    method valueMissing : bool Js.t Js.readonly_prop
  end

type 'a validation =
  | Email : string validation
  | Integer : (int option * int option) -> int validation
  | Float : (float option * float option) -> float validation
  | Text : string validation
  | Password : (string -> (unit, string) result) -> string validation
  | Custom : 'a custom_validation -> 'a validation

and 'a custom_validation =
  { input_type : Html_types.input_type
  ; of_string : string -> ('a, string) result
  ; to_string : 'a -> string }

let input_type_of_validation : type a. a validation -> Html_types.input_type = function
  | Text -> `Text
  | Email -> `Email
  | Float _ -> `Number
  | Integer _ -> `Number
  | Password _ -> `Password
  | Custom c -> c.input_type

let parse_valid (type a) (v : a validation) (s : string) : a option =
  match v with
  | Text -> Some s
  | Email -> Some s
  | Integer (None, None) -> int_of_string_opt s
  | Integer (Some min, None) -> (
    match int_of_string_opt s with
    | None -> None
    | Some (v : int) -> if v >= min then Some v else None)
  | Integer (None, Some max) -> (
    match int_of_string_opt s with
    | None -> None
    | Some (v : int) -> if v <= max then Some v else None)
  | Integer (Some min, Some max) -> (
    match int_of_string_opt s with
    | None -> None
    | Some (v : int) -> if v <= max && v >= min then Some v else None)
  | Float (None, None) -> float_of_string_opt s
  | Float (Some min, None) -> (
    match float_of_string_opt s with
    | None -> None
    | Some (v : float) -> if v >= min then Some v else None)
  | Float (None, Some max) -> (
    match float_of_string_opt s with
    | None -> None
    | Some (v : float) -> if v <= max then Some v else None)
  | Float (Some min, Some max) -> (
    match float_of_string_opt s with
    | None -> None
    | Some (v : float) -> if v <= max && v >= min then Some v else None)
  | Password vf -> (
    match vf s with
    | Ok () -> Some s
    | Error _ -> None)
  | Custom c -> (
    match c.of_string s with
    | Ok v -> Some v
    | Error _ -> None)

let last_char s = s.[String.length s - 1]

let valid_to_string (type a) (v : a validation) (e : a) : string =
  match v with
  | Custom c -> c.to_string e
  | Float _ ->
      let s = string_of_float e in
      if last_char s = '.' then s ^ "0" else s
  | Integer _ -> string_of_int e
  | Email -> e
  | Password _ -> e
  | Text -> e

let get_helper_line (elt : Dom_html.element Js.t) =
  match Js.Opt.to_option @@ Element.get_next_sibling elt with
  | None -> None
  | Some next -> if Element.has_class next CSS.helper_line then Some next else None

let custom_validation (type a) input_elt (v : a validation option) =
  let set_custom_validity s : unit =
    (Js.Unsafe.coerce input_elt)##setCustomValidity (Js.string s)
  in
  let validate : a validation option -> (string -> (unit, string) result) option =
    function
    | Some (Password validate) -> Some validate
    | Some (Custom {of_string; _}) ->
        Some
          (fun s ->
            match s, of_string s with
            | "", _ | _, Ok _ -> Ok ()
            | _, (Error _ as e) -> e)
    | _ -> None
  in
  match validate v with
  | None -> ()
  | Some validate -> (
    match validate @@ Js.to_string input_elt##.value with
    | Ok () -> set_custom_validity ""
    | Error e -> set_custom_validity e)

class ['a] t
  ?on_input
  ?(validate_on_blur = true)
  ?(helper_text : Helper_text.t option)
  ?(use_native_validation = true)
  ?(validation : 'a validation option)
  (elt : Dom_html.element Js.t)
  () =
  let helper_line = get_helper_line elt in
  let icon_elements = Element.query_selector_all elt Selector.icon in
  object (self)
    (* Internal components *)
    val input_elt : Dom_html.inputElement Js.t =
      let element = Element.query_selector_exn elt Selector.input in
      Js.Opt.get (Dom_html.CoerceTo.input element) (fun () -> assert false)

    val line_ripple : Line_ripple.t option =
      match Element.query_selector elt Selector.line_ripple with
      | None -> None
      | Some x -> Some (Line_ripple.attach x)

    val notched_outline : Notched_outline.t option =
      match Element.query_selector elt Selector.notched_outline with
      | None -> None
      | Some x -> Some (Notched_outline.attach x)

    val floating_label : Floating_label.t option =
      match Element.query_selector elt Selector.floating_label with
      | None -> None
      | Some x -> Some (Floating_label.attach x)

    val helper_text =
      match helper_text with
      | Some x -> Some x
      | None -> (
        match helper_line with
        | None -> None
        | Some helper_line -> (
          match Element.query_selector helper_line Selector.helper_text with
          | None -> None
          | Some ht -> Some (Helper_text.attach ht)))

    val character_counter : Character_counter.t option =
      (* Try to search for character counter at root *)
      match Element.query_selector elt Selector.character_counter with
      | Some cc -> Some (Character_counter.attach cc)
      | None -> (
        (* If character counter is not found in root, search in sibling element *)
        match helper_line with
        | None -> None
        | Some helper_line -> (
          match Element.query_selector helper_line Selector.character_counter with
          | None -> None
          | Some cc -> Some (Character_counter.attach cc)))

    val leading_icon : Icon.t option =
      match icon_elements with
      | [] -> None
      | [x] ->
          if Element.has_class elt CSS.with_leading_icon
          then Some (Icon.attach x)
          else None
      | x :: _ :: _ -> Some (Icon.attach x)

    val trailing_icon : Icon.t option =
      match icon_elements with
      | [] -> None
      | [x] ->
          if Element.has_class elt CSS.with_trailing_icon
          then Some (Icon.attach x)
          else None
      | _ :: x :: _ -> Some (Icon.attach x)

    (* Event listeners *)
    val mutable listeners = []

    (* Validation observer *)
    val mutable validation_observer = None

    (* Other variables *)
    val mutable ripple_ : Ripple.t option = None

    val mutable use_native_validation = use_native_validation

    val mutable received_user_input = false

    val mutable is_valid = true

    val mutable is_focused = false

    inherit Widget.t elt () as super

    method! init () : unit =
      (if self#focused
      then self#activate_focus ()
      else
        match floating_label, self#should_float with
        | None, _ | Some _, false -> ()
        | Some (label : Floating_label.t), true ->
            self#notch_outline true;
            label#float true);
      custom_validation input_elt validation;
      self#set_character_counter (String.length self#value_as_string);
      (* Initialize ripple, if needed *)
      if (not (super#has_class CSS.textarea)) && not (super#has_class CSS.outlined)
      then ripple_ <- Some (self#create_ripple ());
      (* Apply validation constraints *)
      Option.iter self#apply_validation_constraints validation;
      super#init ()

    method! initial_sync_with_dom () : unit =
      (* Attach mutation observer *)
      validation_observer <-
        Some (self#register_validation_handler self#handle_validation_attribute_change);
      (* Attach event listeners *)
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ focuses input_elt (fun _ _ ->
                self#activate_focus ();
                Lwt.return_unit)
          ; blurs input_elt (fun _ _ ->
                self#deactivate_focus ();
                Lwt.return_unit)
          ; inputs input_elt self#handle_input
          ; mousedowns input_elt self#set_transform_origin
          ; touchstarts input_elt self#set_transform_origin
          ; clicks input_elt self#handle_text_field_interaction
          ; keydowns input_elt self#handle_text_field_interaction ]
          @ listeners);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      super#destroy ();
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      (* Detach mutation observer *)
      Option.iter (fun x -> x##disconnect) validation_observer;
      validation_observer <- None;
      (* Destroy internal components *)
      Option.iter Ripple.destroy ripple_;
      Option.iter Widget.destroy line_ripple;
      Option.iter Widget.destroy floating_label;
      Option.iter Widget.destroy notched_outline;
      Option.iter Widget.destroy character_counter;
      Option.iter Widget.destroy helper_text;
      Option.iter Widget.destroy leading_icon;
      Option.iter Widget.destroy trailing_icon

    method input_element : Dom_html.inputElement Js.t = input_elt

    method leading_icon : Icon.t option = leading_icon

    method trailing_icon : Icon.t option = trailing_icon

    method disabled : bool = Js.to_bool input_elt##.disabled

    method set_disabled (x : bool) : unit =
      input_elt##.disabled := Js.bool x;
      self#style_disabled x

    method set_leading_icon_aria_label (s : string) =
      Option.iter (fun (x : Icon.t) -> x#set_aria_label s) leading_icon

    method set_trailing_icon_aria_label (s : string) =
      Option.iter (fun (x : Icon.t) -> x#set_aria_label s) trailing_icon

    method set_helper_text_content (s : string) =
      Option.iter (fun (x : Helper_text.t) -> x#set_content s) helper_text

    method focus () : unit =
      self#activate_focus ();
      input_elt##focus

    method ripple : Ripple.t option = ripple_

    method required : bool = Js.to_bool (Js.Unsafe.coerce input_elt)##.required

    method set_required (x : bool) : unit = input_elt##.required := Js.bool x

    method use_native_validation : bool = use_native_validation

    method set_use_native_validation (x : bool) : unit =
      if x then is_valid <- true;
      use_native_validation <- x

    method force_custom_validation () : unit =
      custom_validation self#input_element validation

    method check_validity () : bool =
      Js.to_bool @@ (Js.Unsafe.coerce self#input_element)##checkValidity

    method validity : validity_state Js.t =
      (Js.Unsafe.coerce self#input_element)##.validity

    method valid : bool =
      if use_native_validation then self#is_native_input_valid () else is_valid

    method set_valid (x : bool) : unit =
      if not use_native_validation then is_valid <- x;
      self#style_validity x;
      Option.iter (fun x -> x#shake self#should_shake) floating_label

    method pattern : string = Js.to_string (Js.Unsafe.coerce input_elt)##.pattern

    method set_pattern (pattern : string) : unit =
      (Js.Unsafe.coerce input_elt)##.pattern := Js.string pattern

    method min_length : int option =
      match (Js.Unsafe.coerce input_elt)##.minLength with
      | x when x < 0 -> None
      | x -> Some x

    method set_min_length : int option -> unit =
      function
      | None -> (Js.Unsafe.coerce input_elt)##.minLength := -1
      | Some x -> (Js.Unsafe.coerce input_elt)##.minLength := x

    method max_length : int option =
      match input_elt##.maxLength with
      | x when x < 0 -> None
      | x -> Some x

    method set_max_length : int option -> unit =
      function
      | None -> Element.remove_attribute input_elt "maxLength"
      | Some x -> input_elt##.maxLength := x

    method validation_message : string =
      Js.to_string (Js.Unsafe.coerce input_elt)##.validationMessage

    (* Value API *)
    method is_empty : bool =
      match self#value_as_string with
      | "" -> true
      | _ -> false

    method clear () : unit = input_elt##.value := Js.string ""

    method value_as_string : string = Js.to_string input_elt##.value

    method value : 'a option =
      match validation with
      | None -> None
      | Some validation -> parse_valid validation self#value_as_string

    method set_value_as_string (s : string) : unit =
      (* Prevent Safari from moving the caret to the end of the
         input when the value has not changed. *)
      if not @@ String.equal self#value_as_string s
      then (
        input_elt##.value := Js.string s;
        custom_validation input_elt validation;
        self#set_character_counter (String.length s));
      self#style_validity self#valid;
      Option.iter
        (fun (label : Floating_label.t) ->
          self#notch_outline self#should_float;
          label#float self#should_float;
          label#shake self#should_shake)
        floating_label

    method set_value (v : 'a) =
      match validation with
      | None -> failwith (CSS.root ^ ": type validation is not set")
      | Some validation ->
          let v' = valid_to_string validation v in
          self#set_value_as_string v'

    (* Private methods *)
    method private should_always_float : bool =
      let typ = Js.to_string input_elt##._type in
      List.exists (String.equal typ) Const.always_float_types

    method private should_float : bool =
      self#should_always_float
      || is_focused
      || (not self#is_empty)
      || self#is_bad_input ()

    method private should_shake : bool =
      (not is_focused) && (not self#valid) && not self#is_empty

    method private focused : bool =
      let active = Dom_html.document##.activeElement in
      match Js.Opt.to_option active with
      | None -> false
      | Some elt -> Element.equal (Element.coerce input_elt) elt

    method private handle_input e _ : unit Lwt.t =
      self#auto_complete_focus ();
      self#set_character_counter @@ String.length self#value_as_string;
      custom_validation input_elt validation;
      match on_input with
      | None -> Lwt.return_unit
      | Some f -> f e (self :> 'a t)

    method private handle_text_field_interaction
        : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t -> unit Lwt.t =
      fun _ _ ->
        (if not (Js.to_bool input_elt##.disabled) then received_user_input <- true;
         Lwt.return_unit
          : unit Lwt.t)

    method private handle_validation_attribute_change (attrs : string list) : unit =
      let rec aux = function
        | [] -> ()
        | attr :: tl ->
            if String.equal "maxlength" attr
            then self#set_character_counter @@ String.length self#value_as_string;
            if List.exists (String.equal attr) Const.validation_attr_whitelist
            then self#style_validity true
            else aux tl
      in
      aux attrs

    method private notch_outline (open_notch : bool) : unit =
      Option.iter
        (fun (outline : Notched_outline.t) ->
          if not open_notch
          then outline#close_notch ()
          else
            let label_scale = Const.label_scale in
            let label_width =
              match floating_label with
              | None -> 0
              | Some (label : Floating_label.t) -> label#width
            in
            outline#notch (float_of_int label_width *. label_scale))
        notched_outline

    method private set_transform_origin
        : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t -> unit Lwt.t =
      fun (event : #Dom_html.event Js.t) _ ->
        (let target, client_x =
           Js.Opt.case
             (Dom_html.CoerceTo.mouseEvent event)
             (fun () ->
               let (event : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
               let touch = Js.Optdef.to_option (event##.touches##item 0) in
               match touch with
               | None -> None, 0
               | Some (touch : Dom_html.touch Js.t) ->
                   Js.Optdef.to_option touch##.target, touch##.clientX)
             (fun e -> Js.Opt.to_option e##.target, e##.clientX)
         in
         let left =
           match target with
           | None -> 0.
           | Some x -> x##getBoundingClientRect##.left
         in
         let normalized_x = float_of_int client_x -. left in
         Option.iter
           (fun (line_ripple : Line_ripple.t) ->
             line_ripple#set_ripple_center normalized_x)
           line_ripple;
         Lwt.return_unit
          : unit Lwt.t)

    method private auto_complete_focus () : unit =
      if not received_user_input then self#activate_focus ()

    method private activate_focus () : unit =
      is_focused <- true;
      self#style_focused is_focused;
      Option.iter
        (fun (line_ripple : Line_ripple.t) -> line_ripple#activate ())
        line_ripple;
      Option.iter
        (fun (label : Floating_label.t) ->
          self#notch_outline self#should_float;
          label#float self#should_float;
          label#shake self#should_shake)
        floating_label;
      Option.iter (fun (x : Helper_text.t) -> x#show_to_screen_reader ()) helper_text

    method private deactivate_focus () : unit =
      is_focused <- false;
      Option.iter
        (fun (line_ripple : Line_ripple.t) -> line_ripple#deactivate ())
        line_ripple;
      custom_validation input_elt validation;
      if validate_on_blur && not use_native_validation
      then ignore @@ self#check_validity ();
      self#style_validity self#valid;
      self#style_focused is_focused;
      Option.iter
        (fun (label : Floating_label.t) ->
          self#notch_outline self#should_float;
          label#float self#should_float;
          label#shake self#should_shake)
        floating_label;
      if not self#should_float then received_user_input <- false

    method private set_character_counter (cur_length : int) : unit =
      Option.iter
        (fun (cc : Character_counter.t) ->
          let max_length = input_elt##.maxLength in
          if max_length = -1
          then failwith (CSS.root ^ ": expected `maxlength` html property was set");
          cc#set_value ~max_length cur_length)
        character_counter

    method private is_bad_input () : bool = Js.to_bool self#validity##.badInput

    method private is_native_input_valid () : bool = Js.to_bool self#validity##.valid

    method private style_validity (is_valid : bool) : unit =
      super#toggle_class ~force:(not is_valid) CSS.invalid;
      Option.iter (fun (x : Helper_text.t) -> x#set_validity is_valid) helper_text

    method private style_focused (is_focused : bool) : unit =
      super#toggle_class ~force:is_focused CSS.focused

    method private style_disabled (is_disabled : bool) : unit =
      super#toggle_class ~force:is_disabled CSS.disabled;
      if is_disabled then super#remove_class CSS.invalid;
      Option.iter (fun x -> x#set_disabled is_disabled) leading_icon;
      Option.iter (fun x -> x#set_disabled is_disabled) trailing_icon

    method private register_validation_handler handler =
      MutationObserver.(
        observe
          ~node:input_elt
          ~attributes:true
          ~f:(fun (arr : mutationRecord Js.t Js.js_array Js.t) _ ->
            let cb acc (record : mutationRecord Js.t) _ _ =
              let attr = record##.attributeName in
              if Js.Opt.test attr
              then
                let (s : string) =
                  Js.to_string @@ Js.Opt.get attr (fun () -> assert false)
                in
                s :: acc
              else acc
            in
            handler @@ arr##reduce_init (Js.wrap_callback cb) [])
          ())

    method private set_max_as_number (x : float) : unit =
      (Js.Unsafe.coerce input_elt)##.max := x

    method private set_min_as_number (x : float) : unit =
      (Js.Unsafe.coerce input_elt)##.min := x

    method private apply_validation_constraints (type a) (v : a validation) : unit =
      match v with
      | Float (min, max) ->
          Option.iter self#set_min_as_number min;
          Option.iter self#set_max_as_number max
      | Integer (min, max) ->
          Option.iter (self#set_min_as_number % float_of_int) min;
          Option.iter (self#set_max_as_number % float_of_int) max
      | _ -> ()

    method private create_ripple () : Ripple.t =
      let adapter = Ripple.make_default_adapter super#root in
      let is_surface_active () = Element.matches input_elt ":active" in
      let is_surface_disabled () = self#disabled in
      let adapter =
        { adapter with
          event_target = Element.coerce input_elt
        ; is_surface_active
        ; is_surface_disabled }
      in
      new Ripple.t adapter ()
  end

let attach
    ?validate_on_blur
    ?on_input
    ?helper_text
    ?use_native_validation
    ?(validation : 'a validation option)
    (elt : #Dom_html.element Js.t) : 'a t =
  new t
    ?validate_on_blur
    ?on_input
    ?helper_text
    ?validation
    ?use_native_validation
    (Element.coerce elt)
    ()

let make_textarea
    ?classes
    ?a
    ?disabled
    ?fullwidth
    ?focused
    ?show_character_counter
    ?character_counter
    ?input_id
    ?label
    ?outline
    ?value
    ?placeholder
    ?required
    ?min_length
    ?max_length
    ?rows
    ?cols
    ?input
    ?validate_on_blur
    ?on_input
    ?helper_text
    ?use_native_validation
    () =
  D.Textarea.textarea
    ?classes
    ?a
    ?disabled
    ?fullwidth
    ?focused
    ?show_character_counter
    ?character_counter
    ?input_id
    ?label
    ?outline
    ?value
    ?placeholder
    ?required
    ?min_length
    ?max_length
    ?rows
    ?cols
    ?input
    ()
  |> Tyxml_js.To_dom.of_div
  |> attach
       ?validate_on_blur
       ?on_input
       ?helper_text
       ?use_native_validation
       ~validation:Text

let make
    ?classes
    ?a
    ?disabled
    ?leading_icon
    ?trailing_icon
    ?fullwidth
    ?textarea
    ?focused
    ?label
    ?outline
    ?outlined
    ?line_ripple
    ?input_id
    ?pattern
    ?min_length
    ?max_length
    ?step
    ?value
    ?placeholder
    ?required
    ?typ
    ?input_mode
    ?input
    ?validate_on_blur
    ?on_input
    ?helper_text
    ?use_native_validation
    ?validation
    () =
  Option.iter
    (fun x -> Element.add_class (Tyxml_js.To_dom.of_element x) CSS.icon)
    leading_icon;
  Option.iter
    (fun x -> Element.add_class (Tyxml_js.To_dom.of_element x) CSS.icon)
    trailing_icon;
  let input_id =
    match input_id with
    | Some _ as x -> x
    | None -> (
      match input with
      | None -> Some (Id.get ())
      | Some input -> (
          let input = Tyxml_js.To_dom.of_element input in
          let id = Js.to_string input##.id in
          match id with
          | "" -> None
          | id -> Some id))
  in
  let typ =
    match typ with
    | Some _ as x -> x
    | None -> (
      match validation with
      | Some v -> Some (input_type_of_validation v)
      | None -> None)
  in
  (* Stringify value, if any *)
  let value =
    match value with
    | None -> None
    | Some x -> (
      match validation with
      | None -> None
      | Some validation -> Some (valid_to_string validation x))
  in
  D.textfield
    ?classes
    ?a
    ?disabled
    ?leading_icon
    ?trailing_icon
    ?fullwidth
    ?textarea
    ?focused
    ?label
    ?outline
    ?outlined
    ?line_ripple
    ?input_id
    ?pattern
    ?min_length
    ?max_length
    ?step
    ?value
    ?placeholder
    ?required
    ?typ
    ?input_mode
    ?input
    ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?validate_on_blur ?on_input ?helper_text ?use_native_validation ?validation
