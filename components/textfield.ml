open Containers
open Tyxml_js

module Markup = Components_markup.Textfield.Make(Xml)(Svg)(Html)

let id_ref = ref (Unix.time () |> int_of_float)
let get_id = fun () ->
  incr id_ref;
  Printf.sprintf "text-field-%d" !id_ref

let label_scale = 0.75
let dense_label_scale = 0.923

let always_float_types =
  ["color"; "date"; "datetime-local"; "month"; "range"; "time"; "week"]

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

class ['a] t ?input_id
        ?label
        ?placeholder
        ?(outlined = false)
        ~input_type () =

  let input_id = match input_id with
    | None -> get_id ()
    | Some s -> s in
  let outline = match outlined with
    | false -> None
    | true ->
       Some (new Notched_outline.t (),
             (Notched_outline.Markup.create_idle ())) in
  let notched_outline = Option.map fst outline in
  let floating_label = match label with
    | None -> None
    | Some x -> Some (new Floating_label.t ~for_:input_id x ()) in
  let line_ripple = match true with
    | false -> None
    | true -> Some (new Line_ripple.t ()) in
  let input_elt =
    Markup.create_input
      ?placeholder
      ~input_id
      ~input_type:(Widget.input_type_of_validation input_type)
      ()
    |> To_dom.of_input in
  let input_widget =
    Widget.create input_elt in
  let elt =
    Markup.create
      ?label:(Option.map Widget.to_markup floating_label)
      ?outline:(Option.map (fun (x, y) -> Widget.to_markup x, y) outline)
      ~input:(Widget.to_markup input_widget) ()
    |> To_dom.of_element in
  object(self)

    val mutable _use_native_validation = false
    val mutable _received_user_input = false
    val mutable _is_valid = true
    val mutable _is_focused = false
    val mutable _listeners = []

    inherit ['a] Widget.text_input_widget
              ~input_elt
              input_type
              elt
              () as super

    method set_disabled (x : bool) =
      super#set_disabled x;
      self#style_disabled x

    method set_use_native_validation (x : bool) =
      _use_native_validation <- x

    method valid : bool =
      if _use_native_validation
      then self#is_native_input_valid ()
      else _is_valid

    method set_valid (x : bool) : unit =
      _is_valid <- x;
      self#style_validity x;
      let shold_shake = not x && not _is_focused in
      Option.iter (fun x -> x#shake shold_shake) floating_label

    method value : 'a option =
      React.S.value self#s_input

    method set_value (v : 'a) =
      super#set_value v;
      self#style_validity _is_valid;
      Option.iter (fun l ->
          self#notch_outline self#should_float;
          l#float self#should_float;
          l#shake self#should_shake) floating_label

    method dense : bool =
      self#has_class Markup.dense_class

    method set_dense (x : bool) : unit =
      self#add_or_remove_class x Markup.dense_class

    method full_width : bool =
      self#has_class Markup.full_width_class

    method set_full_width x  =
      self#add_or_remove_class x Markup.full_width_class

    method handle_text_field_interaction () : unit =
      if not (Js.to_bool input_elt##.disabled)
      then _received_user_input <- true

    method notch_outline (open_notch : bool) : unit =
      match notched_outline with
      | None -> ()
      | Some outline ->
         if open_notch
         then
           let label_scale =
             if self#dense then dense_label_scale
             else label_scale in
           let label_width =
             Option.map_or ~default:0 (fun l -> l#width) floating_label in
           outline#notch (float_of_int label_width *. label_scale)

    method private activate_focus () : unit =
      (* TODO helper text *)
      _is_focused <- true;
      self#style_focused _is_focused;
      Option.iter (fun r -> r#activate ()) line_ripple;
      Option.iter (fun l ->
          self#notch_outline self#should_float;
          l#float self#should_float;
          l#shake self#should_shake) floating_label;

    method private auto_complete_focus () : unit =
      if not _received_user_input
      then self#activate_focus ()

    method private deactivate_focus () : unit =
      _is_focused <- false;
      Option.iter (fun r -> r#deactivate ()) line_ripple;
      self#style_validity _is_valid;
      self#style_focused _is_focused;
      Option.iter (fun l ->
          self#notch_outline self#should_float;
          l#float self#should_float;
          l#shake self#should_shake) floating_label;
      if not self#should_float
      then _received_user_input <- false

    (* Sets the line ripple's transform origin,
     * so that the line ripple activate
     * animation will animate out from the user's click location.*)
    method private set_transform_origin (event : Dom_html.mouseEvent Js.t) : unit =
      let target = event##.target in
      let left = match Js.Opt.to_option target with
        | None -> 0
        | Some x -> int_of_float x##getBoundingClientRect##.left in
      let (x : int) = event##.clientX in
      let normalized_x = x - left in
      Option.iter (fun r -> r#set_ripple_center normalized_x) line_ripple

    method private is_bad_input () : bool =
      let (validity : validity_state Js.t) =
        (Js.Unsafe.coerce input_elt)##.validity in
      Js.to_bool validity##.badInput

    method private is_native_input_valid () : bool =
      let (validity : validity_state Js.t) =
        (Js.Unsafe.coerce input_elt)##.validity in
      Js.to_bool validity##.valid

    method private style_validity (is_valid : bool) : unit =
      self#add_or_remove_class (not is_valid) Markup.invalid_class

    method private style_focused (is_focused : bool) : unit =
      self#add_or_remove_class is_focused Markup.focused_class

    method private style_disabled (is_disabled : bool) : unit =
      (* TODO add styles to icons *)
      if is_disabled
      then (self#add_class Markup.disabled_class;
            self#remove_class Markup.invalid_class;)
      else self#remove_class Markup.disabled_class

    method private should_always_float : bool =
      let typ = Js.to_string input_elt##._type in
      List.mem ~eq:String.equal typ always_float_types

    method private should_float : bool =
      self#should_always_float
      || _is_focused
      || Option.is_some self#value
      || self#is_bad_input ()

    method private should_shake : bool =
      not _is_valid && not _is_focused && Option.is_some self#value

    method init () : unit =
      input_widget#listen_lwt Widget.Event.focus (fun _ _ ->
          Lwt.return @@ self#activate_focus ())
      |> fun x -> _listeners <- x :: _listeners;

    method destroy () : unit =
      super#destroy ();
      List.iter Lwt.cancel _listeners;
      _listeners <- []

    initializer
      self#init ()

  end
