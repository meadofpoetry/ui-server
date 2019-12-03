open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Checkbox
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

type transition_state =
  | Init
  | Checked
  | Unchecked
  | Indeterminate

let equal_transition_state (a : transition_state as 'a) (b : 'a) : bool =
  match a, b with
  | Init, Init -> true
  | Checked, Checked -> true
  | Unchecked, Unchecked -> true
  | Indeterminate, Indeterminate -> true
  | _, _ -> false

module Const = struct
  let anim_end_latch_s = 0.25

  let cb_proto_props = ["checked"; "indeterminate"]
end

module Attr = struct
  let aria_checked = "aria-checked"
end

module Selector = struct
  let native_control = Printf.sprintf "input.%s" CSS.native_control
end

class t ?on_change ?(indeterminate = false) (elt : Dom_html.element Js.t) () =
  object (self)
    val input_elt : Dom_html.inputElement Js.t =
      let element = Element.query_selector_exn elt Selector.native_control in
      Js.Opt.get (Dom_html.CoerceTo.input element) (fun () -> assert false)

    val mutable ripple : Ripple.t option = None

    val mutable listeners = []

    val mutable cur_check_state : transition_state = Init

    val mutable cur_animation_class : string option = None

    val mutable enable_animationend_handler = false

    val mutable anim_end_latch_timer = None

    inherit Widget.t elt () as super

    method! init () : unit =
      self#install_property_change_hooks ();
      if indeterminate then self#set_indeterminate true;
      cur_check_state <- self#determine_check_state ();
      self#update_aria_checked ();
      super#add_class CSS.upgraded;
      ripple <- Some (self#create_ripple ());
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ changes input_elt (fun _ _ ->
                self#transition_check_state ();
                self#notify_change ())
          ; seq_loop
              (make_event Dom_html.Event.animationend)
              super#root
              self#handle_animation_end ]
          @ listeners);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      Option.iter Ripple.layout ripple;
      super#layout ()

    method! destroy () : unit =
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      (* Destroy internal components *)
      Option.iter Ripple.destroy ripple;
      ripple <- None;
      (* Clear internal timers *)
      Option.iter Lwt.cancel anim_end_latch_timer;
      anim_end_latch_timer <- None;
      self#uninstall_property_change_hooks ();
      super#destroy ()

    method value : string = Js.to_string input_elt##.value

    method set_value (s : string) : unit = input_elt##.value := Js.string s

    method set_indeterminate (x : bool) : unit =
      (Js.Unsafe.coerce input_elt)##.indeterminate := Js.bool x

    method indeterminate : bool = Js.to_bool (Js.Unsafe.coerce input_elt)##.indeterminate

    method disabled : bool = Js.to_bool input_elt##.disabled

    method set_disabled (x : bool) : unit =
      input_elt##.disabled := Js.bool x;
      super#toggle_class ~force:x CSS.disabled

    method checked : bool = Js.to_bool input_elt##.checked

    method toggle ?(notify = false) ?(force : bool option) () : unit =
      let v =
        match force with
        | None -> not self#checked
        | Some x -> x
      in
      input_elt##.checked := Js.bool v;
      if notify then Lwt.async self#notify_change

    method input_element : Dom_html.inputElement Js.t = input_elt

    method ripple : Ripple.t option = ripple

    method private notify_change () : unit Lwt.t =
      match on_change with
      | None -> Lwt.return_unit
      | Some f -> f (self :> t)

    method private create_ripple () : Ripple.t =
      let adapter = Ripple.make_default_adapter super#root in
      let is_unbounded () = true in
      let is_surface_active () = Element.matches input_elt ":active" in
      let adapter =
        { adapter with
          event_target = Element.coerce input_elt
        ; is_unbounded
        ; is_surface_active }
      in
      new Ripple.t adapter ()

    method private force_layout () : unit = ignore super#root##.offsetWidth

    method private handle_animation_end
        (_ : Dom_html.animationEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      if enable_animationend_handler
      then (
        let t =
          Lwt.catch
            (fun () ->
              Js_of_ocaml_lwt.Lwt_js.sleep Const.anim_end_latch_s
              >>= fun () ->
              Option.iter super#remove_class cur_animation_class;
              enable_animationend_handler <- false;
              Lwt.return_unit)
            (function
              | Lwt.Canceled -> Lwt.return_unit
              | exn -> Lwt.fail exn)
        in
        anim_end_latch_timer <- Some t;
        t)
      else Lwt.return_unit
    (** Handles the `animationend` event for the checkbox *)

    method private transition_check_state () : unit =
      let prev = cur_check_state in
      let cur = self#determine_check_state () in
      if not (equal_transition_state prev cur)
      then (
        self#update_aria_checked ();
        (* Check to ensure that there isn't a previously existing animation class,
           in case for example the user interacted with the checkbox before
           then animation has finished *)
        (match cur_animation_class with
        | None -> ()
        | Some c ->
            Option.iter Lwt.cancel anim_end_latch_timer;
            anim_end_latch_timer <- None;
            self#force_layout ();
            super#remove_class c);
        cur_animation_class <- self#get_transition_animation_class ~prev cur;
        cur_check_state <- cur;
        (* Check for parentNode so that animations are only run when
           then element is attached to the DOM *)
        match
          ( Js.Opt.to_option super#root##.parentNode
          , Js.Opt.test super#root##.offsetParent
          , cur_animation_class )
        with
        | None, _, _ | _, false, _ | _, _, None -> ()
        | Some _, true, Some c ->
            super#add_class c;
            enable_animationend_handler <- true)

    method private determine_check_state () : transition_state =
      if self#indeterminate
      then Indeterminate
      else if self#checked
      then Checked
      else Unchecked

    method private get_transition_animation_class
        ~(prev : transition_state)
        (cur : transition_state)
        : string option =
      match prev, cur with
      | Init, Unchecked -> None
      | Init, Checked -> Some CSS.anim_indeterminate_checked
      | Init, _ -> Some CSS.anim_indeterminate_unchecked
      | Unchecked, Checked -> Some CSS.anim_unchecked_checked
      | Unchecked, _ -> Some CSS.anim_unchecked_indeterminate
      | Checked, Unchecked -> Some CSS.anim_checked_unchecked
      | Checked, _ -> Some CSS.anim_checked_indeterminate
      | Indeterminate, Checked -> Some CSS.anim_indeterminate_checked
      | Indeterminate, _ -> Some CSS.anim_indeterminate_unchecked

    method private update_aria_checked () : unit =
      (* Ensure aria-checked is set to mixed if checkbox is in indeterminate state *)
      if self#indeterminate
      then Element.set_attribute input_elt Attr.aria_checked "mixed"
      else
        (* The on/off state does not need to keep track of aria-checked, since
           the screenreader uses the checked property on the checkbox element *)
        Element.remove_attribute input_elt Attr.aria_checked

    method private install_property_change_hooks () : unit =
      let obj = Js.Unsafe.global ##. Object in
      let cb_proto = obj##getPrototypeOf input_elt in
      let install control_state =
        let control_state = Js.string control_state in
        let desc = obj##getOwnPropertyDescriptor cb_proto control_state in
        match Js.Optdef.test desc, Js.to_string @@ Js.typeof desc##.set with
        | true, "function" ->
            let native_cb_desc =
              object%js
                val configurable = desc##.configurable

                val enumerable = desc##.enumerable

                val get = desc##.get

                method set (x : bool Js.t) =
                  ignore @@ desc##.set##call input_elt x;
                  self#transition_check_state ()
              end
            in
            obj##defineProperty input_elt control_state native_cb_desc
        | _, _ -> ()
      in
      List.iter install Const.cb_proto_props

    method private uninstall_property_change_hooks () : unit =
      let obj = Js.Unsafe.global ##. Object in
      let cb_proto = obj##getPrototypeOf input_elt in
      let uninstall control_state =
        let control_state = Js.string control_state in
        let desc = obj##getOwnPropertyDescriptor cb_proto control_state in
        match Js.Optdef.test desc, Js.to_string @@ Js.typeof desc##.set with
        | true, "function" -> obj##defineProperty input_elt control_state desc
        | _, _ -> ()
      in
      List.iter uninstall Const.cb_proto_props
  end

let attach ?on_change ?indeterminate (elt : #Dom_html.element Js.t) : t =
  new t ?on_change ?indeterminate (elt :> Dom_html.element Js.t) ()

let make ?classes ?a ?input_id ?checked ?indeterminate ?disabled ?on_change () =
  D.checkbox ?classes ?a ?input_id ?checked ?disabled ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?on_change ?indeterminate
