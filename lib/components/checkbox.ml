open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Checkbox
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

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

class checkbox_signals input_elt = object(self)
  method connect : 'a. (#Dom_html.event as 'a) Js.t Events.Typ.t
    -> ('a Js.t -> unit Lwt.t -> unit Lwt.t)
    -> unit Lwt.t =
    fun e cb -> Events.listen_lwt input_elt e cb

  method change = self#connect Events.Typ.change

end

class t ?on_change (elt : Dom_html.element Js.t) () =
  let input_elt : Dom_html.inputElement Js.t =
    find_element_by_class_exn elt CSS.native_control in
  object(self)
    val connect = new checkbox_signals input_elt
    val mutable _ripple : Ripple.t option = None
    val mutable _change_listener = None
    val mutable _animationend_listener = None
    val mutable _cur_check_state : transition_state = Init
    val mutable _cur_animation_class : string option = None
    val mutable _enable_animationend_handler = false
    val mutable _anim_end_latch_timer = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      self#install_property_change_hooks ();
      _cur_check_state <- self#determine_check_state ();
      self#update_aria_checked ();
      super#add_class CSS.upgraded;
      _ripple <- Some (self#create_ripple ())

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      let change_listener =
        Events.changes input_elt (fun _ _ ->
            self#transition_check_state ();
            self#notify_change ()) in
      _change_listener <- Some change_listener;
      let animationend_listener =
        Events.listen_lwt super#root Events.Typ.animationend
          self#handle_animation_end in
      _animationend_listener <- Some animationend_listener

    method! layout () : unit =
      super#layout ();
      match _ripple with
      | None -> ()
      | Some r -> Ripple.layout r

    method! destroy () : unit =
      super#destroy ();
      (* Detach event listeners *)
      Option.iter Lwt.cancel _change_listener;
      _change_listener <- None;
      Option.iter Lwt.cancel _animationend_listener;
      _animationend_listener <- None;
      (* Destroy internal components *)
      Option.iter Ripple.destroy _ripple;
      _ripple <- None;
      (* Clear internal timers *)
      Option.iter Lwt.cancel _anim_end_latch_timer;
      _anim_end_latch_timer <- None;
      self#uninstall_property_change_hooks ()

    method connect = connect

    method value : string =
      Js.to_string input_elt##.value

    method set_value (s : string) : unit =
      input_elt##.value := Js.string s

    method set_indeterminate (x : bool) : unit =
      (Js.Unsafe.coerce input_elt)##.indeterminate := Js.bool x

    method indeterminate : bool =
      Js.to_bool (Js.Unsafe.coerce input_elt)##.indeterminate

    method disabled : bool =
      Js.to_bool input_elt##.disabled

    method set_disabled (x : bool) : unit =
      input_elt##.disabled := Js.bool x;
      super#toggle_class ~force:x CSS.disabled

    method checked : bool =
      Js.to_bool input_elt##.checked

    method toggle ?(notify = false) ?(force : bool option) () : unit =
      let v = match force with None -> not self#checked | Some x -> x in
      input_elt##.checked := Js.bool v;
      if notify then Lwt.async self#notify_change

    method input_element : Dom_html.inputElement Js.t =
      input_elt

    method ripple : Ripple.t option =
      _ripple

    (* Private methods *)

    method private notify_change () : unit Lwt.t =
      match on_change with
      | None -> Lwt.return_unit
      | Some f -> f (self :> t)

    method private create_ripple () : Ripple.t =
      let adapter = Ripple.make_default_adapter super#root in
      let is_unbounded = fun () -> true in
      let is_surface_active = fun () -> Element.matches input_elt ":active" in
      let adapter =
        { adapter with event_target = Element.coerce input_elt
                     ; is_unbounded
                     ; is_surface_active } in
      new Ripple.t adapter ()

    method private force_layout () : unit =
      ignore super#root##.offsetWidth

    (** Handles the `animationend` event for the checkbox *)
    method private handle_animation_end
        (_ : Dom_html.animationEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      if _enable_animationend_handler
      then (
        let t =
          Lwt.catch (fun () ->
              Lwt_js.sleep Const.anim_end_latch_s
              >>= fun () ->
              Option.iter super#remove_class _cur_animation_class;
              _enable_animationend_handler <- false;
              Lwt.return_unit)
            (function
              | Lwt.Canceled -> Lwt.return_unit
              | exn -> Lwt.fail exn) in
        _anim_end_latch_timer <- Some t;
        t)
      else Lwt.return_unit

    method private transition_check_state () : unit =
      let prev = _cur_check_state in
      let cur = self#determine_check_state () in
      if not (equal_transition_state prev cur)
      then (
        self#update_aria_checked ();
        (* Check to ensure that there isn't a previously existing animation class,
           in case for example the user interacted with the checkbox before
           then animation has finished *)
        (match _cur_animation_class with
         | None -> ()
         | Some c ->
           Option.iter Lwt.cancel _anim_end_latch_timer;
           _anim_end_latch_timer <- None;
           self#force_layout ();
           super#remove_class c);
        _cur_animation_class <- self#get_transition_animation_class ~prev cur;
        _cur_check_state <- cur;
        (* Check for parentNode so that animations are only run when
           then element is attached to the DOM *)
        match Js.Opt.to_option super#root##.parentNode,
              Js.Opt.test super#root##.offsetParent,
              _cur_animation_class with
        | None, _, _ | _, false, _ | _, _, None -> ()
        | Some _, true, Some c -> super#add_class c; _enable_animationend_handler <- true)

    method private determine_check_state () : transition_state =
      if self#indeterminate
      then Indeterminate
      else if self#checked then Checked else Unchecked

    method private get_transition_animation_class ~(prev : transition_state)
        (cur : transition_state) : string option =
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
      let obj = Js.Unsafe.global##.Object in
      let cb_proto = obj##getPrototypeOf input_elt in
      List.iter (fun control_state ->
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
              end in
            obj##defineProperty input_elt control_state native_cb_desc
          | _, _ -> ())
        Const.cb_proto_props

    method private uninstall_property_change_hooks () : unit =
      let obj = Js.Unsafe.global##.Object in
      let cb_proto = obj##getPrototypeOf input_elt in
      List.iter (fun control_state ->
          let control_state = Js.string control_state in
          let desc = obj##getOwnPropertyDescriptor cb_proto control_state in
          match Js.Optdef.test desc, Js.to_string @@ Js.typeof desc##.set with
          | true, "function" -> obj##defineProperty input_elt control_state desc
          | _, _ -> ())
        Const.cb_proto_props
  end

let make ?input_id ?checked ?disabled ?on_change () =
  let elt =
    Tyxml_js.To_dom.of_div
    @@ Markup.create ?input_id ?checked ?disabled () in
  new t ?on_change elt ()

let attach ?on_change (elt : #Dom_html.element Js.t) : t =
  new t ?on_change (elt :> Dom_html.element Js.t) ()
