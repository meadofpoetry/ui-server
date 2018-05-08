open Containers

class t ?(propagate=true) ~on_data ~off_data () =

  let elt = Markup.Icon_toggle.create ~on_data ~off_data () |> Tyxml_js.To_dom.of_i in
  let s_state,s_state_push = React.S.create false in
  let is_space (e:Dom_html.keyboardEvent Js.t) =
    let key = Option.map Js.to_string @@ Js.Optdef.to_option e##.key in
    match key,e##.keyCode with
    | Some "Space", _ | _, 32 -> true
    | _ -> false
  in

  object(self)
    inherit Widget.widget elt ()

    val mutable on          = false
    val mutable disabled    = false
    val mutable tab_index   = -1
    val mutable on_data     = on_data
    val mutable off_data    = off_data
    val mutable is_key_down = false

    method disabled       = disabled
    method set_disabled x =
      disabled <- x;
      self#add_or_remove_class x Markup.Icon_toggle.disabled_class;
      match disabled with
      | true  -> tab_index <- (Js.Unsafe.coerce self#root)##.tabIndex;
                 (Js.Unsafe.coerce self#root)##.tabIndex := -1;
                 self#set_attribute "aria-disabled" "true"
      | false -> (Js.Unsafe.coerce self#root)##.tabIndex := tab_index;
                 self#remove_attribute "aria-disabled"

    method toggle () : unit =
      on <- not on;
      self#set_attribute "aria-pressed" @@ string_of_bool on;
      Option.iter (fun x -> self#remove_class x) (if on then off_data else on_data).css_class;
      let data = if on then on_data else off_data in
      Option.iter (fun x -> self#add_class x) data.css_class;
      Option.iter (fun x -> self#set_attribute "aria-label" x) data.label;
      self#set_text_content data.icon;
      s_state_push on

    method on       = on
    method set_on x = if not @@ Bool.equal on x then self#toggle ()

    method s_state = s_state

    initializer
      (let r = Ripple.attach self in
       self#add_class "mdc-ripple-surface"; (*FIXME*)
       r##.unbounded := Js.bool true;
       Ripple.set_unbounded self);
      tab_index <- (Js.Unsafe.coerce self#root)##.tabIndex;
      Dom_events.listen self#root Dom_events.Typ.keydown (fun _ e ->
                          if is_space e
                          then (is_key_down <- true; false)
                          else true) |> ignore;
      Dom_events.listen self#root Dom_events.Typ.keyup (fun _ e ->
                          if is_space e
                          then (is_key_down <- false; self#toggle ());
                          true) |> ignore;
      Dom_events.listen self#root Dom_events.Typ.click (fun _ e ->
                          if not propagate then Dom_html.stopPropagation e;
                          self#toggle ();
                          true) |> ignore;

  end
