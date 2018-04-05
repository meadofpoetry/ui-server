open Containers
open Components
open Topo_types

let get_output_point (elt:#Dom_html.element Js.t) =
  { x = elt##.offsetLeft + elt##.offsetWidth
  ; y = elt##.offsetTop + (elt##.offsetHeight / 2)
  }

let get_input_point ~num i (elt:#Dom_html.element Js.t) =
  let h = elt##.offsetHeight / num in
  let x = elt##.offsetLeft in
  let y = elt##.offsetTop + (i * h) + (h / 2) in
  { x; y }

class t ~(left_node:node_entry) ~(f_lp:unit->point) ~(f_rp:unit -> point) () =
  let _class       = "topology__path" in
  let active_class = Markup.CSS.add_modifier _class "active" in
  let muted_class  = Markup.CSS.add_modifier _class "muted"  in
  let sync_class   = Markup.CSS.add_modifier _class "sync"   in
  let elt          = Tyxml_js.Svg.(line [] |> toelt) |> Js.Unsafe.coerce in
  object(self)

    inherit Widget.widget elt ()

    val mutable state = `Muted

    method left_node = left_node
    method set_state (x:connection_state) =
      state <- x;
      match state with
      | `Active -> self#add_class active_class;
                   self#remove_class muted_class;
                   self#remove_class sync_class;
      | `Muted  -> self#add_class muted_class;
                   self#remove_class active_class;
                   self#remove_class sync_class
      | `Sync   -> self#add_class sync_class;
                   self#remove_class active_class;
                   self#remove_class muted_class

    method layout =
      let left  = f_lp () in
      let right = f_rp () in
      self#set_attribute "x1" (string_of_int left.x);
      self#set_attribute "y1" (string_of_int left.y);
      self#set_attribute "x2" (string_of_int right.x);
      self#set_attribute "y2" (string_of_int right.y);

    initializer
      self#add_class _class;
      self#set_state state;
      self#set_on_load (Some (fun () -> self#layout));
      Dom_events.(listen Dom_html.window Typ.resize (fun _ _ -> self#layout; true))
      |> ignore

  end
