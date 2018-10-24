open Containers
open Components
open Topo_types
open Common.Topology
open Lwt.Infix

let get_output_point (elt:#Dom_html.element Js.t) =
  { x = elt##.offsetLeft + elt##.offsetWidth
  ; y = elt##.offsetTop + (elt##.offsetHeight / 2)
  }

let get_input_point ~num i (elt:#Dom_html.element Js.t) =
  let h = elt##.offsetHeight / num in
  let x = elt##.offsetLeft in
  let y = elt##.offsetTop + (i * h) + (h / 2) in
  { x; y }

class switch (node:node_entry) (port:Common.Topology.topo_port) setter () =
  let _class = "topology__switch" in
  let s,push = React.S.create false in
  object(self)

    inherit Switch.t ()

    val mutable _state = `Unavailable
    method port        = port
    method s_changing  = s

    method set_state x =
      _state <- x;
      match (x:connection_state) with
      | `Active | `Sync | `Sync_lost ->
         self#set_disabled false;
         self#set_checked true
      | `Muted ->
         self#set_disabled false;
         self#set_checked false
      | `Unavailable ->
         self#set_disabled true

    method set_changing x =
      if x then self#set_disabled true
      else (match _state with `Unavailable -> () | _ -> self#set_disabled false)

    initializer
      Dom_events.listen self#input_element Dom_events.Typ.change (fun _ _ ->
          push true;
          setter port.port self#checked
          >>= (function
               | Ok _    -> push false; Lwt.return_unit
               | Error _ ->
                  push false;
                  (* return current state back *)
                  self#set_state _state;
                  Lwt.return_unit)
          |> Lwt.ignore_result;
          true)
      |> ignore;
      self#set_disabled true;
      self#add_class _class
  end

class t ~(left_node:node_entry)
        ~(right_node:node_entry)
        ~(right_point:connection_point)
        ~(f_lp:unit->point)
        ~(f_rp:unit -> point)
        ~port_setter
        () =
  let _class        = "topology__path" in
  let active_class  = Markup.CSS.add_modifier _class "active"  in
  let muted_class   = Markup.CSS.add_modifier _class "muted"   in
  let sync_class    = Markup.CSS.add_modifier _class "sync"    in
  let no_sync_class = Markup.CSS.add_modifier _class "no-sync" in
  let switch = match right_point with
    | `Iface _ -> None
    | `Port p  ->
       if not p.switchable then None
       else Some (new switch right_node p port_setter ()) in
  let elt = Tyxml_js.Svg.(
      path ~a:([ a_fill `None
               ; a_stroke (`Color ("white",None))
               ; a_stroke_width (2., None)])[]
      |> toelt) |> Js.Unsafe.coerce in
  object(self)

    inherit Widget.t elt ()

    val mutable state = `Muted

    method left_node = left_node
    method switch    = switch
    method set_state (x:connection_state) =
      state <- x;
      Option.iter (fun sw -> sw#set_state x) self#switch;
      match state with
      | `Muted | `Unavailable ->
         self#add_class muted_class;
         self#remove_class active_class;
         self#remove_class sync_class;
         self#remove_class no_sync_class;
      | `Active ->
         self#add_class active_class;
         self#remove_class muted_class;
         self#remove_class sync_class;
         self#remove_class no_sync_class;
      | `Sync ->
         self#add_class sync_class;
         self#remove_class active_class;
         self#remove_class muted_class;
         self#remove_class no_sync_class;
      | `Sync_lost ->
         self#add_class no_sync_class;
         self#remove_class active_class;
         self#remove_class muted_class;
         self#remove_class sync_class

    method layout () =
      let depth  = elt##id |> Js.to_string |> int_of_string in
      let full_w = elt##offsetWidth in
      let step   =
        if depth > 0 then
          full_w / depth / 4
        else
          80 in
      let left   = f_lp () in
      let right  = f_rp () in
      let top,height =
        if left.y > right.y
        then left.y,  left.y  - right.y
        else right.y, right.y - left.y in
      let () =
        Option.iter (fun sw ->
            let sw_pos = { right with y = right.y - (sw#offset_height / 2)
                                    ; x = right.x + 15 } in
            let top  = Printf.sprintf "%dpx" sw_pos.y in
            let left = Printf.sprintf "%dpx" sw_pos.x in
            sw#style##.top  := Js.string top;
            sw#style##.left := Js.string left) self#switch in
      let width = right.x - left.x in
      let path  =
        if abs (left.y - right.y) < 4
        then Printf.sprintf "M %d %d L %d %d" left.x left.y right.x left.y
        else
          if right.x - left.x < step
          then
            self#_make_straight_path
              left.x left.y
              left.x left.y (left.x + width / 2) left.y
              (left.x + width / 2) (top - height / 2)
              (left.x + width / 2) (top - height / 2)
              (left.x + width / 2) right.y right.x right.y
          else
            self#_make_curved_path
              left.x left.y
              (right.x - step) left.y
              (right.x - step) left.y (right.x - step / 2) left.y
              (right.x - step / 2) (top - height / 2)
              (right.x - step / 2) (top - height / 2)
              (right.x - step / 2) (right.y) right.x right.y in
      self#set_attribute "d" path

    method private _make_straight_path =
      Printf.sprintf "M %d %d C %d %d %d %d %d %d C %d %d %d %d %d %d"

    method private _make_curved_path =
      Printf.sprintf "M %d %d L %d %d C %d %d %d %d %d %d C %d %d %d %d %d %d"

    initializer
      self#add_class _class;
      self#set_state state
  end
