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
  let elt    = Tyxml_js.Svg.(path ~a:([ a_fill `None
                                      ; a_stroke (`Color ("white",None))
                                      ; a_stroke_width (2., None)])[] |> toelt)
               |> Js.Unsafe.coerce in
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
      let top,height = if left.y > right.y
                       then left.y,  left.y  - right.y
                       else right.y, right.y - left.y
      in
      let width = right.x - left.x in
      let path  = if left.y = right.y
                  then Printf.sprintf "M %d %d L %d %d" left.x left.y right.x right.y
                  else
                    if right.x - left.x < 80
                    then Printf.sprintf "M %d %d C %d %d %d %d %d %d C %d %d %d %d %d %d"
                           left.x left.y
                           left.x left.y (left.x + width/2) left.y (left.x + width/2) (top - height/2)
                           (left.x + width/2) (top - height/2) (left.x + width/2) right.y right.x right.y
                    else Printf.sprintf "M %d %d L %d %d C %d %d %d %d %d %d C %d %d %d %d %d %d"
                           left.x left.y
                           (right.x - 80) left.y
                           (right.x - 80) left.y (right.x - 40) left.y (right.x - 40) (top - height/2)
                           (right.x - 40) (top - height/2) (right.x - 40) (right.y) right.x right.y
      in
      self#set_attribute "d" path

    initializer
      self#set_state state
  end
