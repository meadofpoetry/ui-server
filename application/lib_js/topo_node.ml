open Containers
open Components
open Common.Topology

type connection_state = [ `Listening | `Not_listening | `Sync ]

type point =
  { x : int
  ; y : int
  }

let point_to_string x =
  Printf.sprintf "{ x = %d; y = %d }" x.x x.y

let get_output_point widget =
  let rect : Widget.rect = widget#get_bounding_client_rect in
  let y = (int_of_float rect.top) + (widget#get_offset_height / 2) in
  { x = int_of_float rect.right; y }

let get_input_point ~num i widget =
  let rect : Widget.rect = widget#get_bounding_client_rect in
  let h     = widget#get_offset_height / num in
  let x     = int_of_float rect.left in
  let y     = (int_of_float rect.top) + (i * h) + (h / 2) in
  { x; y }

class t (body:#Dom_html.element Js.t) () =
object(self)
  inherit Widget.widget body ()
  method output_point = get_output_point self
end

class path (left_node:#t) (r,r_push) () =
  let s_left,s_left_push = React.S.create left_node#output_point in
  let ln = Tyxml_js.Svg.line [] in
  let ln_elt = Tyxml_js.Svg.toelt ln |> Js.Unsafe.coerce |> Widget.create in
  let elt = Tyxml_js.Svg.(svg ~a:[ a_width (500.,None)
                                 ; a_height (500.,None) ]
                              [ln])
            |> Tyxml_js.Svg.toelt
            |> Js.Unsafe.coerce in
  object(self)

    inherit Widget.widget elt ()

    method push_left  : point -> unit = s_left_push ?step:None
    method push_right : point -> unit = r_push ?step:None

    method s_left  : point React.signal = s_left
    method s_right : point React.signal = r

    method set_state : connection_state -> unit = function
      | `Listening     -> ()
      | `Not_listening -> ()
      | `Sync          -> ()

    initializer
      self#style##.position := Js.string "fixed";
      self#style##.left     := Js.string "0px";
      self#style##.top      := Js.string "0px";
      React.S.l2 (fun l r -> Printf.printf "left: %s, right: %s\n" (point_to_string l) (point_to_string r);
                             ln_elt#set_attribute "x1" (string_of_int l.x);
                             ln_elt#set_attribute "y1" (string_of_int l.y);
                             ln_elt#set_attribute "x2" (string_of_int r.x);
                             ln_elt#set_attribute "y2" (string_of_int r.y);
                             ln_elt#set_attribute "stroke" "black";
                             )
                 self#s_left self#s_right |> ignore;
      Dom_events.(listen Dom_html.window Typ.resize (fun _ _ ->
                           self#push_left left_node#output_point;
                           true))
      |> ignore

  end

class parent ~(connections:#t list)
             (widget:#Widget.widget)
             () =
  let num = List.length connections in
  let cw  = List.mapi (fun i x -> let sr = React.S.create @@ get_input_point ~num i widget in
                                  new path x sr ()) connections
  in
  object
    method update_path_state n (state:connection_state) = match List.get_at_idx n cw with
      | Some path -> Ok (path#set_state state)
      | None      -> Error "path not found"
    method paths = cw
    initializer
      Dom_events.(listen Dom_html.window Typ.resize (fun _ _ ->
                           List.iteri (fun i c -> c#push_right (get_input_point ~num i widget)) cw;
                           true))
      |> ignore
  end
