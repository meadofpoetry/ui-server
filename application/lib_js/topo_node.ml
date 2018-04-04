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

class t ~(body:#Widget.widget) elt () =
object
  inherit Widget.widget elt ()
  method output_point = get_output_point body
end

class path ~(f_lp:unit->point) ~(f_rp:unit -> point) () =
  let ln = Tyxml_js.Svg.line [] in
  let ln_elt = Tyxml_js.Svg.toelt ln |> Js.Unsafe.coerce |> Widget.create in
  let elt = Tyxml_js.Svg.(svg ~a:[ a_width (500.,None)
                                 ; a_height (500.,None) ]
                              [ln])
            |> Tyxml_js.Svg.toelt
            |> Js.Unsafe.coerce in
  object(self)

    inherit Widget.widget elt ()

    method set_state : connection_state -> unit = function
      | `Listening     -> ()
      | `Not_listening -> ()
      | `Sync          -> ()

    method layout =
      let left  = f_lp () in
      let right = f_rp () in
      ln_elt#set_attribute "x1" (string_of_int left.x);
      ln_elt#set_attribute "y1" (string_of_int left.y);
      ln_elt#set_attribute "x2" (string_of_int right.x);
      ln_elt#set_attribute "y2" (string_of_int right.y);
      ln_elt#set_attribute "stroke" "black";

    initializer
      self#style##.position := Js.string "fixed";
      self#style##.left     := Js.string "0px";
      self#style##.top      := Js.string "0px";
      self#set_on_load (Some (fun () -> self#layout));
      Dom_events.(listen Dom_html.window Typ.resize (fun _ _ -> self#layout; true))
      |> ignore

  end

class parent ~(connections:#t list)
             ~(body:#Widget.widget)
             elt
             () =
  let num = List.length connections in
  let cw  = List.mapi (fun i x -> let f_lp = fun () -> x#output_point in
                                  let f_rp = fun () -> get_input_point ~num i body in
                                  new path ~f_lp ~f_rp ()) connections
  in
  object
    inherit t ~body elt ()
    method update_path_state n (state:connection_state) = match List.get_at_idx n cw with
      | Some path -> Ok (path#set_state state)
      | None      -> Error "path not found"
    method paths = cw
  end
