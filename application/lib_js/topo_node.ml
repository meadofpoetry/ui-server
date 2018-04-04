open Containers
open Components
open Common.Topology

type connection_state = [ `Active | `Muted | `Sync ]

type point =
  { x : int
  ; y : int
  }

let point_to_string x =
  Printf.sprintf "{ x = %d; y = %d }" x.x x.y

let get_output_point (elt:#Dom_html.element Js.t) =
  let rect = elt##getBoundingClientRect |> Widget.to_rect in
  let y = (int_of_float rect.top) + (elt##.offsetHeight / 2) in
  { x = int_of_float rect.right; y }

let get_input_point ~num i (elt:#Dom_html.element Js.t) =
  let rect = elt##getBoundingClientRect |> Widget.to_rect in
  let h     = elt##.offsetHeight / num in
  let x     = int_of_float rect.left in
  let y     = (int_of_float rect.top) + (i * h) + (h / 2) in
  { x; y }

class t ~body elt () =
object
  inherit Widget.widget elt ()
  method output_point = get_output_point body
end

class path ~(f_lp:unit->point) ~(f_rp:unit -> point) () =
  let _class       = "topology__path" in
  let active_class = Markup.CSS.add_modifier _class "active" in
  let muted_class  = Markup.CSS.add_modifier _class "muted" in
  let sync_class   = Markup.CSS.add_modifier _class "sync" in
  let ln           = Tyxml_js.Svg.line [] in
  let ln_elt       = Tyxml_js.Svg.toelt ln |> Js.Unsafe.coerce |> Widget.create in
  let elt          = Tyxml_js.Svg.(svg ~a:[ a_width (100.,Some `Percent)
                                          ; a_height (100.,Some `Percent) ]
                                     [ln])
                     |> Tyxml_js.Svg.toelt
                     |> Js.Unsafe.coerce in
  object(self)

    inherit Widget.widget elt ()

    val mutable state = `Muted

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
      ln_elt#set_attribute "x1" (string_of_int left.x);
      ln_elt#set_attribute "y1" (string_of_int left.y);
      ln_elt#set_attribute "x2" (string_of_int right.x);
      ln_elt#set_attribute "y2" (string_of_int right.y);

    initializer
      self#add_class _class;
      self#set_state state;
      self#style##.position := Js.string "fixed";
      self#style##.left     := Js.string "0px";
      self#style##.top      := Js.string "0px";
      self#set_on_load (Some (fun () -> self#layout));
      Dom_events.(listen Dom_html.window Typ.resize (fun _ _ -> self#layout; true))
      |> ignore

  end

class parent ~(connections:#t list)
             ~(body:#Dom_html.element Js.t)
             elt
             () =
  let connections = List.rev connections in
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
