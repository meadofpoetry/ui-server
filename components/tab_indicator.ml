open Tyxml_js
open Lwt.Infix

module Markup = Components_markup.Tab_indicator.Make(Xml)(Svg)(Html)


let sliding_activate_class =
  Components_markup.CSS.add_modifier Markup.base_class "sliding-activate"

class t ?(fade = false) ?(active = false) () =
  let content =
    Markup.create_content ()
    |> To_dom.of_element
    |> Widget.create in
  let elt =
    Markup.create (Widget.to_markup content) ()
    |> To_dom.of_element in
  object(self : 'self)
    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      (* FIXME *)
      ignore fade;
      ignore active;

    method content = content

    method fade : bool =
      self#has_class Markup.fade_class
    method set_fade (x : bool) : unit =
      self#add_or_remove_class x Markup.fade_class

    method active : bool =
      self#has_class Markup.active_class
    method set_active ?(previous : 'self option) (x : bool) : unit =
      if x
      then (if self#fade
            then () (* FIXME *)
            else self#_activate_slide previous)
      else (if self#fade
            then () (* FIXME *)
            else self#_deactivate_slide ())

    (* Private methods *)

    method private _deactivate_slide () =
      self#remove_class Markup.active_class;
      self#remove_class sliding_activate_class

    method private _activate_slide (prev:'self option) =
      self#add_class Markup.active_class;
      match prev with
      | None -> ()
      | Some prev ->
         let prev_rect = prev#content#bounding_client_rect in
         let cur_rect  = self#content#bounding_client_rect in
         let width_delta = match prev_rect.width, cur_rect.width with
           | Some pw, Some cw -> pw /. cw
           | _ -> 1. (* FIXME*) in
         let x_position  = prev_rect.left -. cur_rect.left in
         let s = Printf.sprintf "translateX(%gpx) scaleX(%g)"
                   x_position width_delta in
         content#style##.transform := Js.string s;
         (* Repaint *)
         self#content#bounding_client_rect |> ignore;
         let wnd = Dom_html.window in
         let f = fun _ ->
           self#add_class sliding_activate_class;
           content#style##.transform := Js.string "" in
         ignore @@ wnd##requestAnimationFrame (Js.wrap_callback f);
         Lwt_js_events.transitionend self#root
         >|= (fun () -> self#remove_class sliding_activate_class)
         |> Lwt.ignore_result

  end
