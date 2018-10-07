open Containers
open Tyxml_js

module Markup = Components_markup.Tab_bar.Make(Xml)(Svg)(Html)

let ( % ) = Fun.( % )

class ['a, 'b] t ?(use_auto_activation = true)
        ?on_change
        ?align
        ~(tabs : ('a, 'b) Tab.t list)
        () =
  let scroller = new Tab_scroller.t ?on_change ?align ~tabs () in
  let elt = Markup.create ~scroller:(Widget.to_markup scroller) ()
            |> To_dom.of_element in

  object(self)

    inherit Widget.t elt ()

    val mutable _use_auto_activation = use_auto_activation

    method scroller : ('a, 'b) Tab_scroller.t =
      scroller

    method tabs = scroller#tabs

    (* Private methods *)

    method handle_key_down e =
      match Utils.Keyboard_event.event_to_key e with
      | `Unknown -> ()
      | key ->
         if self#is_activation_key key
         then Dom.preventDefault e;
         let origin =
           Option.get_or
             ~default:0
             self#scroller#active_tab_index in
         if _use_auto_activation
         then
           if self#is_activation_key key then () else
             let index = self#determine_target_from_key origin key in
             Option.iter (ignore % self#scroller#set_active_tab_index) index
         else () (* TODO implement *)

    method is_activation_key : Utils.Keyboard_event.key_name -> bool = function
      | `Space | `Enter -> true
      | _ -> false

    method determine_target_from_key (index : int)
             (event : Utils.Keyboard_event.key_name) : int option =
      let max_index = (List.length self#tabs) - 1 in
      let index = match event with
        | `End -> Some max_index
        | `Arrow_left -> Some (index - 1)
        | `Arrow_right -> Some (index + 1)
        | `Home -> Some 0
        | _ -> None in
      Option.map (fun x ->
          if x < 0 then max_index
          else if x > max_index then 0
          else x) index

    initializer
      self#listen_lwt Widget.Event.keydown (fun e _ ->
          self#handle_key_down e;
          Lwt.return_unit) |> Lwt.ignore_result

  end
