open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Fab_speed_dial.Make(Xml)(Svg)(Html)

type animation = [`Fling | `Scale]
type direction = [`Up | `Down | `Left | `Right]

let item_delay = 65

class action ~(z_index : int) (fab : Fab.t) () =
object

  inherit Widget.t Dom_html.(createDiv document) () as super

  method! init () : unit =
    super#init ();
    super#add_class Markup.action_class;
    super#style##.zIndex := (Js.string (string_of_int z_index));
    super#append_child fab

  method fab : Fab.t =
    fab

  method z_index : int =
    z_index

end

class actions ~(items : Fab.t list) () =
  let z = 20 in
  let items = List.mapi (fun i x -> new action ~z_index:(z + i) x ()) items in
  object(self)
    val mutable _items = items
    inherit Vbox.t ~widgets:items () as super

    method! init () : unit =
      super#init ();
      super#add_class Markup.actions_class

    method items = _items

    method prepend (x : Fab.t) : unit =
      let action = self#wrap_action x in
      super#insert_child_at_idx 0 action;
      _items <- action :: self#items

    method append (x : Fab.t) : unit =
      let action = self#wrap_action x in
      super#append_child action;
      _items <- self#items @ [action]

    method insert_at_index (idx : int) (x : Fab.t) : unit =
      let action = self#wrap_action x in
      super#insert_child_at_idx idx action;
      _items <- List.insert_at_idx idx action _items

    method remove (fab : Fab.t) : unit =
      match List.find_opt (fun a -> Equal.physical fab a#fab) self#items with
      | None -> ()
      | Some x ->
         self#remove_child x;
         _items <- List.remove ~eq:Widget.equal x self#items

    method private wrap_action (x : Fab.t) : action =
      x#set_mini true;
      new action ~z_index:(List.length self#items + z) x ()

  end

class t ?(animation = `Scale) ?(direction = `Up) ~icon ~items () =
  let main = new Fab.t ~icon () in
  let main_wrapper = Widget.create_div () in
  let actions = new actions ~items () in
  let s, push = React.S.create false in
  let box = new Vbox.t ~widgets:[main_wrapper#widget; actions#widget] () in
  object(self)

    inherit Widget.t box#root () as super

    val mutable _keydown_listener = None
    val mutable _animation = animation
    val mutable _direction = direction
    val mutable _items = items

    method! init () : unit =
      super#init ();
      main_wrapper#append_child main;
      self#_set_main_z_index ();
      let listener =
        self#listen_lwt Widget.Event.keydown (fun e _ ->
            match Utils.Keyboard_event.event_to_key e with
            | `Escape -> Lwt.return @@ self#hide ()
            | _ -> Lwt.return ()) in
      _keydown_listener <- Some listener;
      List.iter (fun x -> x#set_mini true) self#items;
      self#hide (); (* FIXME not working for `Fling cause can't get dimensions *)
      self#set_animation animation;
      self#set_direction direction;
      main_wrapper#add_class Markup.main_class;
      self#add_class Markup.base_class

    method! layout () : unit =
      super#layout ();
      List.iter (fun x -> x#layout ()) self#items

    method! destroy () : unit =
      super#destroy ();
      Option.iter Lwt.cancel _keydown_listener;
      _keydown_listener <- None

    method s_state = s

    method main = main

    method items =
      List.map (fun x -> x#fab) self#actions#items

    method append (x : Fab.t) : unit =
      self#actions#append x;
      self#_set_main_z_index ()

    method prepend (x : Fab.t) : unit =
      self#actions#prepend x;
      self#_set_main_z_index ()

    method insert_at_index (idx : int) (x : Fab.t) : unit =
      self#actions#insert_at_index idx x;
      self#_set_main_z_index ()

    method remove (x : Fab.t) : unit =
      self#actions#remove x

    method animation : animation =
      _animation

    method set_animation (x : animation) : unit =
      self#remove_class (self#_animation_to_class self#animation);
      self#add_class (self#_animation_to_class x);
      _animation <- x

    method direction : direction =
      _direction

    method set_direction (x : direction) : unit =
      self#remove_class (self#_direction_to_class self#direction);
      self#add_class (self#_direction_to_class x);
      _direction <- x

    method opened : bool =
      React.S.value self#s_state

    method show () : unit =
      List.iteri (fun i x -> self#_transform_show i x) self#actions#items;
      Dom_html.setTimeout self#layout
                          (float_of_int (150 + (List.length self#items * item_delay))) |> ignore;
      self#add_class Markup.opened_class;
      push true

    method hide () : unit =
      List.iteri (fun i x -> self#_transform_hide i x) self#actions#items;
      self#remove_class Markup.opened_class; push false

    (** Private methods **)

    method private _animation_to_class : animation -> string = function
      | `Fling -> Markup.fling_class
      | `Scale -> Markup.scale_class

    method private _direction_to_class : direction -> string = function
      | `Up -> Markup.up_class
      | `Down -> Markup.down_class
      | `Left -> Markup.left_class
      | `Right -> Markup.right_class

    method private _transform_show = match self#animation with
      | `Fling -> (fun _ (item : action) ->
        item#root##.style##.transform := Js.string "")
      | `Scale -> (fun idx (item : action) ->
        let delay = (idx * item_delay) in
        item#root##.style##.transform := Js.string "scale(1)";
        (Js.Unsafe.coerce item#root##.style)##.transitionDelay :=
          Js.string (Printf.sprintf "%dms" delay))

    method private _transform_hide = match self#animation with
      | `Fling -> (fun _ (item : action) ->
        let offset = match self#direction with
          | `Up ->
             let actions_h = self#actions#offset_height in
             let item_top = actions_h - item#offset_top in
             let d = (main_wrapper#offset_height - item#offset_height) / 2 in
             item_top + d
          | `Down ->
             let item_top = item#offset_top in
             let d = (main_wrapper#offset_height - item#offset_height) / 2 in
             -(item_top - d)
          | `Left ->
             let actions_w = self#actions#offset_width in
             let item_left = actions_w - item#offset_left in
             let d = (main_wrapper#offset_width - item#offset_width) / 2 in
             item_left + d
          | `Right ->
             let item_left = item#offset_left in
             let d = (main_wrapper#offset_width - item#offset_width) / 2 in
             -(item_left - d)
        in
        let translate = match self#direction with
          | `Up | `Down -> Printf.sprintf "translateY(%dpx)" offset
          | `Left | `Right -> Printf.sprintf "translateX(%dpx)" offset
        in
        item#root##.style##.transform := Js.string translate)
      | `Scale -> (fun idx (item : action) ->
        let delay = 3 - (idx * item_delay) |> Printf.sprintf "%dms" in
        item#root##.style##.transform := Js.string "scale(0)";
        (Js.Unsafe.coerce item#root##.style)##.transitionDelay := Js.string delay)

    method private _set_main_z_index () =
      let z = List.fold_left (fun acc x ->
                  if x#z_index > acc then x#z_index else acc)
                0 self#actions#items in
      main_wrapper#style##.zIndex := Js.string @@ string_of_int @@ succ z

    method private actions = actions

  end
