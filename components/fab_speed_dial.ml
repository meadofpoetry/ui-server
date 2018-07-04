open Containers

type animation = [`Fling | `Scale]
type direction = [`Up | `Down | `Left | `Right]

let base_class   = "mdc-fab-speed-dial"

let main_class    = Markup.CSS.add_element base_class "main"
let action_class  = Markup.CSS.add_element base_class "action"
let actions_class = Markup.CSS.add_element base_class "actions"

let fling_class  = Markup.CSS.add_modifier base_class "animation-fling"
let scale_class  = Markup.CSS.add_modifier base_class "animation-scale"

let up_class     = Markup.CSS.add_modifier base_class "direction-up"
let down_class   = Markup.CSS.add_modifier base_class "direction-down"
let left_class   = Markup.CSS.add_modifier base_class "direction-left"
let right_class  = Markup.CSS.add_modifier base_class "direction-right"

let opened_class = Markup.CSS.add_modifier base_class "opened"

let item_delay = 65

class action ~(z_index:int) (fab:Fab.t) () =
object(self)
  inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()
  method fab = fab
  method z_index = z_index
  initializer
    self#add_class action_class;
    self#style##.zIndex := (Js.string (string_of_int self#z_index));
    Dom.appendChild self#root fab#root
end

class actions ~(items:Fab.t list) () =
  let z     = 20 in
  let items = List.mapi (fun i x -> new action ~z_index:(z + i) x ()) items in
  object(self)
    val mutable _items = items
    inherit Box.t ~vertical:true ~widgets:items ()

    method items = _items

    method add (x:Fab.t) : unit =
      x#set_mini true;
      let action = new action ~z_index:(List.length self#items + z) x () in
      Dom.appendChild self#root action#root;
      _items <- action :: self#items

    method remove (x:Fab.t) : unit =
      match List.find_opt (fun a -> Equal.physical x a#fab) self#items with
      | Some a -> (try Dom.removeChild self#root a#root with _ -> ());
                  _items <- List.remove ~eq:(Equal.physical) ~x:a self#items
      | None   -> ()

    initializer
      self#add_class actions_class
  end

class t ?(animation=`Scale) ?(direction=`Up) ~icon ~items () =
  let main         = new Fab.t ~icon () in
  let main_wrapper = Dom_html.createDiv Dom_html.document |> Widget.create in
  let ()           = Dom.appendChild main_wrapper#root main#root in
  let actions      = new actions ~items () in
  let s,push       = React.S.create false in
  object(self)

    inherit Box.t ~widgets:[main_wrapper#widget;actions#widget] () as super

    val mutable _animation = animation
    val mutable _direction = direction
    val mutable _items     = items

    method s_state = s

    method main  = main

    method items = List.map (fun x -> x#fab) self#actions#items

    method add (x:Fab.t) : unit    = self#actions#add x; self#_set_main_z_index ()
    method remove (x:Fab.t) : unit = self#actions#remove x

    method animation : animation = _animation
    method set_animation (x:animation) =
      self#remove_class (self#_animation_to_class self#animation);
      self#add_class (self#_animation_to_class x);
      _animation <- x

    method direction : direction = _direction
    method set_direction (x:direction) =
      self#remove_class (self#_direction_to_class self#direction);
      self#add_class (self#_direction_to_class x);
      _direction <- x

    method layout () = super#layout ();
                       List.iter (fun x -> x#layout ()) self#items

    method opened  = React.S.value self#s_state
    method show () =
      List.iteri (fun i x -> self#_transform_show i x) self#actions#items;
      Dom_html.setTimeout self#layout
                          (float_of_int (150 + (List.length self#items * item_delay))) |> ignore;
      self#add_class opened_class; push true
    method hide () =
      List.iteri (fun i x -> self#_transform_hide i x) self#actions#items;
      self#remove_class opened_class; push false

    (** Private methods **)

    method private _animation_to_class : animation -> string = function
      | `Fling -> fling_class
      | `Scale -> scale_class

    method private _direction_to_class : direction -> string = function
      | `Up    -> up_class
      | `Down  -> down_class
      | `Left  -> left_class
      | `Right -> right_class

    method private _transform_show = match self#animation with
      | `Fling -> (fun _ (item:action) ->
        item#root##.style##.transform := Js.string "")
      | `Scale -> (fun idx (item:action) ->
        let delay = (idx * item_delay) in
        item#root##.style##.transform := Js.string "scale(1)";
        (Js.Unsafe.coerce item#root##.style)##.transitionDelay := Js.string (Printf.sprintf "%dms" delay))

    method private _transform_hide = match self#animation with
      | `Fling -> (fun _ (item:action) ->
        let offset = match self#direction with
          | `Up ->
             let actions_h = self#actions#offset_height in
             let item_top  = actions_h - item#offset_top in
             let d         = (main_wrapper#offset_height - item#offset_height) / 2 in
             item_top + d
          | `Down ->
             let item_top  = item#offset_top in
             let d         = (main_wrapper#offset_height - item#offset_height) / 2 in
             -(item_top - d)
          | `Left ->
             let actions_w = self#actions#offset_width in
             let item_left = actions_w - item#offset_left in
             let d         = (main_wrapper#offset_width - item#offset_width) / 2 in
             item_left + d
          | `Right ->
             let item_left = item#offset_left in
             let d         = (main_wrapper#offset_width - item#offset_width) / 2 in
             -(item_left - d)
        in
        let translate = match self#direction with
          | `Up | `Down    -> Printf.sprintf "translateY(%dpx)" offset
          | `Left | `Right -> Printf.sprintf "translateX(%dpx)" offset
        in
        item#root##.style##.transform := Js.string translate)
      | `Scale -> (fun idx (item:action) ->
        let delay = 3 - (idx * item_delay) |> Printf.sprintf "%dms" in
        item#root##.style##.transform := Js.string "scale(0)";
        (Js.Unsafe.coerce item#root##.style)##.transitionDelay := Js.string delay)

    method private _set_main_z_index () =
      let z = List.fold_left (fun acc x -> if x#z_index > acc then x#z_index else acc)
                             0 self#actions#items in
      main_wrapper#style##.zIndex := Js.string @@ string_of_int @@ succ z

    method private actions = actions

    initializer
      self#_set_main_z_index ();
      Utils.Keyboard_event.listen self#main#root (function
          | `Escape _ -> self#hide (); true | _ -> true) |> ignore;
      List.iter (fun x -> x#set_mini true) self#items;
      self#hide (); (* FIXME not working for `Fling cause can't get dimensions *)
      self#set_animation animation;
      self#set_direction direction;
      main_wrapper#add_class main_class;
      self#add_class base_class

  end
