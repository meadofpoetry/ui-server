open Containers

type animation = [`Fling | `Scale]
type direction = [`Up | `Down | `Left | `Right]

let base_class   = "mdc-fab-speed-dial"

let main_class    = Markup.CSS.add_element base_class "main"
let actions_class = Markup.CSS.add_element base_class "actions"

let fling_class  = Markup.CSS.add_modifier base_class "animation-fling"
let scale_class  = Markup.CSS.add_modifier base_class "animation-scale"

let up_class     = Markup.CSS.add_modifier base_class "direction-up"
let down_class   = Markup.CSS.add_modifier base_class "direction-down"
let left_class   = Markup.CSS.add_modifier base_class "direction-left"
let right_class  = Markup.CSS.add_modifier base_class "direction-right"

let opened_class = Markup.CSS.add_modifier base_class "opened"

let item_delay = 65

class actions ~(items:Fab.t list) () =
  object(self)
    inherit Box.t ~vertical:true ~widgets:items ()
    initializer
      self#add_class actions_class
  end

class t ?(animation=`Scale) ?(direction=`Up) ~icon ~items () =
  let fab     = new Fab.t ~icon () in
  let actions = new actions ~items () in
  let s,push  = React.S.create false in
  object(self)

    inherit Box.t ~widgets:[fab#widget;actions#widget] ()

    val mutable _animation = animation
    val mutable _direction = direction
    val mutable _items     = items

    method s_state = s

    method main  = fab

    method items = _items

    method add (x:Fab.t)         = x#set_mini true;
                                   Dom.appendChild self#actions#root x#root;
                                   _items <- x :: self#items
    method remove (x:Fab.t)      = (try Dom.removeChild self#actions#root x#root with _ -> ());
                                   _items <- List.remove ~eq:(Equal.physical) ~x self#items
    method remove_at_idx (x:int) = match List.get_at_idx x self#items with
      | Some i -> (try Dom.removeChild self#actions#root i#root with _ -> ());
                  _items <- List.remove ~eq:(Equal.physical) ~x:i self#items
      | None   -> ()

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

    method show () =
      List.iteri (fun i x -> self#_transform_show i x; x#layout ()) self#items;
      (* Dom_html.setTimeout (fun () -> List.iter (fun x -> x#layout ()) self#items)
       *                     (float_of_int (150 + (List.length self#items * item_delay))) |> ignore; *)
      self#add_class opened_class; push true
    method hide () =
      List.iteri (fun i x -> self#_transform_hide i x) self#items;
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
      | `Fling -> (fun idx item ->
        item#root##.style##.transform := Js.string "")
      | `Scale -> (fun idx (item:Fab.t) ->
        let delay = (idx * item_delay) in
        item#root##.style##.transform := Js.string "scale(1)";
        (Js.Unsafe.coerce item#root##.style)##.transitionDelay := Js.string (Printf.sprintf "%dms" delay))

    method private _transform_hide = match self#animation with
      | `Fling -> (fun idx item -> ())
      | `Scale -> (fun idx (item:Fab.t) ->
        let delay = 3 - (idx * item_delay) in
        item#root##.style##.transform := Js.string "scale(0)";
        (Js.Unsafe.coerce item#root##.style)##.transitionDelay := Js.string (Printf.sprintf "%dms" delay))

    method private actions = actions

    initializer
      Utils.Keyboard_event.listen ~f:(function `Escape _ -> self#hide () | _ -> ()) self#main#root |> ignore;
      List.iter (fun x -> x#set_mini true) self#items;
      self#hide ();
      self#set_animation animation;
      self#set_direction direction;
      self#main#add_class main_class;
      self#add_class base_class

  end
