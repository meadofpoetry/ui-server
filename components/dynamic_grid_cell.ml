open Containers
open Dynamic_grid_types

class ['a] cell ?(typ=`Item)
           ~s_col_w
           ~s_row_h
           ~s_grid
           ~(pos: Position.t)
           () =
  let elt = match typ with
    | `Item  -> Markup.Item.create ()       |> Tyxml_js.To_dom.of_element
    | `Ghost -> Markup.Item.create_ghost () |> Tyxml_js.To_dom.of_element
  in
  let s_pos, s_pos_push = React.S.create pos in

  object(self)

    inherit Widget.t elt ()

    val s_margin = React.S.map (fun x -> x.items_margin) s_grid
    val mutable px_pos = Position.empty

    (** API **)

    method pos     : Position.t              = React.S.value s_pos
    method s_pos   : Position.t React.signal = s_pos
    method set_pos : Position.t -> unit      = s_pos_push ?step:None

    (** Private methods **)

    method private margin = React.S.value s_margin

    method private px_pos = px_pos

    method private set_x x =
      px_pos <- { px_pos with x = x + (fst self#margin) };
      self#root##.style##.transform := Js.string @@ Utils.translate px_pos.x px_pos.y
    method private set_y y =
      px_pos <- { px_pos with y = y + (snd self#margin) };
      self#root##.style##.transform := Js.string @@ Utils.translate px_pos.x px_pos.y
    method private set_w w =
      let with_margin = w - (fst self#margin) in
      let w           = if with_margin < 0 then 0 else with_margin in
      px_pos <- { px_pos with w };
      self#root##.style##.width := Js.string @@ Utils.px px_pos.w
    method private set_h h =
      let with_margin = h - (snd self#margin) in
      let h           = if with_margin < 0 then 0 else with_margin in
      px_pos <- { px_pos with h };
      self#root##.style##.height := Js.string @@ Utils.px px_pos.h

    initializer
      React.S.l4 (fun (pos:Position.t) w h _ ->
          self#set_x (pos.x * w);
          self#set_y (pos.y * h);
          self#set_w (pos.w * w);
          self#set_h (pos.h * h)) self#s_pos s_col_w s_row_h s_grid
      |> ignore

  end
