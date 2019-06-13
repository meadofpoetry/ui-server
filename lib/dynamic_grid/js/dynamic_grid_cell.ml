open Js_of_ocaml
open Js_of_ocaml_tyxml
open Dynamic_grid_types

class ['a] cell ?(typ = `Item)
        ~s_col_w
        ~s_row_h
        ~s_grid
        ~(pos : Position.t)
        () =
  let elt = match typ with
    | `Item -> Tyxml_js.To_dom.of_element @@ Markup.create_item ()
    | `Ghost -> Tyxml_js.To_dom.of_element @@ Markup.create_ghost () in
  let s_pos, s_pos_push = React.S.create pos in

  object(self)

    inherit Components.Widget.t elt () as super

    val s_margin : (int * int) React.signal =
      React.S.map (fun x -> x.items_margin) s_grid

    val mutable px_pos : Position.t = Position.empty

    (** API *)

    method! init () : unit =
      super#init ();
      Lwt_react.(
        S.keep @@ S.l3 (fun w h _ ->
            self#set_pos_internal self#pos w h)
          s_col_w s_row_h s_grid)

    method pos : Position.t =
      React.S.value s_pos

    method s_pos : Position.t React.signal =
      s_pos

    method set_pos (pos : Position.t) : unit =
      self#set_pos_internal pos
        (React.S.value s_col_w)
        (React.S.value s_row_h);
      s_pos_push ?step:None pos

    (** Private methods *)

    method private set_pos_internal (pos : Position.t) (w : int) (h : int) : unit =
      self#set_x (pos.x * w);
      self#set_y (pos.y * h);
      self#set_w (pos.w * w);
      self#set_h (pos.h * h)

    method private margin : int * int =
      React.S.value s_margin

    method private px_pos : Position.t =
      px_pos

    method private set_x (x : int) : unit =
      px_pos <- { px_pos with x = x + (fst self#margin) };
      let value = Components.Utils.translate px_pos.x px_pos.y in
      super#root##.style##.transform := Js.string value

    method private set_y (y : int) : unit =
      px_pos <- { px_pos with y = y + (snd self#margin) };
      let value = Components.Utils.translate px_pos.x px_pos.y in
      super#root##.style##.transform := Js.string value

    method private set_w (w : int) : unit =
      let with_margin = w - (fst self#margin) in
      let w = if with_margin < 0 then 0 else with_margin in
      px_pos <- { px_pos with w };
      super#root##.style##.width := Js.string @@ Components.Utils.px px_pos.w

    method private set_h (h : int) : unit =
      let with_margin = h - (snd self#margin) in
      let h = if with_margin < 0 then 0 else with_margin in
      px_pos <- { px_pos with h };
      super#root##.style##.height := Js.string @@ Components.Utils.px px_pos.h
  end
