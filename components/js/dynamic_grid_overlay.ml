open Js_of_ocaml
open Dynamic_grid_types

type line = Column | Row

class overlay_grid ~parent ~s_col_w ~s_row_h ~s_cols ~s_rows ~s_im () =
  let elt = Markup.Overlay_grid.create ()
            |> Tyxml_js.To_dom.of_canvas in
  let context = elt##getContext Dom_html._2d_ in
  object(self)

    inherit Widget.t elt ()

    val mutable show_dividers = true
    val mutable divider_period = 10,10
    val mutable grid_color = "rgba(0, 0, 0, 0.35)"
    val mutable divider_color = "rgba(0, 0, 0, 0.5)"

    method! init () : unit =
      ignore s_col_w;
      ignore s_row_h;
      ignore s_cols;
      ignore s_rows;
      ignore s_im;

    method show () : unit =
      Dom.appendChild parent self#root;
      parent##.classList##add (Js.string Markup.with_overlay_grid_class)

    method hide () : unit =
      (try Dom.removeChild parent self#root with _ -> ());
      parent##.classList##remove (Js.string Markup.with_overlay_grid_class)

    method set_grid_color color =
      let color = Color.to_css_rgba color in
      grid_color <- color;
      self#layout ()

    method set_divider_color color =
      let color = Color.to_css_rgba color in
      divider_color <- color;
      self#layout ()

    method set_default_colors =
      divider_color <- "rgba(0, 0, 0, 0.5)";
      grid_color <- "rgba(0, 0, 0, 0.35)";
      self#layout()


    method divider_color = divider_color

    method grid_color = grid_color

    method set_divider_period x =
      divider_period <- x;
      self#layout

    method divider_period =
      divider_period

    method show_dividers () : unit =
      show_dividers <- true

    method hide_dividers () : unit =
      show_dividers <- false

    method draw_rows ~cw ~cn ~rh ~rn =
      context##.strokeStyle := Js.string grid_color;
      let rec f num =
        context##moveTo 0. (float_of_int @@ rh * num);
        context##lineTo (float_of_int @@ cw * cn) (float_of_int @@ rh * num);
        if num + 1 <= rn
        then f (num + 1)
      in
      context##beginPath;
      f 0;
      context##closePath;
      context##stroke

    method draw_columns ~cw ~cn ~rh ~rn =
      context##.strokeStyle := Js.string grid_color;
      let rec f num =
        context##moveTo (float_of_int @@ cw * num) 0.;
        context##lineTo (float_of_int @@ cw * num) (float_of_int @@ rh * rn);
        if num + 1 <= cn
        then f (num + 1)
      in
      context##beginPath;
      f 0;
      context##closePath;
      context##stroke

    method draw_dividers ~cw ~cn ~rh ~rn =
      context##.strokeStyle := Js.string divider_color;
      let rec f_1 num =
        context##moveTo 0. (float_of_int @@ rh * num);
        context##lineTo (float_of_int @@ cw * cn) (float_of_int @@ rh * num);
        if num + snd divider_period <= rn
        then f_1 (num + snd divider_period)
      in
      let rec f_2 num =
        context##moveTo (float_of_int @@ cw * num) 0.;
        context##lineTo (float_of_int @@ cw * num) (float_of_int @@ rh * rn);
        if num + fst divider_period <= cn
        then f_2 (num + fst divider_period)
      in
      context##beginPath;
      f_1 0;
      f_2 0;
      context##closePath;
      context##stroke


    method! layout () =
      let cn = React.S.value s_cols in
      let rn = React.S.value s_rows in
      let cw = React.S.value s_col_w in
      let rh = React.S.value s_row_h in
      let im = React.S.value s_im in
      elt##.width := cw * cn;
      elt##.height := rh * rn;
      (* clearing the context before drawing again *)
      context##clearRect 0. 0. (float_of_int @@ cw * cn) (float_of_int @@ rn * rh);
      context##.lineWidth := 1.;
      self#draw_rows ~cw ~cn ~rh ~rn;
      self#draw_columns ~cw ~cn ~rh ~rn;
      if show_dividers && rn >= 30 && cn >= 30
      then self#draw_dividers ~cw ~cn ~rh ~rn;
      elt##.style##.marginLeft := Js.string @@ string_of_int ((fst im) / 2) ^ "px";
      elt##.style##.marginTop  := Js.string @@ string_of_int ((snd im) / 2) ^ "px"

  end
