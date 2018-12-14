open Containers
open Vg
open Gg
open Dynamic_grid_types

type line = Column | Row

class overlay_grid ~parent ~s_col_w ~s_row_h ~s_cols ~s_rows ~s_im () =
  let elt = Markup.Overlay_grid.create ()
            |> Tyxml_js.To_dom.of_canvas in
  object(self)

    inherit Widget.t elt () as super

    val mutable grid_color = Color.v 0. 0. 0. 0.25
    val mutable divider_color = Color.v 0. 0. 0. 0.5
    val mutable show_dividers = false
    val mutable divider_period = 10,10

    method show () : unit =
      Dom.appendChild parent self#root;
      parent##.classList##add (Js.string Markup.with_overlay_grid_class)

    method hide () : unit =
      (try Dom.removeChild parent self#root with _ -> ());
      parent##.classList##remove (Js.string Markup.with_overlay_grid_class)

    method set_grid_color r g b a =
      grid_color <- Color.v r g b a;
      self#layout ()

    method grid_color = grid_color

    method set_divider_color r g b a =
      divider_color <- Color.v r g b a;
      self#layout ()

    method divider_color =
      divider_color

    method set_divider_period x =
      divider_period <- x; self#layout

    method divider_period =
      divider_period

    method show_dividers () : unit =
      show_dividers <- true

    method hide_dividers () : unit =
      show_dividers <- false

    method! layout () =
      super#layout ();
      let cn = React.S.value s_cols in
      let rn = React.S.value s_rows in
      let cw = React.S.value s_col_w in
      let rh = React.S.value s_row_h in
      let im = React.S.value s_im in
      let empty =
        I.const @@ Color.v 1. 1. 1. 0.
        |> I.cut (P.empty
                  |> P.sub (P2.v 0. 0.)
                  |> P.line (P2.v 0. 1.)
                  |> P.line (P2.v 1. 1.)
                  |> P.line (P2.v 1. 0.)
                  |> P.line (P2.v 0. 0.)) in
      let filler =
        I.const @@ self#grid_color
        |> I.cut (P.empty
                  |> P.sub  (P2.v 0. 0.)
                  |> P.line (P2.v 0. 1.)
                  |> P.line (P2.v 1. 1.)
                  |> P.line (P2.v 1. 0.)
                  |> P.line (P2.v 0. 0.)) in
      let col_border = 1. /. (float_of_int @@ cw * cn) in
      let row_border = 1. /. (float_of_int @@ rh * rn) in
      let one_row_pos = 1. /. float_of_int rn in
      let one_col_pos = 1. /. float_of_int cn in
      let (dividers : image) =
        if not show_dividers || (rn < 30 && cn < 30) then empty else
          let line_dividers_count = List.range 0 (cn / (fst self#divider_period)) in
          let pos = one_col_pos *. float_of_int (fst self#divider_period) in
          let line_dividers =
            List.fold_left (fun acc num ->
                let img = self#draw_line ~pos ~num ~typ:Row ~w:col_border in
                I.blend img acc) empty line_dividers_count in
          let row_dividers_count = List.range 0 (rn / (snd self#divider_period)) in
          let pos = one_row_pos *. float_of_int (snd self#divider_period) in
          List.fold_left (fun acc num ->
              let img = self#draw_line ~pos ~num ~typ:Column ~w:row_border in
              I.blend img acc) line_dividers row_dividers_count in
      let cols_rows =
        if cw > 2
        then (
          let cols_count = List.range 0 cn in
          let (cols : image) =
            List.fold_left (fun acc num ->
                let img =
                  self#draw_line ~pos:one_col_pos
                    ~num
                    ~typ:Row
                    ~w:col_border in
                I.blend img acc) empty cols_count in
          let rows_count = List.range 0 rn in
          let (rows : image) =
            List.fold_left (fun acc num ->
                let img = self#draw_line ~pos:one_row_pos
                            ~num
                            ~typ:Column
                            ~w:row_border in
                I.blend img acc) empty rows_count in
          I.blend cols rows)
        else filler in
      let (final : image) = I.blend dividers cols_rows in
      self#render elt (self#size (cw * cn) (rh * rn)) final;
      elt##.style##.marginLeft := Js.string @@ (string_of_int @@ (fst im)/2)^"px";
      elt##.style##.marginTop := Js.string @@ (string_of_int @@ (snd im)/2)^"px"

    method private size x y =
      Size2.v (float_of_int x *. 0.26458333)
        (float_of_int y *. 0.26458333)

    method private render canvas size image =
      let r = Vgr.create (Vgr_htmlc.target canvas) `Other in
      ignore @@ Vgr.render r (`Image (size, Box2.unit, image));
      ignore @@ Vgr.render r `End

    method private draw_line ~(pos : float)
                     ~(num : int)
                     ~(typ : line)
                     ~(w : float) =
      let num = float_of_int num in
      let area = `O { P.o with P.width = w } in
      let starting, ending =
        match typ with
        | Row -> P2.v (pos *. num) 0., P2.v (pos *. num) 1.
        | Column -> P2.v 0. (1. -. pos *. num), P2.v 1. (1. -. pos *. num) in
      let path = P.empty
                 |> P.sub starting
                 |> P.line ending
      in
      I.const self#grid_color |> I.cut ~area path

  end
