open Js_of_ocaml
open Components

let color_from_css (elt : #Dom_html.element Js.t) =
  (Dom_html.window##getComputedStyle elt)##.color

class t ?(show_grid_lines = true)
    ?(show_snap_lines = true)
    ?(divider_period = 2)
    ~size
    (canvas : Dom_html.canvasElement Js.t) () =
  object(self)

    inherit Widget.t canvas () as super

    val context = canvas##getContext Dom_html._2d_
    val mutable size = size
    val mutable divider_period = divider_period
    val mutable snap_lines = []
    val mutable show_grid_lines = show_grid_lines
    val mutable show_snap_lines = show_snap_lines

    method! layout () =
      canvas##.width := canvas##.offsetWidth;
      canvas##.height := canvas##.offsetHeight;
      let width = canvas##.width in
      let height = canvas##.height in
      context##clearRect 0. 0. (float_of_int width) (float_of_int height);
      if show_grid_lines then self#draw_grid ~width ~height;
      if show_snap_lines then List.iter (self#draw_snap_line ~width ~height) snap_lines;
      super#layout ()

    method size : int =
      size

    method set_size (x : int) : unit =
      size <- x;
      self#layout ()

    method divider_period : int =
      divider_period

    method set_divider_period (x : int) : unit =
      divider_period <- x;
      self#layout ()

    method set_snap_lines (x : Snap_line.t list) =
      snap_lines <- x;
      self#layout ()

    method grid_lines_visible : bool =
      show_grid_lines

    method set_grid_lines_visible (x : bool) : unit =
      show_grid_lines <- x;
      self#layout ()

    method snap_lines_visible : bool =
      show_snap_lines

    method set_snap_lines_visible (x : bool) : unit =
      show_snap_lines <- x;
      self#layout ()

    (* Private methods *)

    method private draw_snap_line ~width ~height (line : Snap_line.t) : unit =
      let color = match line.is_center, line.is_multiple with
        | true, true -> "rgba(255, 0, 0, 0.9)"
        | false, true -> "rgba(0, 0, 255, 0.9)"
        | true, false -> "rgba(255, 0, 0, 0.4)"
        | false, false -> "rgba(0, 0, 255, 0.4)" in
      context##.strokeStyle := Js.string color;
      let start_x, start_y, end_x, end_y =
        if line.is_vertical then
          line.origin, 0.,
          line.origin, float_of_int height
        else
          0., line.origin,
          float_of_int width, line.origin
      in
      context##.strokeStyle := Js.string color;
      context##beginPath;
      context##moveTo start_x start_y;
      context##lineTo end_x end_y;
      context##closePath;
      context##stroke

    method private draw_rows ~color ~cols ~rows =
      context##.strokeStyle := color;
      let rec f num =
        context##moveTo 0. (float_of_int @@ size * num);
        context##lineTo (float_of_int @@ size * cols) (float_of_int @@ size * num);
        if num + 1 <= rows
        then f (num + 1)
      in
      context##beginPath;
      f 0;
      context##closePath;
      context##stroke

    method private draw_columns ~color ~cols ~rows =
      context##.strokeStyle := color;
      let rec f num =
        context##moveTo (float_of_int @@ size * num) 0.;
        context##lineTo (float_of_int @@ size * num) (float_of_int @@ size * rows);
        if num + 1 <= cols
        then f (num + 1)
      in
      context##beginPath;
      f 0;
      context##closePath;
      context##stroke

    method private draw_dividers ~color ~cols ~rows =
      context##.strokeStyle := color;
      let rec f_1 num =
        context##moveTo 0. (float_of_int @@ size * num);
        context##lineTo (float_of_int @@ size * cols) (float_of_int @@ size * num);
        if num + divider_period <= rows
        then f_1 (num + divider_period)
      in
      let rec f_2 num =
        context##moveTo (float_of_int @@ size * num) 0.;
        context##lineTo (float_of_int @@ size * num) (float_of_int @@ size * rows);
        if num + divider_period <= cols
        then f_2 (num + divider_period)
      in
      context##beginPath;
      f_1 0;
      f_2 0;
      context##closePath;
      context##stroke

    method private draw_grid ~width ~height : unit =
      let cols = (width / size) + 1 in
      let rows = (height / size) + 1 in
      let color = color_from_css canvas in
      context##.lineWidth := 0.3;
      self#draw_rows ~color ~cols ~rows;
      self#draw_columns ~color ~cols ~rows;
      self#draw_dividers ~color ~cols ~rows

  end

let attach ?show_grid_lines ?show_snap_lines ?(size = 10)
    (elt : #Dom_html.element Js.t) : t =
  Js.Opt.case (Dom_html.CoerceTo.canvas elt)
    (fun () -> failwith "grid-overlay: host element must have a `canvas` tag")
    (fun (canvas : Dom_html.canvasElement Js.t) ->
       new t ?show_grid_lines ?show_snap_lines ~size canvas ())
