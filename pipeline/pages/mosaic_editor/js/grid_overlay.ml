open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

let color_from_css (elt : #Dom_html.element Js.t) =
  (Dom_html.window##getComputedStyle elt)##.color

class t ?(divider_period = 2) ~size (canvas : Dom_html.canvasElement Js.t) () =
  object(self)

    inherit Widget.t canvas () as super

    val context = canvas##getContext Dom_html._2d_
    val mutable show_dividers = true
    val mutable divider_period = divider_period
    val mutable snap_lines = []

    method! layout () =
      canvas##.width := canvas##.offsetWidth;
      canvas##.height := canvas##.offsetHeight;
      let width = canvas##.width in
      let height = canvas##.height in
      context##clearRect 0. 0. (float_of_int width) (float_of_int height);
      self#draw_grid ~width ~height;
      List.iter self#draw_snap_line snap_lines;
      super#layout ()

    method show () : unit =
      super#root##.style##.display := Js.string ""

    method hide () : unit =
      super#root##.style##.display := Js.string "none"

    method divider_period : int =
      divider_period

    method set_divider_period (x : int) : unit =
      divider_period <- x;
      self#layout ()

    method show_dividers () : unit =
      show_dividers <- true;
      self#layout ()

    method hide_dividers () : unit =
      show_dividers <- false;
      self#layout ()

    method set_snap_lines (x : Resizable.Sig.line list) =
      snap_lines <- x;
      self#layout ()

    (* Private methods *)

    method private draw_snap_line (line : Resizable.Sig.line) : unit =
      ()

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
      let cols = width / size in
      let rows = height / size in
      let color = color_from_css canvas in
      context##.lineWidth := 1.;
      self#draw_rows ~color ~cols ~rows;
      self#draw_columns ~color ~cols ~rows;
      if show_dividers then self#draw_dividers ~color ~cols ~rows

  end

let make size : t =
  let canvas =
    Tyxml_js.To_dom.of_canvas
    @@ Markup.create_grid_overlay () in
  new t ~size canvas ()

(* TODO read size from DOM attribute *)
let attach ?(size = 10) (elt : #Dom_html.element Js.t) : t =
  match String.uppercase_ascii @@ Js.to_string elt##.tagName with
  | "CANVAS" -> new t ~size (Js.Unsafe.coerce elt) ()
  | _ -> failwith "grid-overlay: host element must have a `canvas` tag"
