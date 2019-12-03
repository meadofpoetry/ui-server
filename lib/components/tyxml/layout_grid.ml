type cell_align =
  | Top
  | Middle
  | Bottom

type grid_align =
  | Left
  | Right

type device =
  | Desktop
  | Phone
  | Tablet

let align_to_string : cell_align -> string = function
  | Top -> "top"
  | Middle -> "middle"
  | Bottom -> "bottom"

let align_of_string : string -> cell_align option = function
  | "top" -> Some Top
  | "middle" -> Some Middle
  | "bottom" -> Some Bottom
  | _ -> None

let equal_device (a : device) (b : device) : bool =
  match a, b with
  | Desktop, Desktop -> true
  | Phone, Phone -> true
  | Tablet, Tablet -> true
  | _, _ -> false

let device_to_string : device -> string = function
  | Desktop -> "desktop"
  | Phone -> "phone"
  | Tablet -> "tablet"

let device_of_string : string -> device option = function
  | "desktop" -> Some Desktop
  | "phone" -> Some Phone
  | "tablet" -> Some Tablet
  | _ -> None

let grid_align_to_string : grid_align -> string = function
  | Left -> "left"
  | Right -> "right"

let grid_align_of_string : string -> grid_align option = function
  | "left" -> Some Left
  | "right" -> Some Right
  | _ -> None

let max_columns = 12

let check_columns_number_exn n =
  if n > max_columns || n < 0 then failwith "Layout grid: bad columns number"

module CSS = struct
  (** Mandatory, for the layout grid element. *)
  let root = "mdc-layout-grid"

  (** Mandatory, for wrapping grid cell. *)
  let inner = BEM.add_element root "inner"

  let align_prefix = BEM.add_modifier root "align"

  (** Optional, specifies the alignment of the whole grid. *)
  let align (x : grid_align) : string =
    Printf.sprintf "%s-%s" align_prefix (grid_align_to_string x)

  (** Mandatory, for the layout grid cell. *)
  let cell = BEM.add_element root "cell"

  let cell_span_prefix = BEM.add_modifier cell "span"

  (** Optional, specifies the number of columns the cell spans on a type of device
      (desktop, tablet, phone). *)
  let cell_span ?device (n : int) : string =
    check_columns_number_exn n;
    match device with
    | None -> Printf.sprintf "%s-%d" cell_span_prefix n
    | Some d -> Printf.sprintf "%s-%d-%s" cell_span_prefix n (device_to_string d)

  let cell_order_prefix = BEM.add_modifier cell "order"

  (** Optional, specifies the order of the cell. *)
  let cell_order (n : int) : string =
    check_columns_number_exn n;
    Printf.sprintf "%s-%d" cell_order_prefix n

  let cell_align_prefix = BEM.add_modifier cell "align"

  (** Optional, specifies the alignment of cell. *)
  let cell_align (x : cell_align) : string =
    Printf.sprintf "%s-%s" cell_align_prefix (align_to_string x)

  (** Optional, specifies the grid should have fixed column width. *)
  let fixed_column_width = BEM.add_modifier root "fixed-column-width"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let layout_grid_cell
      ?(classes = return [])
      ?(a = [])
      ?align
      ?order
      ?span
      ?span_phone
      ?span_tablet
      ?span_desktop
      ?(children = nil ())
      () =
    let classes =
      fmap
        (Utils.map_cons_option CSS.cell_span span
        % Utils.map_cons_option (CSS.cell_span ~device:Phone) span_phone
        % Utils.map_cons_option (CSS.cell_span ~device:Tablet) span_tablet
        % Utils.map_cons_option (CSS.cell_span ~device:Desktop) span_desktop
        % Utils.map_cons_option CSS.cell_align align
        % Utils.map_cons_option CSS.cell_order order
        % List.cons CSS.cell)
        classes
    in
    div ~a:(a_class classes :: a) children

  let layout_grid_inner ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.inner) classes in
    div ~a:(a_class classes :: a) children

  let layout_grid
      ?(classes = return [])
      ?(a = [])
      ?align
      ?(fixed_column_width = false)
      ?cells
      ?(children = singleton (return (layout_grid_inner ?children:cells ())))
      () =
    let classes =
      fmap
        (Utils.map_cons_option CSS.align align
        % Utils.cons_if fixed_column_width CSS.fixed_column_width
        % List.cons CSS.root)
        classes
    in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
