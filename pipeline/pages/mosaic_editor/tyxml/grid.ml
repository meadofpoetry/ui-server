open Components_tyxml

type value = Auto | Px of float | Fr of float | Pc of float

type cell_position = { col : int; row : int; col_span : int; row_span : int }

let make_cell_position ?(col_span = 1) ?(row_span = 1) ~col ~row () =
  { col; row; col_span; row_span }

let cell_position_to_string (pos : cell_position) =
  Printf.sprintf "%s / %s / span %s / span %s" (string_of_int pos.row)
    (string_of_int pos.col)
    (string_of_int pos.row_span)
    (string_of_int pos.col_span)

let split_string ~suffix pattern =
  let pattern_len = String.length pattern in
  let len = String.length suffix in
  if len > pattern_len then None
  else
    let sub = String.sub pattern (pattern_len - len) len in
    if String.uppercase_ascii sub = String.uppercase_ascii suffix then
      Some (String.sub pattern 0 (pattern_len - len))
    else None

let value_to_string = function
  | Auto -> "auto"
  | Px x -> Printf.sprintf "%gpx" x
  | Fr x -> Printf.sprintf "%gfr" x
  | Pc x -> Printf.sprintf "%g%%" x

let value_of_string : string -> value = function
  | "auto" -> Auto
  | s ->
      let rec aux = function
        | [] -> failwith @@ Printf.sprintf "parse: unknown unit (%s)" s
        | suffix :: tl -> (
            match split_string ~suffix s with
            | None -> aux tl
            | Some x -> (
                match float_of_string_opt x with
                | None -> failwith @@ Printf.sprintf "parse: bad value (%s)" x
                | Some x -> (
                    match suffix with
                    | "px" -> Px x
                    | "fr" -> Fr x
                    | "%" -> Pc x
                    | s ->
                        failwith @@ Printf.sprintf "parse: unknown unit (%s)" s
                    ) ) )
      in
      aux [ "px"; "fr"; "%" ]

let value_of_string_opt (s : string) : value option =
  try Some (value_of_string s) with _ -> None

let rec loop f acc = function 0 -> acc | n -> loop f (f n :: acc) (pred n)

let gen_template ?(size = Fr 1.) (rows : int) =
  String.concat " " @@ loop (fun _ -> value_to_string size) [] rows

type property = [ `Repeat of int * value | `Value of value list ]

let property_to_string : property -> string = function
  | `Repeat (n, v) -> gen_template ~size:v n
  | `Value v -> String.concat " " @@ List.map value_to_string v

module CSS = struct
  let root = "grid"

  let cell = BEM.add_element root "cell"

  let col_handle = BEM.add_element root "col-handle"

  let row_handle = BEM.add_element root "row-handle"

  let mul_handle = BEM.add_element root "mul-handle"

  let cell_selected = BEM.add_modifier cell "selected"

  let cell_dragging_column = BEM.add_modifier cell "dragging-column"

  let cell_dragging_row = BEM.add_modifier cell "dragging-row"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_cell ?(classes = []) ?(a = []) ?(content = []) position : 'a elt =
    let classes = CSS.cell :: classes in
    let style =
      Printf.sprintf "grid-area: %s;" (cell_position_to_string position)
    in
    div
      ~a:
        ( a_class classes
        :: a_draggable true
        :: a_style style
        :: a_aria "colindex" [ string_of_int position.col ]
        :: a_aria "rowindex" [ string_of_int position.row ]
        :: a_aria "colspan" [ string_of_int position.col_span ]
        :: a_aria "rowspan" [ string_of_int position.row_span ]
        :: a_role [ "gridcell" ]
        :: a )
      ( [
          div ~a:[ a_class [ CSS.row_handle ] ] [];
          div ~a:[ a_class [ CSS.col_handle ] ] [];
          div ~a:[ a_class [ CSS.mul_handle ] ] [];
        ]
      @ content )

  let create ?(classes = []) ?(a = []) ?(rows : property option)
      ?(cols : property option) ?(content = []) () : 'a elt =
    let classes = CSS.root :: classes in
    let style =
      match (rows, cols) with
      | None, None -> None
      | Some rows, None ->
          Some
            (Printf.sprintf "grid-template-rows: %s" @@ property_to_string rows)
      | None, Some cols ->
          Some
            ( Printf.sprintf "grid-template-columns: %s"
            @@ property_to_string cols )
      | Some rows, Some cols ->
          Some
            (Printf.sprintf "grid-template-rows: %s; grid-template-columns: %s"
               (property_to_string rows) (property_to_string cols))
    in
    div
      ~a:
        ( a_class classes :: a_role [ "grid" ] :: a
        |> Utils.map_cons_option a_style style )
      content
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
