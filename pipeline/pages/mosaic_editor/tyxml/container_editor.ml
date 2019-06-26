open Components_tyxml

type value =
  | Auto
  | Px of float
  | Fr of float
  | Pc of float

let split_string ~suffix pattern =
  let pattern_len = String.length pattern in
  let len = String.length suffix in
  if len > pattern_len
  then None
  else
    let sub = String.sub pattern (pattern_len - len) len in
    if String.uppercase_ascii sub = String.uppercase_ascii suffix
    then Some (String.sub pattern 0 (pattern_len - len))
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
      | suffix :: tl ->
        match split_string ~suffix s with
        | None -> aux tl
        | Some x ->
          match float_of_string_opt x with
          | None -> failwith @@ Printf.sprintf "parse: bad value (%s)" x
          | Some x ->
            match suffix with
            | "px" -> Px x
            | "fr" -> Fr x
            | "%" -> Pc x
            | s -> failwith @@ Printf.sprintf "parse: unknown unit (%s)" s
    in
    aux ["px"; "fr"; "%"]

let value_of_string_opt (s : string) : value option =
  try Some (value_of_string s) with _ -> None

let rec loop f acc = function
  | 0 -> acc
  | n -> loop f ((f n) :: acc) (pred n)

let gen_template ?(size = Fr 1.) (rows : int) =
  String.concat " " @@ loop (fun _ -> value_to_string size) [] rows

module CSS = struct
  let root = "container-editor"

  let grid = "container-grid"

  let cell = BEM.add_element grid "cell"

  let col_handle = BEM.add_element grid "col-handle"
  let row_handle = BEM.add_element grid "row-handle"
  let mul_handle = BEM.add_element grid "mul-handle"

  let cell_selected = BEM.add_modifier cell "selected"
  let cell_dragging_column = BEM.add_modifier cell "dragging-column"
  let cell_dragging_row = BEM.add_modifier cell "dragging-row"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  module Card = Card.Make(Xml)(Svg)(Html)

  let create_grid_cell ?(classes = []) ?attrs ?row ?col ?(content = []) () : 'a elt =
    let classes = CSS.cell :: classes in
    let style = match row, col with
      | None, None -> None
      | Some row, None -> Some (Printf.sprintf "grid-row: %d" row)
      | None, Some col -> Some (Printf.sprintf "grid-column: %d" col)
      | Some row, Some col ->
        Some (Printf.sprintf "grid-row: %d; grid-column: %d" row col)
    in
    div ~a:([a_class classes] <@> attrs
            |> map_cons_option a_style style
            |> map_cons_option (a_user_data "row" % string_of_int) row
            |> map_cons_option (a_user_data "col" % string_of_int) col)
      ([ div ~a:[a_class [CSS.row_handle]] []
       ; div ~a:[a_class [CSS.col_handle]] []
       ; div ~a:[a_class [CSS.mul_handle]] []
       ] @ content)

  let create_grid ?(classes = []) ?attrs
      ?(row_size = Fr 1.) ?(col_size = Fr 1.)
      ?rows ?cols ?(content = []) () : 'a elt =
    let classes = CSS.grid :: classes in
    let style = match rows, cols with
      | None, None -> None
      | Some rows, None ->
        Some (Printf.sprintf "grid-template-rows: %s"
                (gen_template ~size:row_size rows))
      | None, Some cols ->
        Some (Printf.sprintf "grid-template-columns: %s"
                (gen_template ~size:col_size cols))
      | Some rows, Some cols ->
        Some (Printf.sprintf "grid-template-rows: %s; grid-template-columns: %s"
                (gen_template ~size:row_size rows)
                (gen_template ~size:col_size cols)) in
    div ~a:([a_class classes] <@> attrs
            |> map_cons_option a_style style) content

  let create ?(classes = []) ?attrs ~grid () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] <@> attrs)
      ([ Card.create_actions [Card.create_action_icons [] ()] ()
       ; Card.create_media [grid] ()
       ])

end
