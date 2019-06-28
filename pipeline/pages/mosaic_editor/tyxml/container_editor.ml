
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

  let grid_bordered = BEM.add_modifier grid "bordered"

  let aspect_ratio_sizer = BEM.add_element root "aspect-ratio-sizer"

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

  module Card' = Card.Make(Xml)(Svg)(Html)

  let create_grid_cell ?(classes = []) ?attrs
      ?row_start
      ?col_start
      ?row_end
      ?col_end
      ?(content = []) () : 'a elt =
    let classes = CSS.cell :: classes in
    let get_style = function None -> "auto" | Some x -> Printf.sprintf "%d" x in
    let style = Printf.sprintf "grid-area: %s / %s / %s / %s;"
        (get_style row_start)
        (get_style col_start)
        (get_style row_end)
        (get_style col_end) in
    div ~a:([ a_class classes
            ; a_style style ] <@> attrs
            |> map_cons_option (a_user_data "row" % string_of_int) row_start
            |> map_cons_option (a_user_data "col" % string_of_int) col_start)
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

  let create ?(classes = []) ?attrs
      ~width ~height
      ~grid () : 'a elt =
    let classes = CSS.root :: Card.CSS.root :: Card.CSS.outlined :: classes in
    div ~a:([a_class classes] <@> attrs)
      ([ Card'.create_media
           [ svg ~a:[ Svg.a_class [CSS.aspect_ratio_sizer]
                    ; Svg.a_viewBox ( 0.
                                    , 0.
                                    , (float_of_int width)
                                    , (float_of_int height)
                                    )
                    ] []
           ; grid
           ] ()
       ; Card'.create_actions [] ()
       ])

end
