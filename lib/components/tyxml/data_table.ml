module CSS = struct
  (** Mandatory. The root DOM element containing [table]
      and other supporting elements. *)
  let root = "mdc-data-table"

  (** Mandatory. Table element.
      Added to [table] HTML tag. *)
  let table = BEM.add_element root "table"

  (** Mandatory. Table header row element.
      Added to [thead > tr] HTML tag. *)
  let header_row = BEM.add_element root "header-row"

  (** Mandatory. Table header cell element.
      Added to [thead > th > td] HTML tag. *)
  let header_cell = BEM.add_element root "header-cell"

  (** Optional. Table header cell element that contains mdc-checkbox.
      Added to [thead > th > td:first-child] HTML tag. *)
  let header_cell_checkbox = BEM.add_modifier header_cell "checkbox"

  (** Optional. Table header cell element that maps to numeric cells.
      Added to [thead > th > td] HTML tag. *)
  let header_cell_numeric = BEM.add_modifier header_cell "numeric"

  (** Mandatory. Table body element.
      Added to [tbody] HTML tag. *)
  let content = BEM.add_element root "content"

  (** Mandatory. Table row element.
      Added to [tbody > tr] HTML tag. *)
  let row = BEM.add_element root "row"

  (** Mandatory. Table cell element.
      Added to [tbody > tr > td] HTML tag. *)
  let cell = BEM.add_element root "cell"

  (** Optional. Table cell element that contains numeric data.
      Added to [tbody > tr > td] HTML tag. *)
  let cell_numeric = BEM.add_modifier cell "numeric"

  (** Optional. Table cell element that contains mdc-checkbox.
      Added to [thead > th > td:first-child] HTML tag. *)
  let cell_checkbox = BEM.add_modifier cell "checkbox"

  (** Optional. Checkbox element rendered inside table header row element.
      Add this class name to [mdc-checkbox] element to override styles required
      for data-table. *)
  let header_row_checkbox = BEM.add_element root "header-row-checkbox"

  (** Optional. Checkbox element rendered inside table row element.
      Add this class name to [mdc-checkbox] element to override styles
      required for data-table. *)
  let row_checkbox = BEM.add_element root "row-checkbox"

  (** Optional. Modifier class added to [mdc-data-table__row]
      when table row is selected. *)
  let row_selected = BEM.add_modifier row "selected"

  let dense = BEM.add_modifier root "dense"

  let header_cell_sortable = BEM.add_modifier header_cell "sortable"
end

let default_float = Printf.sprintf "%f"

type sort =
  | Asc
  | Dsc

let sort_to_string = function
  | Asc -> "ascending"
  | Dsc -> "descending"

let sort_of_string = function
  | "ascending" -> Some Asc
  | "descending" -> Some Dsc
  | _ -> None

module Make_fmt (Xml : Xml_sigs.NoWrap) = struct
  type 'a custom_elt =
    { to_elt : 'a -> Xml.elt
    ; of_elt : Xml.elt -> 'a
    ; compare : 'a -> 'a -> int
    ; is_numeric : bool }

  type 'a custom =
    { to_string : 'a -> string
    ; of_string : string -> 'a
    ; compare : 'a -> 'a -> int
    ; is_numeric : bool }

  type _ t =
    | String : string t
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Float : float t
    | Option : ('a t * string) -> 'a option t
    | Html : (bool * (Xml.elt -> Xml.elt -> int)) -> Xml.elt t
    | Custom : 'a custom -> 'a t
    | Custom_elt : 'a custom_elt -> 'a t

  let rec to_string : type a. a t -> a -> string = function
    | Int -> string_of_int
    | Int32 -> Int32.to_string
    | Int64 -> Int64.to_string
    | Float -> default_float
    | String -> Fun.id
    | Option (t, e) -> (
        function
        | None -> e
        | Some v -> (to_string t) v)
    | Custom x -> x.to_string
    (* Not allowed *)
    | Html _ | Custom_elt _ -> assert false

  let rec of_string : type a. a t -> string -> a =
   fun fmt s ->
    match fmt with
    | Int -> int_of_string s
    | Int32 -> Int32.of_string s
    | Int64 -> Int64.of_string s
    | Float -> Float.of_string s
    | String -> s
    | Option (fmt, default) ->
        if String.equal default s then None else Some (of_string fmt s)
    | Custom x -> x.of_string s
    | Html _ | Custom_elt _ -> assert false

  let rec is_numeric : type a. a t -> bool = function
    | Int | Int32 | Int64 | Float -> true
    | String -> false
    | Option (t, _) -> is_numeric t
    | Custom x -> x.is_numeric
    | Custom_elt x -> x.is_numeric
    | Html (numeric, _) -> numeric

  let rec compare : type a. a t -> a -> a -> int = function
    | Int -> Stdlib.compare
    | Int32 -> Int32.compare
    | Int64 -> Int64.compare
    | Float -> Float.compare
    | String -> String.compare
    | Option (t, _) -> Option.compare (compare t)
    | Custom x -> x.compare
    | Custom_elt x -> x.compare
    | Html (_, cmp) -> cmp

  let equal : type a. a t -> a -> a -> bool = fun t a b -> 0 = compare t a b
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Fmt = Make_fmt (Xml)

  let make_cell_content fmt v : 'a elt =
    let rec aux : type a. a Fmt.t -> a -> Xml.elt =
     fun fmt v ->
      match fmt with
      | Option (fmt, e) -> (
        match v with
        | None -> aux String e
        | Some x -> aux fmt x)
      | Html _ -> v
      | Custom_elt x -> x.to_elt v
      | _ ->
          let elt = txt (Fmt.to_string fmt v) in
          toelt elt
    in
    tot @@ aux fmt v

  let make_cell ?(classes = []) ?(attrs = []) ?colspan ?(numeric = false) content :
      'a elt =
    let classes =
      classes |> Utils.cons_if numeric CSS.cell_numeric |> List.cons CSS.cell
    in
    td ~a:([a_class classes] @ attrs |> Utils.map_cons_option a_colspan colspan) content

  let cell_of_fmt ?classes ?attrs ?colspan fmt v =
    let content = make_cell_content fmt v in
    make_cell ?classes ?attrs ?colspan ~numeric:(Fmt.is_numeric fmt) [content]

  let make_header_cell
      ?(classes = [])
      ?(attrs = [])
      ?(numeric = false)
      ?(sortable = false)
      content : 'a elt =
    let classes =
      classes
      |> Utils.cons_if sortable CSS.header_cell_sortable
      |> Utils.cons_if numeric CSS.header_cell_numeric
      |> List.cons CSS.header_cell
    in
    th ~a:([a_class classes] @ attrs) [content]

  let make_row ?(classes = []) ?(attrs = []) cells : 'a elt =
    let classes = CSS.row :: classes in
    tr ~a:([a_class classes] @ attrs) cells

  let make_header ?(classes = []) ?(attrs = []) row : 'a elt =
    thead ~a:([a_class classes] @ attrs) [row]

  let make_body ?(classes = []) ?(attrs = []) rows : 'a elt =
    tbody ~a:([a_class classes] @ attrs) rows

  let make_table ?(classes = []) ?(attrs = []) ?header ~body () : 'a elt =
    let classes = CSS.table :: classes in
    table ?thead:header ~a:([a_class classes] @ attrs) [body]

  let make_content ?(classes = []) ?(attrs = []) ~table () : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] @ attrs) [table]

  let make ?(classes = []) ?(attrs = []) ~content () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] @ attrs) [content]
end
