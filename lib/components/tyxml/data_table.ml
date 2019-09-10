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

  type 'a column =
    { sortable : bool
    ; title : string
    ; format : 'a t }

  let make_column ?(sortable = false) ~title format = {sortable; title; format}

  type _ format =
    | [] : unit format
    | ( :: ) : 'a column * 'b format -> ('a * 'b) format

  type _ data =
    | [] : unit data
    | ( :: ) : 'a * 'b data -> ('a * 'b) data
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Fmt = Make_fmt (Xml)

  let create_cell_content fmt v : 'a elt =
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

  let create_cell
      ?(classes = [])
      ?(attrs = [])
      ?colspan
      ?(numeric = false)
      ?(children = [])
      () : 'a elt =
    let classes =
      classes |> Utils.cons_if numeric CSS.cell_numeric |> List.cons CSS.cell
    in
    td ~a:([a_class classes] @ attrs |> Utils.map_cons_option a_colspan colspan) children

  let create_header_cell
      ?(classes = [])
      ?(attrs = [])
      ?(numeric = false)
      ?(sortable = false)
      ?(children = [])
      () : 'a elt =
    let classes =
      classes
      |> Utils.cons_if sortable CSS.header_cell_sortable
      |> Utils.cons_if numeric CSS.header_cell_numeric
      |> List.cons CSS.header_cell
    in
    th ~a:([a_class classes; a_role ["columnheader"]] @ attrs) children

  let create_row ?(classes = []) ?(attrs = []) ?(children = []) () : 'a elt =
    let classes = CSS.row :: classes in
    tr ~a:([a_class classes] @ attrs) children

  let create_header_row ?(classes = []) ?(attrs = []) ?(children = []) () : 'a elt =
    let classes = CSS.header_row :: classes in
    tr ~a:([a_class classes] @ attrs) children

  let create_header ?(classes = []) ?(attrs = []) ?cells ?children () : 'a elt =
    let children =
      match children with
      | Some x -> x
      | None -> (
        match cells with
        | None -> []
        | Some cells -> [create_header_row ~children:cells ()])
    in
    thead ~a:([a_class classes] @ attrs) children

  let create_body ?(classes = []) ?(attrs = []) ?(children = []) () : 'a elt =
    let classes = CSS.content :: classes in
    tbody ~a:([a_class classes] @ attrs) children

  let create_table ?(classes = []) ?(attrs = []) ?header ?(children = []) () : 'a elt =
    let classes = CSS.table :: classes in
    tablex ?thead:header ~a:([a_class classes] @ attrs) children

  let create ?(classes = []) ?(attrs = []) ?(dense = false) ?(children = []) () =
    let classes = classes |> Utils.cons_if dense CSS.dense |> List.cons CSS.root in
    div ~a:([a_class classes] @ attrs) children

  (** GADT table *)

  let create_cell_of_fmt ?classes ?attrs ?colspan ~fmt ~value () =
    let content = create_cell_content fmt value in
    create_cell
      ?classes
      ?attrs
      ?colspan
      ~numeric:(Fmt.is_numeric fmt)
      ~children:[content]
      ()

  let create_header_cell_of_fmt ?classes ?attrs ~(column : _ Fmt.column) () =
    create_header_cell
      ?classes
      ?attrs
      ~sortable:column.sortable
      ~numeric:(Fmt.is_numeric column.format)
      ~children:[txt column.title]
      ()

  let rec create_header_cells_of_fmt : type a. a Fmt.format -> 'b Html.elt list =
   fun format ->
    match format with
    | [] -> []
    | column :: tl ->
        let cell = create_header_cell_of_fmt ~column () in
        cell :: create_header_cells_of_fmt tl

  let create_header_of_fmt ?classes ?attrs ~format () =
    create_header ?classes ?attrs ~cells:(create_header_cells_of_fmt format) ()

  let rec create_cells_of_fmt : type a. a Fmt.format -> a Fmt.data -> 'b Html.elt list =
   fun format data ->
    match format, data with
    | [], [] -> []
    | col :: l1, v :: l2 ->
        let cell = create_cell_of_fmt ~fmt:col.format ~value:v () in
        cell :: create_cells_of_fmt l1 l2

  let create_row_of_fmt ?classes ?attrs ~format ~data () =
    let cells = create_cells_of_fmt format data in
    create_row ?classes ?attrs ~children:cells ()

  (* TODO how to provide custom classes, attrs to inner components? *)
  let create_of_fmt ?classes ?attrs ~format ~data () =
    let rows = List.map (fun x -> create_row_of_fmt ~format ~data:x ()) data in
    let header = create_header_of_fmt ~format () in
    let body = create_body ~children:rows () in
    let table = create_table ~header ~children:[body] () in
    create ?classes ?attrs ~children:[table] ()

  (** Example using GADT format:

      {[ let table =
           let (fmt : _ Fmt.format) =
             Fmt.
               [ make_column ~title:"Title 1" Int
               ; make_column ~title:"Title 2" Int
               ; make_column ~title:"Title 3" Int ]
           in
           let (data : _ Fmt.data list) =
             [[3; 3; 3]; [4; 5; 4]; [1; 2; 3]; [1; 1; 1]; [4; 3; 1]; [1; 6; 4]]
           in
           create_of_fmt ~format:fmt ~data ()
      ]}
  *)
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
module Fmt = Make_fmt (Tyxml.Xml)
