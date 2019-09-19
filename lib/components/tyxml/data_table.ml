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

module Make_fmt (Xml : Xml_sigs.T) = struct
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
    ; title : string Xml.wrap
    ; format : 'a t }

  let make_column ?(sortable = false) ~title format = {sortable; title; format}

  type _ format =
    | [] : unit format
    | ( :: ) : 'a column * 'b format -> ('a * 'b) format

  type _ data_format =
    | [] : unit data_format
    | ( :: ) : 'a t * 'b data_format -> ('a * 'b) data_format

  type _ data =
    | [] : unit data
    | ( :: ) : 'a * 'b data -> ('a * 'b) data

  type _ data_lazy =
    | [] : unit data_lazy
    | ( :: ) : (unit -> 'a) * 'b data_lazy -> ('a * 'b) data_lazy

  type _ data_opt =
    | [] : unit data_opt
    | ( :: ) : 'a option * 'b data_opt -> ('a * 'b) data_opt
end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS
  module Fmt = Make_fmt (Xml)

  let data_table_cell_content fmt v : 'a elt =
    let rec aux : type a. a Fmt.t -> a -> Xml.elt =
     fun fmt v ->
      match fmt with
      | Option (fmt, e) -> (
        match v with
        | None -> aux String e
        | Some x -> aux fmt x)
      | Html _ -> v
      | Custom_elt x -> x.to_elt v
      | _ -> toelt @@ txt (return (Fmt.to_string fmt v))
    in
    tot @@ aux fmt v

  let data_table_cell
      ?(classes = [])
      ?(a = [])
      ?colspan
      ?(numeric = false)
      ?(children = nil ())
      () : 'a elt =
    let classes =
      classes |> Utils.cons_if numeric CSS.cell_numeric |> List.cons CSS.cell
    in
    td
      ~a:(a_class (return classes) :: a |> Utils.map_cons_option a_colspan colspan)
      children

  let data_table_header_cell
      ?(classes = [])
      ?(a = [])
      ?(numeric = false)
      ?(sortable = false)
      ?(children = nil ())
      () : 'a elt =
    let classes =
      classes
      |> Utils.cons_if sortable CSS.header_cell_sortable
      |> Utils.cons_if numeric CSS.header_cell_numeric
      |> List.cons CSS.header_cell
    in
    th ~a:(a_class (return classes) :: a_role (return ["columnheader"]) :: a) children

  let data_table_row ?(classes = []) ?(a = []) ?(children = nil ()) () =
    let classes = CSS.row :: classes in
    tr ~a:(a_class (return classes) :: a) children

  let data_table_header_row ?(classes = []) ?(a = []) ?(children = nil ()) () =
    let classes = CSS.header_row :: classes in
    tr ~a:(a_class (return classes) :: a) children

  let data_table_header ?(classes = []) ?(a = []) ?cells ?children () =
    let children =
      match children with
      | Some x -> x
      | None -> (
        match cells with
        | None -> nil ()
        | Some cells -> singleton (return (data_table_header_row ~children:cells ())))
    in
    thead ~a:(a_class (return classes) :: a) children

  let data_table_body ?(classes = []) ?(a = []) ?(children = nil ()) () : 'a elt =
    let classes = CSS.content :: classes in
    tbody ~a:(a_class (return classes) :: a) children

  let data_table_table ?(classes = []) ?(a = []) ?header ?(children = nil ()) () =
    let classes = CSS.table :: classes in
    tablex ?thead:header ~a:(a_class (return classes) :: a) children

  let data_table ?(classes = []) ?(a = []) ?(dense = false) ?(children = nil ()) () =
    let classes = classes |> Utils.cons_if dense CSS.dense |> List.cons CSS.root in
    div ~a:(a_class (return classes) :: a) children

  (** GADT table *)

  let data_table_cell_of_fmt ?classes ?a ?colspan ~fmt ~value () =
    let children = singleton (return (data_table_cell_content fmt value)) in
    data_table_cell ?classes ?a ?colspan ~numeric:(Fmt.is_numeric fmt) ~children ()

  let data_table_header_cell_of_fmt ?classes ?a ~(column : _ Fmt.column) () =
    let children = singleton (return (txt column.title)) in
    data_table_header_cell
      ?classes
      ?a
      ~sortable:column.sortable
      ~numeric:(Fmt.is_numeric column.format)
      ~children
      ()

  let rec data_table_header_cells_of_fmt : type a. a Fmt.format -> _ list_wrap =
   fun format ->
    match format with
    | [] -> nil ()
    | column :: tl ->
        let cell = return @@ data_table_header_cell_of_fmt ~column () in
        cons cell (data_table_header_cells_of_fmt tl)

  let data_table_header_of_fmt ?classes ?a ~format () =
    data_table_header ?classes ?a ~cells:(data_table_header_cells_of_fmt format) ()

  let rec data_table_cells_of_fmt : type a. a Fmt.format -> a Fmt.data -> _ list_wrap =
   fun format data ->
    match format, data with
    | [], [] -> nil ()
    | col :: l1, v :: l2 ->
        let cell = return (data_table_cell_of_fmt ~fmt:col.format ~value:v ()) in
        cons cell (data_table_cells_of_fmt l1 l2)

  let data_table_row_of_fmt ?classes ?a ~format ~data () =
    let cells = data_table_cells_of_fmt format data in
    data_table_row ?classes ?a ~children:cells ()

  (* TODO how to provide custom classes, a to inner components? *)
  let data_table_of_fmt ?classes ?a ?dense ~format ?(rows = nil ()) () =
    let header = return @@ data_table_header_of_fmt ~format () in
    let body = return @@ data_table_body ~children:rows () in
    let table = return @@ data_table_table ~header ~children:(singleton body) () in
    data_table ?classes ?a ?dense ~children:(singleton table) ()

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
           data_table_of_fmt ~format:fmt ~data ()
      ]}
  *)
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
module Fmt_f = Make_fmt (Tyxml.Xml)
