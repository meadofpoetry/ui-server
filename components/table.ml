open Containers
open Markup

let base_class      = "mdc-data-table"
let container_class = CSS.add_element base_class "container"

(* TEST *)

type _ fmt =
  | String : string fmt
  | Int    : int fmt
  | Int32  : int32 fmt
  | Int64  : int64 fmt

(* END TEST *)

type (_,_) row =
  | (::) : (string * 'a fmt) * ('b,'c) row -> ('a -> 'b,'c) row
  | E    : ('a,'a) row

let to_string : type a. a fmt -> a -> string = fun fmt v ->
  match fmt with
  | Int    -> string_of_int v
  | Int32  -> Int32.to_string v
  | Int64  -> Int64.to_string v
  | String -> v

let empty = E

module Cell = struct

  open Tyxml_js.Html

  let _class         = CSS.add_element  base_class "cell"
  let numeric_class  = CSS.add_modifier _class "numeric"

  let markup ?(classes=[]) ?(is_numeric=false) ?(header=false) content () =
    let tag = if header then th else td in
    tag ~a:([ a_class (classes
                       |> (fun l -> cons_if is_numeric numeric_class l))])
      [ pcdata content ]

  class t ~(value:'a) (fmt:'a fmt) (conv:'a -> string) () =
    let is_numeric : type a. a fmt -> bool = function
      | Int -> true
      | Int32 -> true
      | Int64 -> true
      | String -> false
    in
    let s   = conv value in
    let elt = markup ~is_numeric:(is_numeric fmt) s () |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.widget elt ()
      initializer
        self#add_class _class;
    end

end

module Column = struct

  let _class = CSS.add_element base_class "column"

  class t ~(value:string) () =
    let elt = Cell.markup ~header:true value () |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.widget elt ()
      initializer
        self#add_class _class
    end

end

module Row = struct

  let _class = CSS.add_element base_class "row"

  class t ~cells () =
    let elt = Table.Row.create ~cells:(List.map Widget.widget_to_markup cells) ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.widget elt ()
      initializer
        self#add_class _class
    end

end

module Header = struct

  let _class = CSS.add_element base_class "header"

  class t ~row () =
    let elt = Table.Header.create ~row:(Widget.widget_to_markup row) ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.widget elt ()
      initializer
        self#add_class _class
    end

end

let identity : 'a. 'a -> 'a = fun x -> x

let rec get_column_names : type a b. (a,b) row -> string list = function
  | E                 -> [ ]
  | (::) ((s,_),rest) -> s :: get_column_names rest

let rec make_cells : type ty v. (Cell.t list -> v) -> (ty,v) row -> ty =
  fun k ->
  function
  | E -> k []
  | (::) ((_,fmt),rest) ->
     let f x = make_cells (fun acc -> let cell = new Cell.t ~value:x fmt (to_string fmt) () in
                                      k @@ cell :: acc) rest in f

class ['a] t ~(fmt:('a,_) row) () =
  let elt = Dom_html.createDiv Dom_html.document in
  object(self)
    inherit Widget.widget elt ()
    method add_row =
      make_cells (fun cells ->
          let row = new Row.t ~cells () in
          Dom.appendChild self#root row#root) fmt

    (* Private methods *)
    method private _add_headers () =
      let cells = List.map (fun n -> new Column.t ~value:n ()) (get_column_names fmt) in
      let row   = new Row.t ~cells () in
      let hdr   = new Header.t ~row () in
      Dom.appendChild self#root hdr#root

    initializer
      self#_add_headers ()
  end
