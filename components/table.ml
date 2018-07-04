open Containers

type sort = Asc | Dsc

let sort_to_string = function
  | Asc -> "ascending"
  | Dsc -> "descending"

let cmp_invert i = if i = 1 then -1 else if i = -1 then 1 else i

type 'a custom =
  { to_string  : 'a -> string
  ; compare    : string -> string -> int
  ; is_numeric : bool
  }

type _ fmt =
  | String : string fmt
  | Int    : int fmt
  | Int32  : int32 fmt
  | Int64  : int64 fmt
  | Option : ('a fmt * string) -> 'a option fmt
  | Custom : 'a custom -> 'a fmt

type column =
  { sortable : bool
  ; title    : string
  }

let to_column ?(sortable=false) title =
  { sortable; title }

type (_,_) row =
  | (::) : (column * 'a fmt) * ('b,'c) row -> ('a -> 'b,'c) row
  | []   : ('a,'a) row

let rec to_string : type a. a fmt -> a -> string = fun fmt v ->
  match fmt with
  | Int            -> string_of_int v
  | Int32          -> Int32.to_string v
  | Int64          -> Int64.to_string v
  | String         -> v
  | Option (fmt,e) -> (match v with None -> e | Some v -> (to_string fmt) v)
  | Custom x       -> x.to_string v

let rec is_numeric : type a. a fmt -> bool = function
  | Int            -> true
  | Int32          -> true
  | Int64          -> true
  | String         -> false
  | Option (fmt,_) -> is_numeric fmt
  | Custom x       -> x.is_numeric

let rec compare : type a. a fmt -> (string -> string -> int) = function
  | Int            -> fun i1 i2 -> Int.compare (int_of_string i1) (int_of_string i2)
  | Int32          -> fun i1 i2 -> Int32.compare (Int32.of_string_exn i1) (Int32.of_string_exn i2)
  | Int64          -> fun i1 i2 -> Int64.compare (Int64.of_string_exn i1) (Int64.of_string_exn i2)
  | String         -> String.compare
  | Option (fmt,e) -> fun x1 x2 -> let o1 = if String.equal e x1 then None else Some x1 in
                                   let o2 = if String.equal e x2 then None else Some x2 in
                                   Option.compare (compare fmt) o1 o2
  | Custom x       -> x.compare

module Cell = struct
  class t ~(value:'a) i (fmt:'a fmt) (conv:'a -> string) (compare:string -> string -> int) () =
    let s   = conv value in
    let content = Tyxml_js.Html.(pcdata s) in
    let elt = Markup.Table.Cell.create ~is_numeric:(is_numeric fmt) content ()
              |> Tyxml_js.To_dom.of_element in
    object(self : 'self)
      method index : int  = i
      method value = s
      method compare (other:'self) = compare other#value self#value
      inherit Widget.widget elt ()
    end
end

module Column = struct

  class t i push (column:column) (fmt:'a fmt) () =
    let elt = Markup.Table.Column.create ~sortable:column.sortable
                ~is_numeric:(is_numeric fmt) column.title ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.widget elt ()
      method index : int = i
      initializer
        if column.sortable
        then Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
                 let order = self#get_attribute "aria-sort" in
                 (match order with
                  | Some "descending" -> push (Some (self#index,Asc))
                  | _                 -> push (Some (self#index,Dsc)));
                 true) |> ignore
    end
end

module Row = struct
  class t ~(cells:Cell.t list) () =
    let elt = Markup.Table.Row.create ~cells:(List.map Widget.widget_to_markup cells) ()
              |> Tyxml_js.To_dom.of_element in
    let arr = Array.of_list cells in
    object
      method cells     = cells
      method cells_arr = arr
      inherit Widget.widget elt ()
    end
end

module Header = struct

  let rec make_columns : type a b. ((int * sort) option -> unit) -> (a,b) row -> Column.t list =
    fun push fmt ->
    let rec loop : type a b. int -> ((int * sort) option -> unit) -> (a,b) row -> Column.t list =
      fun i push fmt ->
      match fmt with
      | [] -> []
      | (::) ((c,fmt),rest) -> (new Column.t i push c fmt ()) :: (loop (succ i) push rest)
    in loop 0 push fmt

  class row ~(columns:Column.t list) () =
    let elt = Markup.Table.Row.create ~cells:(List.map Widget.widget_to_markup columns) ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.widget elt ()
    end

  class t fmt () =
    let s_sorted,s_sorted_push = React.S.create None in
    let columns = make_columns s_sorted_push fmt in
    let row = new row ~columns () in
    let elt = Markup.Table.Header.create ~row:(Widget.widget_to_markup row) ()
              |> Tyxml_js.To_dom.of_element in
    object
      val mutable _s = None
      val _columns = columns
      inherit Widget.widget elt ()

      method columns  = _columns
      method s_sorted = s_sorted

      initializer
        React.S.map (function
            | Some (index,sort) ->
               List.iter (fun x -> if index = x#index
                                   then x#set_attribute "aria-sort" (sort_to_string sort)
                                   else x#remove_attribute "aria-sort") _columns
            | None -> List.iter (fun x -> x#remove_attribute "aria-sort") _columns) s_sorted
        |> fun s -> _s <- Some s
    end
end

module Body = struct
  class t ~(rows:Row.t list) () =
    let elt = Markup.Table.Body.create ~rows:(List.map Widget.widget_to_markup rows) ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.widget elt ()
      val mutable _rows = List.rev rows

      method add_row (row:Row.t) = _rows <- row :: _rows; Dom.appendChild self#root row#root
      method rows = List.rev _rows
    end
end

module Table = struct
  class t ~header ~body () =
    let elt = Markup.Table.create_table ~header:(Widget.widget_to_markup header)
                ~body:(Widget.widget_to_markup body) ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.widget elt ()
    end
end

let rec make_cells : type ty v. (Cell.t list -> v) -> (ty,v) row -> ty = fun k ->
  let rec loop : type a b. int -> (Cell.t list -> b) -> (a,b) row -> a = fun i k ->
    function
    | [] -> k []
    | (::) ((_,fmt),rest) ->
       let f x = loop (succ i) (fun acc ->
                     let cell = new Cell.t ~value:x i fmt (to_string fmt) (compare fmt) () in
                     k @@ cell :: acc) rest in f
  in loop 0 k

class ['a] t ~(fmt:('a,_) row) () =
  let body   = new Body.t ~rows:[] () in
  let header = new Header.t fmt () in
  let table  = new Table.t ~header ~body () in
  let elt    = Markup.Table.create ~table:(Widget.widget_to_markup table) ()
               |> Tyxml_js.To_dom.of_element in
  object(self)
    inherit Widget.widget elt ()

    method body = body
    method rows = body#rows

    method add_row =
      make_cells (fun cells -> let row = new Row.t ~cells () in body#add_row row) fmt

    method private _sort index sort =
      let rows = List.sort (fun row_1 row_2 ->
                     let cell_1 = row_1#cells_arr.(index) in
                     let cell_2 = row_2#cells_arr.(index) in
                     match sort, cell_1#compare cell_2 with
                     | Asc,x -> x
                     | Dsc,x -> cmp_invert x) self#rows
      in
      body#set_empty ();
      List.iter (fun x -> Dom.appendChild body#root x#root) rows

    initializer
      React.S.map (function
          | Some (index,sort) -> self#_sort index sort
          | None -> ()) header#s_sorted
      |> ignore

  end
