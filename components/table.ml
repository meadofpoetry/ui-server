open Containers
open Tyxml_js

module Markup = Components_markup.Table.Make(Xml)(Svg)(Html)

type sort = Asc | Dsc

let sort_to_string = function
  | Asc -> "ascending"
  | Dsc -> "descending"
let sort_of_string = function
  | "ascending"  -> Some Asc
  | "descending" -> Some Dsc
  | _            -> None

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

  let wrap_checkbox = function
    | None    -> None
    | Some cb -> Markup.Cell.create (Widget.to_markup cb) () |> Option.return

  class t ~(value:'a) i (fmt:'a fmt) (conv:'a -> string) (compare:string -> string -> int) () =
    let s = conv value in
    let content = Tyxml_js.Html.(pcdata s) in
    let elt = Markup.Cell.create ~is_numeric:(is_numeric fmt) content ()
              |> Tyxml_js.To_dom.of_element in
    object(self : 'self)
      method index : int  = i
      method value = s
      method compare (other:'self) = compare other#value self#value
      inherit Widget.t elt ()
    end
end

module Column = struct

  let wrap_checkbox = function
    | None    -> None
    | Some cb -> Markup.Column.create (Widget.to_markup cb) () |> Option.return

  class t i push (column:column) (fmt:'a fmt) () =
    let content = Tyxml_js.Html.(pcdata column.title) in
    let elt = Markup.Column.create ~sortable:column.sortable
                ~is_numeric:(is_numeric fmt) content ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.t elt ()
      method index : int = i
      initializer
        if column.sortable
        then Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
                 let order = self#get_attribute "aria-sort" in
                 (match Option.flat_map sort_of_string order with
                  | Some Dsc -> push (Some (self#index,Asc))
                  | _        -> push (Some (self#index,Dsc)));
                 true) |> ignore
    end
end

module Row = struct

  class t ?selection (s_selected:t list React.signal) s_selected_push (cells:Cell.t list) () =
    let cb  = match selection with
      | Some `Multiple -> Some (new Checkbox.t ())
      | _ -> None
    in
    let elt = Markup.Row.create ~cells:(List.map Widget.to_markup cells
                                              |> List.cons_maybe (Cell.wrap_checkbox cb)) ()
              |> Tyxml_js.To_dom.of_element in
    let arr = Array.of_list cells in
    object(self)
      inherit Widget.t elt ()
      method cells      = cells
      method cells_arr  = arr
      method checkbox   = cb
      method s_selected = s_selected

      method set_selected' x =
        Option.iter (fun cb -> cb#set_checked x) self#checkbox;
        self#add_or_remove_class x Markup.Row.selected_class

      method set_selected x  =
        let v = (self :> t) in
        (match selection with
         | Some `Single ->
            if x then (match React.S.value s_selected with
                       | [] -> s_selected_push @@ List.pure v
                       | l  -> List.iter (fun (t:t) -> t#set_selected' x) l;
                               s_selected_push @@ List.pure v)
            else (match React.S.value s_selected with
                  | [] -> ()
                  | l  -> List.remove ~eq:Equal.physical ~x:v l |> s_selected_push)
         | Some `Multiple ->
            let l = if x
                    then List.add_nodup ~eq:Equal.physical (self :> t) (React.S.value s_selected)
                    else List.remove ~eq:Equal.physical ~x:v (React.S.value s_selected)
            in s_selected_push l
         | None -> ());
        self#set_selected' x

      method selected = self#has_class Markup.Row.selected_class

      initializer
        match selection with
        | Some _ -> Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
                        self#set_selected (not self#selected); true)
                    |> ignore
        | _      -> ()
    end
end

module Body = struct
  class t ?selection (s_selected:Row.t list React.signal) s_selected_push () =
    let elt = Markup.Body.create ~rows:[] () |> Tyxml_js.To_dom.of_element in
    let s_rows,s_rows_push = React.S.create List.empty in
    object(self)
      inherit Widget.t elt ()
      method add_row (cells:Cell.t list) =
        let row = new Row.t ?selection s_selected s_selected_push cells () in
        s_rows_push @@ (List.cons row self#rows);
        Dom.appendChild self#root row#root
      method s_rows     = s_rows
      method rows       = List.rev @@ React.S.value s_rows
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

  class row ?selection ~(columns:Column.t list) () =
    let cb = match selection with Some `Multiple -> Some (new Checkbox.t ()) | _ -> None in
    let elt = Markup.Row.create ~cells:(List.map Widget.to_markup columns
                                              |> List.cons_maybe (Column.wrap_checkbox cb)) ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.t elt ()
      method checkbox = cb
    end

  class t ?selection s_selected s_selected_push s_rows fmt () =
    let s_sorted,s_sorted_push = React.S.create None in
    let columns = make_columns s_sorted_push fmt in
    let row = new row ?selection ~columns () in
    let elt = Markup.Header.create ~row:(Widget.to_markup row) ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      val _columns = columns
      inherit Widget.t elt ()
      inherit Widget.stateful ()

      method checkbox = row#checkbox
      method columns  = _columns
      method s_sorted = s_sorted

      method select_all () =
        List.iter (fun (r:Row.t) -> r#set_selected true) self#_rows
      method unselect_all () =
        List.iter (fun (r:Row.t) -> r#set_selected false) self#_rows

      method private _rows = React.S.value s_rows

      initializer
        Option.iter (fun cb ->
            Dom_events.listen cb#root Dom_events.Typ.change (fun _ _ ->
                if cb#checked
                then (List.iter (fun r -> r#set_selected' true) self#_rows;
                      s_selected_push self#_rows)
                else (List.iter (fun r -> r#set_selected' false) self#_rows;
                      s_selected_push List.empty);
                false)
            |> ignore) self#checkbox;
        React.S.map (fun (x:Row.t list) ->
            match List.length x with
            | 0 ->
               Option.iter (fun cb -> cb#set_checked false;
                                      cb#set_indeterminate false) self#checkbox
            | l when l = List.length self#_rows ->
               Option.iter (fun cb -> cb#set_checked true;
                                      cb#set_indeterminate false) self#checkbox
            | _ ->
               Option.iter (fun cb -> cb#set_indeterminate true;
                                      cb#set_checked false) self#checkbox) s_selected
        |> self#_keep_s;
        React.S.map (function
            | Some (index,sort) ->
               List.iter (fun (x:Column.t) ->
                   if index = x#index
                   then x#set_attribute "aria-sort" (sort_to_string sort)
                   else x#remove_attribute "aria-sort") _columns
            | None -> List.iter (fun x -> x#remove_attribute "aria-sort") _columns) s_sorted
        |> self#_keep_s
    end
end

module Table = struct
  class t ~header ~body () =
    let elt = Markup.create_table ~header:(Widget.to_markup header)
                ~body:(Widget.to_markup body) ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.t elt ()
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

class ['a] t ?selection ~(fmt:('a,_) row) () =
  let s_selected,s_selected_push = React.S.create List.empty in
  let body       = new Body.t ?selection s_selected s_selected_push () in
  let header     = new Header.t ?selection s_selected s_selected_push body#s_rows fmt () in
  let table      = new Table.t ~header ~body () in
  let elt        = Markup.create ?selection ~table:(Widget.to_markup table) ()
                   |> Tyxml_js.To_dom.of_element in
  object(self)
    inherit Widget.t elt ()
    inherit Widget.stateful ()

    method body       = body
    method rows       = body#rows
    method s_rows     = body#s_rows
    method s_selected = s_selected

    method add_row = make_cells (fun cells -> body#add_row cells) fmt

    method sort index sort =
      let rows = List.sort (fun row_1 row_2 ->
                     let cell_1 = row_1#cells_arr.(index) in
                     let cell_2 = row_2#cells_arr.(index) in
                     match sort, cell_1#compare cell_2 with
                     | Asc,x -> x
                     | Dsc,x -> ~-x) self#rows
      in
      body#set_empty ();
      List.iter (fun x -> Dom.appendChild body#root x#root) rows

    initializer
      React.S.map (function
          | Some (index,sort) -> self#sort index sort
          | None -> ()) header#s_sorted
      |> self#_keep_s

  end
