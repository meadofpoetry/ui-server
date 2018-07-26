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
  ; of_string  : string -> 'a
  ; compare    : 'a -> 'a -> int
  ; is_numeric : bool
  }

type 'a conv =
  { to_string : 'a -> string
  ; of_string : string -> 'a
  }

let default_time : Ptime.t conv =
  { to_string = Format.asprintf "%a" Ptime.pp
  ; of_string = fun s ->
                Result.get_exn @@ Ptime.of_rfc3339 s
                |> fun (t, _, _) -> t }

let default_float : float conv =
  { to_string = Printf.sprintf "%f"
  ; of_string = float_of_string
  }

type _ fmt =
  | String : string fmt
  | Int    : int fmt
  | Int32  : int32 fmt
  | Int64  : int64 fmt
  | Float  : float conv option   -> float fmt
  | Time   : Ptime.t conv option -> Ptime.t fmt
  | Option : ('a fmt * string)   -> 'a option fmt
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
  | Float x        -> (Option.get_or ~default:default_float x).to_string v
  | String         -> v
  | Time x         -> (Option.get_or ~default:default_time x).to_string v
  | Option (fmt,e) -> (match v with None -> e | Some v -> (to_string fmt) v)
  | Custom x       -> x.to_string v

let rec is_numeric : type a. a fmt -> bool = function
  | Int            -> true
  | Int32          -> true
  | Int64          -> true
  | Float _        -> true
  | String         -> false
  | Time _         -> false
  | Option (fmt,_) -> is_numeric fmt
  | Custom x       -> x.is_numeric

let rec compare : type a. a fmt -> (string -> string -> int) = function
  | Int   ->
     fun i1 i2 ->
     Int.compare (int_of_string i1) (int_of_string i2)
  | Int32 ->
     fun i1 i2 ->
     Int32.(compare (of_string_exn i1) (of_string_exn i2))
  | Int64 ->
     fun i1 i2 ->
     Int64.(compare (of_string_exn i1) (of_string_exn i2))
  | Float t ->
     fun i1 i2 ->
     let t = Option.get_or ~default:default_float t in
     Float.compare (t.of_string i1) (t.of_string i2)
  | String -> String.compare
  | Time t ->
     fun x y ->
     let t = Option.get_or ~default:default_time t in
     let x = t.of_string x in
     let y = t.of_string y in
     Ptime.compare x y
  | Option (fmt,e) ->
     fun x1 x2 ->
     let o1 = if String.equal e x1 then None else Some x1 in
     let o2 = if String.equal e x2 then None else Some x2 in
     Option.compare (compare fmt) o1 o2
  | Custom x ->
     fun x1 x2 ->
     let x1 = x.of_string x1 in
     let x2 = x.of_string x2 in
     x.compare x1 x2

let rec of_string : type a. a fmt -> string -> a = fun fmt s ->
  match fmt with
  | Int     -> int_of_string s
  | Int32   -> Int32.of_string_exn s
  | Int64   -> Int64.of_string_exn s
  | Float x -> (Option.get_or ~default:default_float x).of_string s
  | String  -> s
  | Time x  -> (Option.get_or ~default:default_time x).of_string s
  | Option (fmt, empty) ->
     if String.equal s empty then None
     else Some (of_string fmt s)
  | Custom c -> c.of_string s

let rec compare' : type a. a fmt -> a -> a -> int = fun fmt x y ->
  match fmt with
  | Int            -> Int.compare x y
  | Int32          -> Int32.compare x y
  | Int64          -> Int64.compare x y
  | Float _        -> Float.compare x y
  | String         -> String.compare x y
  | Time _         -> Ptime.compare x y
  | Option (fmt,_) -> Option.compare (compare' fmt) x y
  | Custom c       -> c.compare x y

module Cell = struct

  let wrap_checkbox = function
    | None    -> None
    | Some cb -> Markup.Cell.create (Widget.to_markup cb) () |> Option.return

  class ['a] t ~(value:'a) (fmt:'a fmt) () =
    let s = to_string fmt value in
    let content = Tyxml_js.Html.(pcdata s) in
    let elt = Markup.Cell.create ~is_numeric:(is_numeric fmt) content ()
              |> Tyxml_js.To_dom.of_element in
    object(self : 'self)
      val mutable _value = value
      method fmt   : 'a fmt = fmt
      method value : 'a = _value
      method set_value (v:'a) =
        _value <- v;
        self#set_text_content (to_string fmt v)
      method compare (other:'self) = compare' fmt other#value self#value
      inherit Widget.t elt ()
    end

  type (_,_) cells =
    | (::) : 'a t * ('b,'c) cells -> ('a -> 'b, 'c) cells
    | []   : ('a,'a) cells

  (* type opt =
   *   | String of string option t
   *   | Int    of int option t
   *   | Int32  of int32 option t
   *   | Int64  of int64 option t
   *   | Float  of float option t
   *   | Time   of Ptime.t option t
   *   | Custom of Widget.t
   * 
   * type value =
   *   | String of string t
   *   | Int    of int t
   *   | Int32  of int32 t
   *   | Int64  of int64 t
   *   | Float  of float t
   *   | Time   of Ptime.t t
   *   | Option of opt
   *   | Custom of Widget.t
   * 
   * let rec cell_to_value : type a. a t -> a fmt -> value =
   *   fun cell ->
   *   function
   *   | String   -> String cell
   *   | Int      -> Int cell
   *   | Int32    -> Int32 cell
   *   | Int64    -> Int64 cell
   *   | Float _  -> Float cell
   *   | Time _   -> Time cell
   *   | _        -> Custom cell#widget *)

  let rec make_cells : type a b. ((a,b) cells -> b) -> (a,b) row -> a = fun k ->
    function
    | [] -> k []
    | (::) ((_, fmt), rest) ->
       let f x = make_cells (fun acc ->
                     let cell = new t ~value:x fmt () in
                     k @@ cell :: acc) rest in f

  let rec cells_to_widget_list : type a b. (a,b) cells -> Widget.t list =
    fun cells ->
    let rec aux : type a b. Widget.t list -> (a,b) cells -> Widget.t list =
      fun acc ->
      function
      | [] -> acc
      | cell :: rest -> aux (cell#widget :: acc) rest
    in List.rev @@ aux [] cells

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

  let rec update : type ty v. ((ty,v) Cell.cells -> v) -> (ty,v) Cell.cells -> ty =
    fun k ->
    let open Cell in
    function
    | [] -> k []
    | (::) (cell, rest) ->
       let f x = update (fun acc ->
                     cell#set_value x;
                     k @@ cell :: acc) rest
       in f

  class ['a,'b] t
          ?selection
          (s_selected:('a,'b) t list React.signal)
          s_selected_push
          (cells:('a,'b) Cell.cells) () =
    let cb  = match selection with
      | Some `Multiple -> Some (new Checkbox.t ())
      | _ -> None
    in
    let cells' = Cell.cells_to_widget_list cells in
    let elt = Markup.Row.create ~cells:(List.map Widget.to_markup cells'
                                        |> List.cons_maybe (Cell.wrap_checkbox cb)) ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.t elt ()
      method checkbox   = cb
      method s_selected = s_selected

      method update = update (fun _ -> ()) self#cells

      method selected = self#has_class Markup.Row.selected_class

      method set_selected' x =
        Option.iter (fun cb -> cb#set_checked x) self#checkbox;
        self#add_or_remove_class x Markup.Row.selected_class

      method set_selected x  =
        let v = (self :> ('a,'b) t) in
        (match selection with
         | Some `Single ->
            if x
            then (match React.S.value s_selected with
                  | [] -> s_selected_push @@ List.pure v
                  | l  -> List.iter (fun (t:('a,'b) t) -> t#set_selected' x) l;
                          s_selected_push @@ List.pure v)
            else (match React.S.value s_selected with
                  | [] -> ()
                  | l  -> List.remove ~eq:Equal.physical ~x:v l
                          |> s_selected_push)
         | Some `Multiple ->
            let l =
              if x
              then List.add_nodup ~eq:Equal.physical
                     (self :> ('a,'b) t)
                     (React.S.value s_selected)
              else List.remove ~eq:Equal.physical ~x:v
                     (React.S.value s_selected)
            in s_selected_push l
         | None -> ());
        self#set_selected' x

      method cells = cells

      method cells_widgets = cells'

      initializer
        match selection with
        | Some _ -> Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
                        self#set_selected (not self#selected); true)
                    |> ignore
        | _      -> ()
    end
end

module Body = struct
  class ['a,'b] t ?selection () =
    let elt = Markup.Body.create ~rows:[] ()
              |> Tyxml_js.To_dom.of_element in
    let s_rows,s_rows_push = React.S.create List.empty in
    object(self)
      inherit Widget.t elt ()
      method prepend_row (row:('a,'b) Row.t) =
        s_rows_push @@ (List.cons row self#rows);
        Dom.insertBefore self#root row#root self#root##.firstChild
      method append_row (row:('a,'b) Row.t) =
        s_rows_push @@ (List.cons row self#rows);
        Dom.appendChild self#root row#root
      method s_rows = s_rows
      method rows   = List.rev @@ React.S.value s_rows
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

  class ['a,'b] t ?selection s_selected s_selected_push s_rows fmt () =
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
        List.iter (fun (r:('a,'b) Row.t) -> r#set_selected true) self#_rows
      method unselect_all () =
        List.iter (fun (r:('a,'b) Row.t) -> r#set_selected false) self#_rows

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
        React.S.map (fun (x:('a,'b) Row.t list) ->
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

let rec compare : type a b. int ->
                       (a,b) Cell.cells ->
                       (a,b) Cell.cells -> int =
  fun index cells1 cells2 ->
  let open Cell in
  match cells1, cells2 with
  | [], [] -> 0
  | x1 :: rest1, x2 :: rest2 ->
     if index = 0
     then x1#compare x2
     else compare (pred index) rest1 rest2
  | [], _ -> assert false
  | _, [] -> assert false

class ['a] t ?selection ?(dense=false) ~(fmt:('a,_) row) () =
  let s_selected,s_selected_push = React.S.create List.empty in
  let body       = new Body.t ?selection () in
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

    method dense = self#has_class Markup.dense_class
    method set_dense x = self#add_or_remove_class x Markup.dense_class

    method add_row =
      Cell.make_cells (fun cells ->
          let row = new Row.t ?selection
                      s_selected
                      s_selected_push
                      cells
                      () in
          body#append_row row) fmt

    method sort index sort =
      let rows =
        List.sort (fun (row_1:('a,'b) Row.t)
                       (row_2:('a,'b) Row.t) ->
            match sort, compare index row_1#cells row_2#cells with
            | Asc,x -> x
            | Dsc,x -> ~-x) self#rows in
      body#set_empty ();
      List.iter (fun x -> Dom.appendChild body#root x#root) rows

    initializer
      self#set_dense dense;
      React.S.map (function
          | Some (index,sort) -> self#sort index sort
          | None -> ()) header#s_sorted
      |> self#_keep_s

  end
