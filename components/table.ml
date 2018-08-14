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
  ; compare    : 'a -> 'a -> int
  ; is_numeric : bool
  }

type 'a custom_elt =
  { to_elt     : 'a -> Tyxml_js.Xml.elt
  ; compare    : 'a -> 'a -> int
  ; is_numeric : bool
  }

let default_time = Format.asprintf "%a" Ptime.pp

let default_float = Printf.sprintf "%f"

type _ fmt =
  | String     : (string -> string) option -> string fmt
  | Int        : (int -> string) option -> int fmt
  | Int32      : (int32 -> string) option -> int32 fmt
  | Int64      : (int64 -> string) option -> int64 fmt
  | Float      : (float -> string) option   -> float fmt
  | Time       : (Ptime.t -> string) option -> Ptime.t fmt
  | Option     : ('a fmt * string) -> 'a option fmt
  (* XXX maybe just Tyxml, not Tyxml_js? *)
  | Html       : Tyxml_js.Xml.elt custom_elt option -> Tyxml_js.Xml.elt fmt
  | Widget     : 'a custom_elt option -> (#Widget.t as 'a) fmt
  | Custom     : 'a custom     -> 'a fmt
  | Custom_elt : 'a custom_elt -> 'a fmt

type column =
  { sortable : bool
  ; title    : string
  }

let to_column ?(sortable=false) title =
  { sortable; title }

let rec to_string : type a. a fmt -> (a -> string) = fun fmt ->
  let open Option in
  match fmt with
  | Int x          -> (get_or ~default:string_of_int x)
  | Int32 x        -> (get_or ~default:Int32.to_string x)
  | Int64 x        -> (get_or ~default:Int64.to_string x)
  | Float x        -> (get_or ~default:default_float x)
  | String x       -> (get_or ~default:(fun x -> x) x)
  | Time x         -> (get_or ~default:default_time x)
  | Option (fmt,e) -> (function
                       | None   -> e
                       | Some v -> (to_string fmt) v)
  | Custom x       -> x.to_string
  | Html _         -> fun _ -> ""
  | Widget _       -> fun _ -> ""
  | Custom_elt _   -> fun _ -> ""

let rec is_numeric : type a. a fmt -> bool = function
  | Int _          -> true
  | Int32 _        -> true
  | Int64 _        -> true
  | Float _        -> true
  | String _       -> false
  | Time _         -> false
  | Option (fmt,_) -> is_numeric fmt
  | Html x         ->
     Option.map_or ~default:false (fun x -> x.is_numeric) x
  | Widget x       ->
     Option.map_or ~default:false (fun x -> x.is_numeric) x
  | Custom x       -> x.is_numeric
  | Custom_elt x   -> x.is_numeric

let rec compare : type a. a fmt -> (a -> a -> int) = fun fmt->
  match fmt with
  | Int _          -> Int.compare
  | Int32 _        -> Int32.compare
  | Int64 _        -> Int64.compare
  | Float _        -> Float.compare
  | String _       -> String.compare
  | Time _         -> Ptime.compare
  | Option (fmt,_) -> Option.compare (compare fmt)
  | Html x         ->
     Option.map_or ~default:(fun _ _ -> 0) (fun x -> x.compare) x
  | Widget x       ->
     Option.map_or ~default:(fun _ _ -> 0) (fun x -> x.compare) x
  | Custom x       -> x.compare
  | Custom_elt x   -> x.compare

module Cell = struct

  let rec set_value : type a. a fmt -> bool -> a option -> a -> Widget.t -> bool =
    fun fmt force prev v w ->
    let f  = fun prev -> Int.equal 0 @@ compare fmt prev v in
    let eq = Option.map_or ~default:false f prev in
    match force || not eq with
    | false -> false
    | true  ->
       let set_text = w#set_text_content in
       (match fmt with
        | Option (fmt,e) ->
           (match v with
            | None   ->
               ignore @@ set_value (String None) force None e w
            | Some x ->
               (match prev with
                | Some p -> ignore @@ set_value fmt force p x w
                | None   -> ignore @@ set_value fmt force None x w))
        | Html x   ->
           let to_elt = Option.map_or ~default:(fun x -> x)
                          (fun x -> x.to_elt) x in
           w#set_empty ();
           Dom.appendChild w#root (to_elt v)
        | Custom_elt x ->
           w#set_empty ();
           Dom.appendChild w#root (x.to_elt v)
        | Widget x ->
           let to_elt =
             Option.map_or ~default:(fun x -> x#node) (fun x -> x.to_elt) x in
           w#set_empty ();
           Dom.appendChild w#root (to_elt v)
        | _ -> set_text @@ to_string fmt v);
       true

  let rec update : type a. a fmt -> a -> Widget.t -> unit =
    fun fmt v w ->
    match fmt with
    | Html _   -> ()
    | Widget _ -> ()
    | Option (fmt, e) ->
       (match v with
        | Some v -> update fmt v w
        | None   -> update (String None) e w)
    | _ -> ignore @@ set_value fmt true None v w

  let wrap_checkbox = function
    | None    -> None
    | Some cb -> Markup.Cell.create [ Widget.to_markup cb ] ()
                 |> Option.return

  class ['a] t ~(value:'a) (fmt:'a fmt) () =
    let elt = Markup.Cell.create ~is_numeric:(is_numeric fmt) [] ()
              |> Tyxml_js.To_dom.of_element in
    object(self : 'self)
      val mutable _value = value
      val mutable _fmt   = fmt

      inherit Widget.t elt ()

      method format : 'a fmt = _fmt
      method set_format : 'a fmt -> unit = fun (x:'a fmt) ->
        _fmt <- x;
        update x _value self#widget

      method value : 'a = _value
      method set_value ?(force=false) (v:'a) =
        match set_value fmt force (Some _value) v self#widget with
        | false -> ()
        | true  -> _value <- v

      method compare (other:'self) = compare fmt other#value self#value

      initializer
        self#set_value ~force:true value
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

module Data = struct
  type _ t =
    | []   : unit t
    | (::) : 'a * 'b t -> ('a * 'b) t
end

module Format = struct
  type _ t =
    | []   : unit t
    | (::) : (column * 'a fmt) * 'b t -> ('a * 'b) t
end

type _ cells =
  | []   : unit cells
  | (::) : 'a Cell.t * 'b cells -> ('a * 'b) cells

module Row = struct

  let rec make_cells : type a. a Format.t -> a Data.t -> a cells =
    fun format data ->
    match format, data with
    | Format.[], Data.[] -> []
    | Format.((_, fmt) :: l1), Data.( x :: l2) ->
       let cell = new Cell.t ~value:x fmt () in
       cell :: make_cells l1 l2

  let rec cells_to_widgets : type a. a cells -> Widget.t list =
    function
    | [] -> []
    | cell :: rest ->
       List.cons cell#widget (cells_to_widgets rest)

  class ['a] t
          ?selection
          (s_selected:'a t list React.signal)
          (s_selected_push:'a t list -> unit)
          (fmt:'a Format.t)
          (data:'a Data.t)
          () =
    let cb  = match selection with
      | Some `Multiple -> Some (new Checkbox.t ())
      | _ -> None in
    let cells  = make_cells fmt data in
    let cells' = cells_to_widgets cells in
    let elt = Markup.Row.create
                ~cells:(List.map Widget.to_markup cells'
                        |> List.cons_maybe (Cell.wrap_checkbox cb)) ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.t elt ()
      method checkbox   = cb

      method cells = cells
      method cells_widgets = cells'

      method update (data:'a Data.t) =
        let rec aux : type a. a Data.t -> a cells -> unit =
          fun data cells ->
          match data, cells with
          | Data.[], Cell.[] -> ()
          | Data.(::) (x, l1), cell :: l2 ->
             cell#set_value x;
             aux l1 l2 in
        aux data self#cells

      method data : 'a Data.t =
        let rec aux : type a. a cells -> a Data.t =
          function
          | [] -> []
          | cell :: rest -> cell#value :: aux rest in
        aux self#cells

      method s_selected = s_selected

      method selected = self#has_class Markup.Row.selected_class

      method set_selected' x =
        Option.iter (fun cb -> cb#set_checked x) self#checkbox;
        self#add_or_remove_class x Markup.Row.selected_class

      method set_selected x  =
        let v = (self :> 'a t) in
        (match selection with
         | Some `Single ->
            if x
            then (match React.S.value s_selected with
                  | [] -> s_selected_push @@ List.pure v
                  | l  -> List.iter (fun (t:'a t) -> t#set_selected' x) l;
                          s_selected_push @@ List.pure v)
            else (match React.S.value s_selected with
                  | [] -> ()
                  | l  -> List.remove ~eq:Equal.physical ~x:v l
                          |> s_selected_push)
         | Some `Multiple ->
            let l =
              if x
              then List.add_nodup ~eq:Equal.physical
                     (self :> 'a t)
                     (React.S.value s_selected)
              else List.remove ~eq:Equal.physical ~x:v
                     (React.S.value s_selected)
            in s_selected_push l
         | None -> ());
        self#set_selected' x

      initializer
        match selection with
        | Some _ -> Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
                        self#set_selected (not self#selected); true)
                    |> ignore
        | _      -> ()
    end

end

module Body = struct
  class ['a] t ?selection () =
    let elt = Markup.Body.create ~rows:[] ()
              |> Tyxml_js.To_dom.of_element in
    let s_rows,s_rows_push = React.S.create List.empty in
    object(self)
      inherit Widget.t elt ()
      method prepend_row (row:'a Row.t) =
        s_rows_push @@ (List.cons row self#rows);
        Dom.insertBefore self#root row#root self#root##.firstChild
      method append_row (row:'a Row.t) =
        s_rows_push @@ (List.cons row self#rows);
        Dom.appendChild self#root row#root
      method s_rows = s_rows
      method rows   = List.rev @@ React.S.value s_rows
    end
end

module Header = struct

  let rec make_columns : type a. ((int * sort) option -> unit) ->
                              a Format.t -> Column.t list =
    fun push fmt ->
    let rec loop : type a. int -> ((int * sort) option -> unit) ->
                        a Format.t -> Column.t list =
      fun i push fmt ->
      let open Format in
      match fmt with
      | [] -> []
      | (c,fmt) :: rest ->
         (new Column.t i push c fmt ()) :: (loop (succ i) push rest)
    in loop 0 push fmt

  class row ?selection ~(columns:Column.t list) () =
    let cb = match selection with
      | Some `Multiple -> Some (new Checkbox.t ())
      | _ -> None in
    let elt =
      Markup.Row.create
        ~cells:(List.map Widget.to_markup columns
                |> List.cons_maybe (Column.wrap_checkbox cb)) ()
      |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.t elt ()
      method checkbox = cb
    end

  class ['a] t ?selection s_selected s_selected_push s_rows fmt () =
    let s_sorted,s_sorted_push = React.S.create None in
    let columns = make_columns s_sorted_push fmt in
    let row = new row ?selection ~columns () in
    let elt = Markup.Header.create ~row:(Widget.to_markup row) ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      val _columns = columns
      inherit Widget.t elt ()

      method checkbox = row#checkbox
      method columns  = _columns
      method s_sorted = s_sorted

      method select_all () =
        List.iter (fun (r:'a Row.t) -> r#set_selected true) self#_rows
      method unselect_all () =
        List.iter (fun (r:'a Row.t) -> r#set_selected false) self#_rows

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
        React.S.map (fun (x:'a Row.t list) ->
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

let rec compare : type a. int ->
                       a cells ->
                       a cells -> int =
  fun index cells1 cells2 ->
  let open Cell in
  match cells1, cells2 with
  | [], [] -> 0
  | x1 :: rest1, x2 :: rest2 ->
     if index = 0
     then x1#compare x2
     else compare (pred index) rest1 rest2

class ['a] t ?selection ?(sticky_header=false) ?(dense=false)
        ~(fmt:'a Format.t) () =
  let s_selected,s_selected_push = React.S.create List.empty in
  let body   = new Body.t ?selection () in
  let header = new Header.t ?selection s_selected
                 s_selected_push body#s_rows fmt () in
  let table  = new Table.t ~header ~body () in
  let elt    = Markup.create ?selection ~table:(Widget.to_markup table) ()
               |> Tyxml_js.To_dom.of_element in
  object(self)
    inherit Widget.t elt ()

    val mutable _fmt : 'a Format.t = fmt

    method body       = body
    method rows       = body#rows
    method s_rows     = body#s_rows
    method s_selected = s_selected

    method sticky_header : bool =
      self#has_class Markup.sticky_header_class
    method set_sticky_header x =
      self#add_or_remove_class x Markup.sticky_header_class

    method dense : bool =
      self#has_class Markup.dense_class
    method set_dense x =
      self#add_or_remove_class x Markup.dense_class

    method add_row (data:'a Data.t) =
      let row =
        new Row.t ?selection
          s_selected
          s_selected_push
          fmt data () in
      body#append_row row;
      row

    method sort index sort =
      let rows =
        List.sort (fun (row_1:'a Row.t)
                       (row_2:'a Row.t) ->
            match sort, compare index row_1#cells row_2#cells with
            | Asc,x -> x
            | Dsc,x -> ~-x) self#rows in
      (* FIXME do it in a more smart way. Move only those rows that
         have changed their positions *)
      body#set_empty ();
      List.iter (fun x -> Dom.appendChild body#root x#root) rows

    initializer
      self#set_dense dense;
      self#set_sticky_header sticky_header;
      React.S.map (function
          | Some (index,sort) -> self#sort index sort
          | None -> ()) header#s_sorted
      |> self#_keep_s

  end
