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
  | _ -> None

type 'a custom =
  { to_string : 'a -> string
  ; compare : 'a -> 'a -> int
  ; is_numeric : bool
  }

type 'a custom_elt =
  { to_elt : 'a -> Tyxml_js.Xml.elt
  ; compare : 'a -> 'a -> int
  ; is_numeric : bool
  }

let default_time = Format.asprintf "%a" Ptime.pp

let default_float = Printf.sprintf "%f"

let identity : 'a. 'a -> 'a = fun x -> x

type _ fmt =
  | String : (string -> string) option -> string fmt
  | Int : (int -> string) option -> int fmt
  | Int32 : (int32 -> string) option -> int32 fmt
  | Int64 : (int64 -> string) option -> int64 fmt
  | Float : (float -> string) option -> float fmt
  | Time : (Ptime.t -> string) option -> Ptime.t fmt
  | Option : ('a fmt * string) -> 'a option fmt
  (* XXX maybe just Tyxml, not Tyxml_js? *)
  | Html : Xml.elt custom_elt option -> Xml.elt fmt
  | Widget : 'a custom_elt option -> (#Widget.t as 'a) fmt
  | Custom : 'a custom -> 'a fmt
  | Custom_elt : 'a custom_elt -> 'a fmt

type column =
  { sortable : bool
  ; title : string
  }

let to_column ?(sortable = false) title =
  { sortable; title }

let rec to_string : type a. a fmt -> (a -> string) = fun fmt ->
  let open Option in
  match fmt with
  | Int x -> get_or ~default:string_of_int x
  | Int32 x -> get_or ~default:Int32.to_string x
  | Int64 x -> get_or ~default:Int64.to_string x
  | Float x -> get_or ~default:default_float x
  | String x -> get_or ~default:identity x
  | Time x -> get_or ~default:default_time x
  | Option (fmt, e) ->
     begin function
       | None -> e
       | Some v -> (to_string fmt) v
     end
  | Custom x -> x.to_string
  (* Not allowed *)
  | Html _ -> assert false
  | Widget _ -> assert false
  | Custom_elt _ -> assert false

let map_or = Option.map_or

let rec is_numeric : type a. a fmt -> bool = function
  | Int _ -> true
  | Int32 _ -> true
  | Int64 _ -> true
  | Float _ -> true
  | String _ -> false
  | Time _ -> false
  | Option (fmt, _) -> is_numeric fmt
  | Html x -> map_or ~default:false (fun x -> x.is_numeric) x
  | Widget x -> map_or ~default:false (fun x -> x.is_numeric) x
  | Custom x -> x.is_numeric
  | Custom_elt x -> x.is_numeric

let rec compare : type a. a fmt -> (a -> a -> int) = fun fmt->
  match fmt with
  | Int _ -> Int.compare
  | Int32 _ -> Int32.compare
  | Int64 _ -> Int64.compare
  | Float _ -> Float.compare
  | String _ -> String.compare
  | Time _ -> Ptime.compare
  | Option (fmt, _) -> Option.compare (compare fmt)
  | Html x -> map_or ~default:(fun _ _ -> 0) (fun x -> x.compare) x
  | Widget x -> map_or ~default:(fun _ _ -> 0) (fun x -> x.compare) x
  | Custom x -> x.compare
  | Custom_elt x -> x.compare

module Cell = struct

  let rec set_value : type a. a fmt -> bool -> a option -> a -> Widget.t -> bool =
    fun fmt force prev v w ->
    let f = fun prev -> Int.equal 0 @@ compare fmt prev v in
    let eq = map_or ~default:false f prev in
    match force || not eq with
    | false -> false
    | true  ->
       let set_text = w#set_text_content in
       begin match fmt with
       | Option (fmt, e) ->
          begin match v with
          | None -> ignore @@ set_value (String None) force None e w
          | Some x ->
             begin match prev with
             | Some p -> ignore @@ set_value fmt force p x w
             | None   -> ignore @@ set_value fmt force None x w
             end
          end
       | Html x   ->
          let to_elt = map_or ~default:identity (fun x -> x.to_elt) x in
          w#set_empty ();
          Dom.appendChild w#root (to_elt v)
       | Custom_elt x ->
          w#set_empty ();
          Dom.appendChild w#root (x.to_elt v)
       | Widget x ->
          let to_elt =
            map_or ~default:(fun x -> x#node) (fun x -> x.to_elt) x in
          w#set_empty ();
          Dom.appendChild w#root (to_elt v)
       | _ -> set_text @@ to_string fmt v
       end;
       true

  let rec update : type a. a fmt -> a -> Widget.t -> unit =
    fun fmt v w ->
    match fmt with
    | Html _ -> ()
    | Widget _ -> ()
    | Option (fmt, e) ->
       begin match v with
       | Some v -> update fmt v w
       | None -> update (String None) e w
       end
    | _ -> ignore @@ set_value fmt true None v w

  let wrap_checkbox = function
    | None -> None
    | Some cb -> Markup.Cell.create [ Widget.to_markup cb ] ()
                 |> Option.return

  class ['a] t ~(value : 'a) (fmt : 'a fmt) () =
    let is_numeric = is_numeric fmt in
    let elt = Markup.Cell.create ~is_numeric [] ()
              |> To_dom.of_element in
    object(self : 'self)
      val mutable _value = value
      val mutable _fmt = fmt

      inherit Widget.t elt ()

      method format : 'a fmt = _fmt
      method set_format : 'a fmt -> unit = fun (x : 'a fmt) ->
        _fmt <- x;
        update x _value self#widget

      method value : 'a = _value
      method set_value ?(force = false) (v : 'a) =
        match set_value self#format force (Some _value) v self#widget with
        | false -> ()
        | true -> _value <- v

      method compare (other : 'self) =
        compare self#format other#value self#value

      initializer
        (* Initialize cell value *)
        self#set_value ~force:true value;
        (* Code to show tooltip if text in a cell overflows *)
        self#listen_lwt Widget.Event.mouseover (fun _ _ ->
            let overflow = self#scroll_width > self#client_width in
            let title =
              Option.choice [ self#get_attribute "data-title"
                            ; self#get_attribute "title"
                            ; self#text_content ]
              |> Option.flat_map (fun s ->
                     if String.is_empty s
                     then None else Some s) in
            begin match overflow, title with
            | true, Some title -> self#set_attribute "title" title
            | _ -> self#remove_attribute "title"
            end;
            Lwt.return_unit) |> Lwt.ignore_result;
        self#listen_lwt Widget.Event.mouseout (fun _ _ ->
            self#remove_attribute "title";
            Lwt.return_unit) |> Lwt.ignore_result
    end

end

module Column = struct

  let wrap_checkbox = function
    | None -> None
    | Some cb ->
       Markup.Column.create (Widget.to_markup cb) ()
       |> Option.return

  class t i push ({ sortable; title = text; _ } : column) (fmt : 'a fmt) () =
    let is_numeric = is_numeric fmt in
    let content = Html.(pcdata text) in
    let elt = Markup.Column.create ~sortable
                ~is_numeric content ()
              |> To_dom.of_element in
    object(self)
      inherit Widget.t elt ()

      method index : int = i

      initializer
        if sortable then
          self#listen_lwt Widget.Event.click (fun _ _ ->
              let order = self#get_attribute "aria-sort" in
              (match Option.flat_map sort_of_string order with
               | Some Dsc -> push (Some (self#index, Asc))
               | _ -> push (Some (self#index, Dsc)));
              Lwt.return_unit) |> Lwt.ignore_result
    end
end

module Data = struct
  type _ t =
    | [ ] : unit t
    | (::) : 'a * 'b t -> ('a * 'b) t
end

module Format = struct
  type _ t =
    | [ ] : unit t
    | (::) : (column * 'a fmt) * 'b t -> ('a * 'b) t
end

type _ cells =
  | [ ] : unit cells
  | (::) : 'a Cell.t * 'b cells -> ('a * 'b) cells

module Row = struct

  let rec make_cells : type a. a Format.t -> a Data.t -> a cells =
    fun format data ->
    match format, data with
    | Format.[], Data.[] -> []
    | Format.((_, fmt) :: l1), Data.(x :: l2) ->
       let cell = new Cell.t ~value:x fmt () in
       cell :: make_cells l1 l2

  let rec cells_to_widgets : type a. a cells -> Widget.t list =
    function
    | [] -> []
    | cell :: rest ->
       List.cons cell#widget (cells_to_widgets rest)

  let rec set_format : type a. a Format.t -> a cells -> unit =
    fun format cells ->
    match format, cells with
    | Format.[], [] -> ()
    | Format.((_, fmt) :: l1), x :: l2 ->
       x#set_format fmt;
       set_format l1 l2

  class ['a] t
          ?selection
          (s_selected : 'a t list React.signal)
          (set_selected : 'a t list -> unit)
          (fmt : 'a Format.t)
          (data : 'a Data.t)
          () =
    let cb  = match selection with
      | Some `Multiple -> Some (new Checkbox.t ())
      | _ -> None in
    let cells = make_cells fmt data in
    let cells' = cells_to_widgets cells in
    let elt = Markup.Row.create
                ~cells:(List.map Widget.to_markup cells'
                        |> List.cons_maybe (Cell.wrap_checkbox cb)) ()
              |> To_dom.of_element in
    object(self)
      val mutable _fmt : 'a Format.t = fmt

      inherit Widget.t elt ()

      method set_format (x : 'a Format.t) : unit =
        _fmt <- x;
        set_format x self#cells

      method checkbox = cb

      method cells = cells
      method cells_widgets = cells'

      method update ?force (data : 'a Data.t) =
        let rec aux : type a. a Data.t -> a cells -> unit =
          fun data cells ->
          match data, cells with
          | Data.[], [] -> ()
          | Data.(::) (x, l1), cell :: l2 ->
             cell#set_value ?force x;
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
        self#set_selected' x;
        match selection with
        | Some `Single ->
           if x
           then
             begin match React.S.value s_selected with
             | [] -> set_selected @@ List.pure v
             | l -> List.iter (fun (t : 'a t) -> t#set_selected' x) l;
                    set_selected @@ List.pure v
             end
           else
             begin match React.S.value s_selected with
             | [] -> ()
             | l -> List.remove ~eq:Equal.physical ~x:v l
                    |> set_selected
             end
        | Some `Multiple ->
           let l =
             if x
             then List.add_nodup ~eq:Equal.physical
                    (self :> 'a t)
                    (React.S.value s_selected)
             else List.remove ~eq:Equal.physical ~x:v
                    (React.S.value s_selected)
           in set_selected l
        | None -> ()

      initializer
        match selection with
        | Some _ ->
           self#listen_lwt Widget.Event.click (fun _ _ ->
               self#set_selected (not self#selected);
               Lwt.return_unit)
           |> Lwt.ignore_result
        | _ -> ()
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
      | (c, fmt) :: rest ->
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
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
      method checkbox = cb
    end

  let iter = Option.iter

  class ['a] t ?selection s_selected set_selected s_rows fmt () =
    let s_sorted, set_sorted = React.S.create None in
    let columns = make_columns set_sorted fmt in
    let row = new row ?selection ~columns () in
    let elt = Markup.Header.create ~row:(Widget.to_markup row) ()
              |> To_dom.of_element in
    object(self)
      val _columns = columns
      inherit Widget.t elt ()

      method checkbox = row#checkbox
      method columns = _columns
      method s_sorted = s_sorted

      method select_all () =
        List.iter (fun (r : 'a Row.t) -> r#set_selected true) self#_rows
      method unselect_all () =
        List.iter (fun (r : 'a Row.t) -> r#set_selected false) self#_rows

      method private _rows = React.S.value s_rows

      initializer
        iter (fun (cb : #Widget.t) ->
            cb#listen_lwt Widget.Event.change (fun _ _ ->
                if cb#checked
                then (List.iter (fun r -> r#set_selected' true) self#_rows;
                      set_selected self#_rows)
                else (List.iter (fun r -> r#set_selected' false) self#_rows;
                      set_selected List.empty);
                Lwt.return_unit)
            |> Lwt.ignore_result) self#checkbox;
        React.S.map (fun (x : 'a Row.t list) ->
            match List.length x with
            | 0 ->
               iter (fun cb ->
                   cb#set_checked false;
                   cb#set_indeterminate false) self#checkbox
            | l when l = List.length self#_rows ->
               iter (fun cb ->
                   cb#set_checked true;
                   cb#set_indeterminate false) self#checkbox
            | _ ->
               iter (fun cb ->
                   cb#set_indeterminate true;
                   cb#set_checked false) self#checkbox) s_selected
        |> self#_keep_s;
        React.S.map (function
            | Some (index, sort) ->
               List.iter (fun (x : Column.t) ->
                   if index = x#index
                   then x#set_attribute "aria-sort" (sort_to_string sort)
                   else x#remove_attribute "aria-sort") _columns
            | None ->
               List.iter (fun x -> x#remove_attribute "aria-sort")
                 _columns)
          s_sorted |> self#_keep_s
    end
end

module Body = struct

  type pagination =
    { rpp : int
    ; offset : int
    }

  class ['a] t () =
    let elt = Markup.Body.create ~rows:[] ()
              |> To_dom.of_element in
    let s_rows, s_rows_push = React.S.create List.empty in
    object(self)

      val mutable _pagination : pagination option = None

      inherit Widget.t elt () as super

      method prepend_row (row : 'a Row.t) : unit =
        s_rows_push (List.cons row self#rows);
        Dom.insertBefore self#root row#root self#root##.firstChild;
        self#layout ();

      method append_row (row : 'a Row.t) : unit =
        s_rows_push (self#rows @ [row]);
        self#append_child row;
        self#layout ();

      method remove_row (row : 'a Row.t) : unit =
        s_rows_push @@ List.remove ~eq:Widget.equal ~x:row self#rows;
        self#remove_child row;
        self#layout ();

      method remove_all_rows () : unit =
        self#set_empty ();
        s_rows_push [];
        self#layout ();

      method s_rows : 'a Row.t list React.signal =
        s_rows

      method rows : 'a Row.t list =
        List.rev @@ React.S.value s_rows

      method set_pagination (x : pagination option) : unit =
        _pagination <- x;
        self#layout ()

      method! layout () =
        super#layout ();
        (* match _pagination with
         * | None ->
         *    List.iter self#append_child (React.S.value s_rows)
         * | Some { rpp; offset } ->
         *    let rows = React.S.value s_rows in
         *    let prev, visible' = List.take_drop offset rows in
         *    let visible, next = List.take_drop rpp visible' in
         *    List.iter self#remove_child (prev @ next);
         *    List.iter self#append_child visible; *)
    end
end

module Footer = struct

  type per_page =
    | Num of int
    | All

  let rec col_num : type a. a Format.t -> int = fun l ->
    let open Format in
    match l with
    | [] -> 0
    | _ :: tl -> 1 + (col_num tl)

  module Select = struct

    class t ?(all : string option)
            (num : int list)
            () =
      let items =
        let init = match all with
          | None -> List.empty
          | Some s -> [ `Item (new Select.Item.t ~value:All ~text:s ()) ] in
        let items =
          List.map (fun x ->
              let text = string_of_int x in
              let item = new Select.Item.t ~value:(Num x) ~text () in
              `Item item) num in
        items @ init in
      let select = new Select.t
                     ~bottom_line:false
                     ~items () in
      let elt = Markup.Footer.create_select
                  (Widget.to_markup select) ()
                |> To_dom.of_element in
      object
        inherit Widget.t elt ()

        method select = select
      end

  end

  class t ?(spacer = true)
          ?(rows_per_page : (string * Select.t) option)
          ?(actions = List.empty)
          () =
    let spacer = match spacer with
      | false -> None
      | true -> Option.return @@ Markup.Footer.create_spacer () in
    let (rpp : 'a list) = match rows_per_page with
      | Some (s, select) ->
         let caption = Markup.Footer.create_caption s () in
         [ caption; Widget.to_markup select ]
      | None -> [] in
    let (actions : 'a list) = match actions with
      | [] -> []
      | l -> let actions = List.map Widget.to_markup l in
             [ Markup.Footer.create_actions actions () ] in
    let content = List.cons_maybe spacer rpp @ actions in
    let elt = Markup.Footer.create_toolbar content ()
              |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

end

module Table = struct
  class t ?header ~body ?footer () =
    let elt = Markup.create_table
                ?header:(Option.map Widget.to_markup header)
                ~body:(Widget.to_markup body)
                ?footer:(Option.map Widget.to_markup footer)
                ()
              |> To_dom.of_element in
    object
      inherit Widget.t elt ()
      initializer
        body#layout ()
    end
end

let rec compare : type a. int ->
                       a cells ->
                       a cells -> int =
  fun index cells1 cells2 ->
  match cells1, cells2 with
  | [], [] -> 0
  | x1 :: rest1, x2 :: rest2 ->
     if index = 0
     then x1#compare x2
     else compare (pred index) rest1 rest2

class ['a] t ?selection
        ?footer
        ?(sticky_header = false)
        ?(dense = false)
        ~(fmt : 'a Format.t) () =
  let s_selected, set_selected = React.S.create List.empty in
  let body = new Body.t () in
  let header = new Header.t ?selection s_selected
                 set_selected body#s_rows fmt () in
  let table = new Table.t ~header ~body () in
  let content =
    Markup.create_content ~table:(Widget.to_markup table) ()
    |> To_dom.of_element
    |> Widget.create in
  let elt =
    Markup.create ?selection
      ?footer:(Option.map Widget.to_markup footer)
      ~content:(Widget.to_markup content) ()
    |> To_dom.of_element in
  object(self)
    inherit Widget.t elt ()

    val mutable _fmt : 'a Format.t = fmt

    method content : Widget.t =
      content

    method table : Table.t =
      table

    method body : 'a Body.t =
      body

    method rows : 'a Row.t list =
      body#rows

    method s_rows : 'a Row.t list React.signal =
      body#s_rows

    method is_empty : bool = match self#rows with
      | [] -> true
      | _ -> false

    method set_format (x : 'a Format.t) : unit =
      _fmt <- x;
      List.iter (fun row -> row#set_format x) self#rows

    method s_selected : 'a Row.t list React.signal =
      s_selected

    method sticky_header : bool =
      self#has_class Markup.sticky_header_class
    method set_sticky_header (x : bool) : unit =
      self#add_or_remove_class x Markup.sticky_header_class

    method dense : bool =
      self#has_class Markup.dense_class
    method set_dense x =
      self#add_or_remove_class x Markup.dense_class

    method prepend_row (data : 'a Data.t) : 'a Row.t =
      let row = self#_make_row data in
      body#prepend_row row;
      row

    method append_row (data : 'a Data.t) : 'a Row.t =
      let row = self#_make_row data in
      body#append_row row;
      row

    method add_row (data : 'a Data.t) : 'a Row.t =
      let row = self#_make_row data in
      body#append_row row;
      row

    method remove_row (row : 'a Row.t) : unit =
      body#remove_row row

    method remove_all_rows () : unit =
      body#remove_all_rows ()

    method sort index sort =
      let rows =
        List.sort (fun (row_1 : 'a Row.t)
                       (row_2 : 'a Row.t) ->
            match sort, compare index row_1#cells row_2#cells with
            | Asc, x -> x
            | Dsc, x -> ~-x) self#rows in
      (* FIXME do it in a more smart way. Move only those rows that
         have changed their positions *)
      body#set_empty ();
      List.iter body#append_child rows

    (* Private methods *)

    method private _make_row (data : 'a Data.t) : 'a Row.t =
      new Row.t ?selection s_selected set_selected _fmt data ()

    initializer
      self#set_dense dense;
      self#set_sticky_header sticky_header;
      React.S.map (function
          | Some (index, sort) -> self#sort index sort
          | None -> ()) header#s_sorted
      |> self#_keep_s

  end
