open Containers

type _ fmt =
  | String : string fmt
  | Int    : int fmt
  | Int32  : int32 fmt
  | Int64  : int64 fmt
  | Custom : ('a -> string) * bool -> 'a fmt
  | Option : ('a fmt) -> 'a option fmt

type (_,_) row =
  | (::) : (string * 'a fmt) * ('b,'c) row -> ('a -> 'b,'c) row
  | []    : ('a,'a) row

let rec to_string : type a. a fmt -> a -> string = fun fmt v ->
  match fmt with
  | Int          -> string_of_int v
  | Int32        -> Int32.to_string v
  | Int64        -> Int64.to_string v
  | String       -> v
  | Option (fmt) -> (match v with None -> "" | Some v -> (to_string fmt) v)
  | Custom (f,_) -> f v

let rec is_numeric : type a. a fmt -> bool = function
  | Int          -> true
  | Int32        -> true
  | Int64        -> true
  | String       -> false
  | Option (fmt) -> is_numeric fmt
  | Custom (_,x) -> x

module Cell = struct
  class t ~(value:'a) (fmt:'a fmt) (conv:'a -> string) () =
    let s   = conv value in
    let content = Tyxml_js.Html.(pcdata s) in
    let elt = Markup.Table.Cell.create ~is_numeric:(is_numeric fmt) content ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.widget elt ()
    end
end

module Column = struct
  class t ~(value:string) (fmt:'a fmt) () =
    let content = Tyxml_js.Html.(pcdata value) in
    let elt = Markup.Table.Cell.create ~header:true ~is_numeric:(is_numeric fmt) content ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.widget elt ()
    end
end

module Row = struct
  class t ~cells () =
    let elt = Markup.Table.Row.create ~cells:(List.map Widget.widget_to_markup cells) ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.widget elt ()
    end
end

module Header = struct
  class t ~cells () =
    let row = new Row.t ~cells () in
    let elt = Markup.Table.Header.create ~row:(Widget.widget_to_markup row) ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.widget elt ()
    end
end

module Body = struct
  class t ~rows () =
    let elt = Markup.Table.Body.create ~rows:(List.map Widget.widget_to_markup rows) ()
              |> Tyxml_js.To_dom.of_element in
    object
      inherit Widget.widget elt ()
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

let rec make_columns : type a b. (a,b) row -> Column.t list = function
  | [] -> []
  | (::) ((s,fmt),rest) -> (new Column.t ~value:s fmt ()) :: (make_columns rest)

let rec make_cells : type ty v. (Cell.t list -> v) -> (ty,v) row -> ty =
  fun k ->
  function
  | [] -> k []
  | (::) ((_,fmt),rest) ->
     let f x = make_cells (fun acc -> let cell = new Cell.t ~value:x fmt (to_string fmt) () in
                                      k @@ cell :: acc) rest in f

let make_header : type a b. (a,b) row -> Header.t = fun fmt ->
  let cells = make_columns fmt in
  new Header.t ~cells ()

class ['a] t ~(fmt:('a,_) row) () =
  let body   = new Body.t ~rows:[] () in
  let header = make_header fmt in
  let table  = new Table.t ~header ~body () in
  let elt    = Markup.Table.create ~table:(Widget.widget_to_markup table) ()
               |> Tyxml_js.To_dom.of_element in
  object(self)
    inherit Widget.widget elt ()
    method add_row =
      make_cells (fun cells ->
          let row = new Row.t ~cells () in
          Dom.appendChild body#root row#root) fmt

    (* Private methods *)

    initializer
      Dom.appendChild self#root table#root
  end
