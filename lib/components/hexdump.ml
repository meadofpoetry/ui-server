open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

(* TODO
   * add range selection by holding shift
 *)

include Components_tyxml.Hexdump
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let elt_to_string (elt : 'a Tyxml.Html.elt) : string =
  Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt

let string_of_chars (l : char list) =
  let buf = Buffer.create @@ List.length l in
  List.iter (Buffer.add_char buf) l;
  Buffer.contents buf

class t (elt : Dom_html.element Js.t) () =
object(self)
  val num_block : Dom_html.element Js.t =
    find_element_by_class_exn elt CSS.block_line_numbers
  val hex_block : Dom_html.element Js.t =
    find_element_by_class_exn elt CSS.block_hex
  val chr_block : Dom_html.element Js.t =
    find_element_by_class_exn elt CSS.block_chars
  val mutable _click_listener = None
  val mutable _selected : #Dom_html.element Js.t list = []
  val mutable _bytes : string = ""

  (* Config *)
  val mutable _base : base =
    match Option.map base_of_string @@ Element.get_attribute elt "data-base" with
    | None -> Hex
    | Some x -> x
  val mutable _width : int =
    match Option.map int_of_string @@ Element.get_attribute elt "data-width" with
    | None -> (* TODO we can count number of <span> elements in a row *) 16
    | Some x -> x
  val mutable _grouping : int =
    match Option.map int_of_string @@ Element.get_attribute elt "data-grouping" with
    | None -> 1
    | Some x -> x
  val mutable _no_line_numbers : bool =
    Element.has_class elt CSS.no_line_numbers

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    (* XXX need testing *)
    let values = List.map self#_get_hex_char self#_hex_items in
    _bytes <- string_of_chars values;
    self#set_non_interactive false

  method base : base =
    _base

  method set_base (x : base) : unit =
    if not (equal_base x _base)
    then (_base <- x; self#set_bytes _bytes)

  method width : int =
    _width

  method set_width (x : int) : unit =
    if not (x = _width)
    then (_width <- x; self#set_bytes _bytes)

  method grouping : int =
    _grouping

  method set_grouping (x : int) : unit =
    if not (x = _grouping)
    then (_grouping <- x; self#set_bytes _bytes)

  method no_line_numbers : bool =
    _no_line_numbers

  method set_no_line_numbers (x : bool) : unit =
    if not (Bool.equal x _no_line_numbers)
    then (_no_line_numbers <- x;
          super#toggle_class ~force:x CSS.no_line_numbers)

  method set_bytes (bytes : string) : unit =
    _bytes <- bytes;
    let module M = Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html) in
    let num, hex, chr =
      M.create_rows
        ~width:_width
        ~grouping:_grouping
        ~base:_base
        bytes in
    let to_string x = String.concat "" @@ List.map elt_to_string x in
    num_block##.innerHTML := Js.string (to_string num);
    hex_block##.innerHTML := Js.string (to_string hex);
    chr_block##.innerHTML := Js.string (to_string chr)

  method non_interactive : bool =
    super#has_class CSS.non_interactive

  method set_non_interactive (x : bool) : unit =
    super#toggle_class ~force:x CSS.non_interactive;
    match x, _click_listener with
    | false, None ->
       let listener = Events.listen_lwt hex_block Events.Typ.click self#handle_click in
       _click_listener <- Some listener
    | true, Some l -> Lwt.cancel l; _click_listener <- None
    | _ -> ()

  method select ?(flush = true) (id : int) : unit =
    let eq = fun x -> id = self#_get_item_id x in
    match List.find_opt eq self#_hex_items with
    | None -> ()
    | Some elt ->
       (match List.find_opt (Element.equal elt) _selected with
        | Some _ -> () (* already selected *)
        | None  ->
           if flush then List.iter self#_unselect _selected;
           self#_select elt)

  method select_range ?(flush = true) (from : int) (till : int) : unit =
    if till < from || from < 0 || till < 0
    then raise_notrace (Invalid_argument "bad range") else if flush then
      List.iter self#_unselect _selected;
    let rec aux t = function
      | i when i > t -> ()
      | i -> self#select ~flush:false i;
             aux t (succ i) in
    aux till from

  (* Private methods *)

  method private _hex_items : Dom_html.element Js.t list =
    Element.children hex_block
    |> List.filter (fun x ->
           Option.is_some @@ Element.get_attribute x "data-id"
           && Element.has_class x CSS.item)
    |> fun x ->
       List.iter (fun x -> Js.Unsafe.global##.console##log x) x;
       x
    |> List.sort (fun e1 e2 ->
           compare (self#_get_item_id e1) (self#_get_item_id e2))

  method private _char_items : Dom_html.element Js.t list =
    Element.children chr_block
    |> List.filter (fun x ->
           Option.is_some @@ Element.get_attribute x "data-id"
           && Element.has_class x CSS.item)
    |> List.sort (fun e1 e2 ->
           compare (self#_get_item_id e1) (self#_get_item_id e2))

  method private _get_item_id (x : Dom_html.element Js.t) : int =
    match Element.get_attribute x "data-id" with
    | None -> failwith "hexdump: data-id attribute not found"
    | Some x ->
       match int_of_string_opt x with
       | None -> failwith "hexdump: bad data-id attribute value"
       | Some x -> x

  method private _get_hex_char (x : Dom_html.element Js.t) =
    let f_conv s = match _base with
      | Hex -> "0x" ^ s
      | Bin -> "0b" ^ s
      | Dec -> s in
    match Js.Opt.to_option x##.textContent with
    | None -> failwith "hexdump: textContent not found"
    | Some x ->
       match int_of_string_opt @@ f_conv @@ String.trim @@ Js.to_string x with
       | None -> failwith "hexdump: bad char content"
       | Some x -> Char.chr x

  method private _unselect x =
    List.find_opt ((=) (self#_get_item_id x) % self#_get_item_id) self#_char_items
    |> Option.iter (fun x -> Element.remove_class x CSS.item_selected);
    _selected <- List.remove ~eq:Element.equal x _selected;
    Element.remove_class x CSS.item_selected

  method private _select x =
    List.find_opt (fun c -> self#_get_item_id c = self#_get_item_id x) self#_char_items
    |> Option.iter (fun x -> Element.add_class x CSS.item_selected);
    Element.add_class x CSS.item_selected;
    _selected <- x :: _selected

  method private handle_click = fun e _ ->
    let ctrl = Js.to_bool e##.ctrlKey in
    let target = Js.Opt.to_option e##.target in
    let is_span =
      Option.map (fun e ->
          Option.is_some @@ Element.get_attribute e "data-id"
          && Element.has_class e CSS.item) target
      |> function None -> false | Some x -> x in
    begin match target, is_span with
    | Some e, true ->
       begin match List.find_opt (Element.equal e) _selected with
       | Some x ->
          if not ctrl
          then List.iter self#_unselect (List.remove ~eq:Element.equal x _selected)
          else self#_unselect x
       | None ->
          if not ctrl then List.iter self#_unselect _selected;
          self#_select e
       end
    | _ -> ();
    end;
    Lwt.return_unit
end

let make ?width ?grouping ?no_line_numbers ?non_interactive ?base
      (bytes : string) () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.of_bytes ?width ?grouping ?no_line_numbers ?non_interactive
         ?base bytes in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
