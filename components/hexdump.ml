open Containers
open Tyxml_js

let line_number_len = 4

module Markup = Components_markup.Hexdump.Make(Xml)(Svg)(Html)

type base = [`Hex | `Dec | `Bin ] [@@deriving eq]

let get_padding = function `Hex -> 2 | `Dec -> 3 | `Bin -> 8
let get_converter = function
  | `Hex -> Printf.sprintf "%x"
  | `Dec -> Printf.sprintf "%d"
  | `Bin -> fun x -> Int.to_string_binary x |> String.drop 2

type config =
  { style        : style
  ; base         : base
  ; width        : int
  ; grouping     : int
  ; line_numbers : bool
  }
and style =
  { line_number        : padding
  ; hex                : padding
  ; char               : padding
  ; empty_hex          : char
  ; empty_char         : char
  ; non_printable_char : char
  }
and padding =
  { prefix : string
  ; suffix : string
  }

let pad (p:padding) s = p.prefix ^ s ^ p.suffix

let to_style ?(line_number = { prefix = ""; suffix = "" })
      ?(hex ={prefix = ""; suffix = "" })
      ?(char={prefix = ""; suffix = "" })
      ?(empty_hex = '.')
      ?(empty_char = ' ')
      ?(non_printable_char = '.')
      () =
  { line_number; hex; char; empty_hex; empty_char; non_printable_char }

let to_config
      ?(style=to_style ())
      ?(base=`Hex)
      ?(width=16)
      ?(grouping=1)
      ?(line_numbers=true) () =
  { style; base; width; grouping; line_numbers }

let to_line_number i (config:config) =
  let cur_line_count = i * config.width in
  let s = String.pad ~c:'0' line_number_len (string_of_int cur_line_count) in
  let s = pad config.style.line_number s in
  Tyxml.Html.(span ~a:[a_class [Markup.Line_number._class]] [pcdata s])
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let to_hex_span ?value (config:config) =
  let empty = config.style.empty_hex in
  let udata_id, udata_val, s, _class = match value with
    | None ->
       "hex-empty","true",
       String.pad ~c:empty (get_padding config.base) (String.of_char empty),
       Markup.Hex.empty_class
    | Some (id,v) ->
       let s = (get_converter config.base) v
               |> String.pad ~c:'0' (get_padding config.base) in
       "hex-id", string_of_int id, s, Markup.Hex._class
  in
  Tyxml.Html.(span ~a:[ a_class [_class]; a_user_data udata_id udata_val ]
                [pcdata s])
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let is_printable (c:char) : bool =
  match Char.code c with
  | x when x > 31 && x < 128 -> true
  | _ -> false

let to_char_span ?value (config:config) =
  let empty = config.style.empty_char in
  let udata_id, udata_val, s, _class = match value with
    | Some (id,v) ->
       "char-id",string_of_int id,
       String.of_char (if is_printable v then v
                       else config.style.non_printable_char),
       Markup.Char._class
    | None ->
       "char-empty", "true", String.pad ~c:empty 1 (String.of_char empty),
       Markup.Char.empty_class
  in
  Tyxml.Html.(span ~a:[ a_class [_class]; a_user_data udata_id udata_val]
                [pcdata s])
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let make_data id (config:config) (data:char list) =
  let space cnt =
    let g = config.grouping in
    if g = 0 then false else (cnt mod g) = 0 in
  let conv_hex ?value () = to_hex_span ?value config in
  let conv_chr ?value () = to_char_span ?value config in
  let rec aux_empty cnt hex_acc chr_acc = function
    | x when x > 0 ->
       let cnt,hex_acc =
         let hex = hex_acc ^ (conv_hex ()) in
         let hex = if space cnt then hex ^ " " else hex in
         (succ cnt),hex
       in aux_empty cnt hex_acc (chr_acc ^ conv_chr ()) (pred x)
    | _ -> Option.get_or
             ~default:hex_acc
             (String.chop_suffix ~suf:" " hex_acc),chr_acc in
  let rec aux id cnt hex_acc chr_acc = function
    | [] ->
       let hex_acc, chr_acc =
         aux_empty cnt hex_acc chr_acc (config.width - cnt + 1)
       in id, hex_acc, chr_acc
    | hd::tl ->
       let cnt, hex_acc =
         let code = Char.code hd in
         let hex  = hex_acc ^ (conv_hex ~value:(id,code) ()) in
         let hex  = if space cnt then hex ^ " " else hex in
         (succ cnt), hex
       in
       let chr_acc = chr_acc ^ (conv_chr ~value:(id,hd) ()) in
       aux (succ id) cnt hex_acc chr_acc tl in
  let id, hex, chars = aux id 1 "" "" data in
  id,
  (pad config.style.hex hex)    ^ "\n",
  (pad config.style.char chars) ^ "\n"

class t ?(interactive=true)
        ~(config:config)
        (data:string) () =
  let num_elt = Markup.create_block ()
                |> To_dom.of_element |> Widget.create in
  let hex_elt = Markup.create_block ()
                |> To_dom.of_element |> Widget.create in
  let chr_elt = Markup.create_block ()
                |> To_dom.of_element |> Widget.create in
  object(self)
    val mutable _listener = None
    val mutable _selected : #Dom_html.element Js.t list = []
    val mutable _config   : config = config
    val mutable _bytes    : string = data

    inherit Hbox.t ~widgets:[num_elt; hex_elt; chr_elt] ()

    method hex_widget  = hex_elt
    method char_widget = chr_elt
    method num_widget  = num_elt

    method set_base (x:base) =
      if not (equal_base x _config.base)
      then (_config <- { _config with base = x };
            self#set_bytes _bytes)

    method set_width (x:int) =
      if not (x = _config.width)
      then (_config <- { _config with width = x };
            self#set_bytes _bytes)

    method set_grouping (x:int) =
      if not (x = _config.grouping)
      then (_config <- { _config with grouping = x };
            self#set_bytes _bytes)

    method set_line_numbers (x:bool) =
      if not (Equal.bool x _config.line_numbers)
      then (_config <- { _config with line_numbers = x };
            if x then num_elt#style##.display := Js.string ""
            else num_elt#style##.display := Js.string "none")

    method set_bytes (bytes:string) =
      _bytes <- bytes;
      let rec aux acc bytes = match List.take_drop _config.width bytes with
        | l, [] -> List.rev (l :: acc)
        | l, r  -> aux (l :: acc) r in
      let bytes = aux [] (String.to_list bytes) in
      let _, num, hex, chr =
        List.fold_left (fun (id, num, hex, chr) (x:char list) ->
            let num'           = to_line_number (id / _config.width) _config in
            let id, hex', chr' = make_data id _config x in
            id, num ^ num' ^ "\n", hex ^ hex',chr ^ chr') (0,"","","") bytes in
      hex_elt#set_inner_html hex;
      chr_elt#set_inner_html chr;
      num_elt#set_inner_html num;

    method select ?(flush=true) id =
      let eq = fun x -> id = self#_get_hex_id x in
      match List.find_opt eq self#_hex_items with
      | None     -> ()
      | Some elt ->
         (match List.find_opt (Equal.physical elt) _selected with
          | Some _ -> () (* already selected *)
          | None   ->
             if flush then List.iter self#_unselect _selected;
             self#_select elt)

    method select_range ?(flush=true) from till =
      if till < from || from < 0 || till < 0
      then raise_notrace (Invalid_argument "bad range")
      else
        let () = if flush then List.iter self#_unselect _selected in
        List.iter (self#select ~flush:false) (List.range from till)

    method interactive : bool =
      self#has_class Markup.interactive_class
    method set_interactive (x:bool) : unit =
      self#add_or_remove_class x Markup.interactive_class;
      match x, _listener with
      | true, None   ->
         _listener <- Some (self#listen_lwt Widget.Event.click self#_on_click)
      | false, Some l -> Lwt.cancel l; _listener <- None
      | _ -> ()


    (* Private methods *)

    method private _hex_items : Dom_html.element Js.t list =
      Dom.list_of_nodeList @@ self#hex_widget#root##.childNodes
      |> List.filter_map (fun x -> match Dom.nodeType x with
                                   | Element e -> Some e
                                   | _         -> None)
      |> List.map Js.Unsafe.coerce
      |> List.filter (fun x ->
             let class' = Js.string Markup.Hex._class in
             Js.to_bool @@ x##.classList##contains class')
      |> List.sort (fun e1 e2 -> compare
                                   (self#_get_hex_id e1)
                                   (self#_get_hex_id e2))

    method private _char_items : Dom_html.element Js.t list =
      Dom.list_of_nodeList @@ self#char_widget#root##.childNodes
      |> List.filter_map (fun x -> match Dom.nodeType x with
                                   | Element e -> Some e
                                   | _         -> None)
      |> List.map Js.Unsafe.coerce
      |> List.filter (fun x ->
             let class' = Js.string Markup.Char._class in
             Js.to_bool @@ x##.classList##contains class')
      |> List.sort (fun e1 e2 -> compare
                                   (self#_get_char_id e1)
                                   (self#_get_char_id e2))

    method private _get_char_id (x:Dom_html.element Js.t) =
      x##getAttribute (Js.string "data-char-id") |> Js.Opt.to_option
      |> Option.map Fun.(int_of_string % Js.to_string)
      |> Option.get_exn

    method private _get_hex_id (x:Dom_html.element Js.t) =
      x##getAttribute (Js.string "data-hex-id") |> Js.Opt.to_option
      |> Option.map Fun.(int_of_string % Js.to_string)
      |> Option.get_exn

    method private _get_hex_char (x:Dom_html.element Js.t) =
      let f_conv s = match config.base with
        | `Hex -> "0x" ^ s
        | `Bin -> "0b" ^ s
        | `Dec -> s in
      x##.textContent |> Js.Opt.to_option
      |> Option.map Fun.(Char.chr % int_of_string % f_conv % Js.to_string)
      |> Option.get_exn

    method private _add_hex_selected x =
      x##.classList##add (Js.string Markup.Hex.selected_class)

    method private _rm_hex_selected x =
      x##.classList##remove (Js.string Markup.Hex.selected_class)

    method private _add_char_selected x =
      x##.classList##add (Js.string Markup.Char.selected_class)

    method private _rm_char_selected x =
      x##.classList##remove (Js.string Markup.Char.selected_class)

    method private _unselect x =
      List.find_opt (fun c -> Int.equal (self#_get_char_id c)
                                (self#_get_hex_id x))
        self#_char_items
      |> Option.iter self#_rm_char_selected;
      _selected <- List.remove ~eq:Equal.physical ~x _selected;
      self#_rm_hex_selected x

    method private _select x =
      List.find_opt (fun c -> Int.equal (self#_get_char_id c)
                                (self#_get_hex_id x))
        self#_char_items
      |> Option.iter self#_add_char_selected;
      self#_add_hex_selected x;
      _selected <- x :: _selected

    method private _on_click = fun e _ ->
      let eq      = Equal.physical in
      let ctrl    = Js.to_bool e##.ctrlKey in
      let target  = Js.Opt.to_option e##.target in
      let class'  = Js.string Markup.Hex._class in
      let is_span =
        Option.map (fun e -> e##.classList##contains class' |> Js.to_bool)
          target
        |> Option.get_or ~default:false in
      let () = match target, is_span with
        | Some e, true ->
           (match List.find_opt (eq e) _selected with
            | Some x ->
               if not ctrl
               then List.iter self#_unselect (List.remove ~eq ~x _selected)
               else self#_unselect x
            | None ->
               if not ctrl then List.iter self#_unselect _selected;
               self#_select e)
        | _ -> () in
      Lwt.return_unit

    initializer
      self#set_bytes data;
      self#set_interactive interactive;
      num_elt#add_class Markup.line_numbers_block_class;
      hex_elt#add_class Markup.hex_block_class;
      chr_elt#add_class Markup.chars_block_class;
      self#add_class    Markup.base_class
  end
