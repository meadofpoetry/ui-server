open Components_tyxml

module Const = struct
  let line_number_len = 4
end

module CSS = struct
  let root = "mdc-hexdump"

  let block = root ^ "-block"

  let item = root ^ "-item"

  let no_line_numbers = BEM.add_modifier root "no-line-numbers"

  let non_interactive = BEM.add_modifier root "non-interactive"

  let block_hex = BEM.add_modifier block "hex"

  let block_chars = BEM.add_modifier block "chars"

  let block_line_numbers = BEM.add_modifier block "line-numbers"

  let item_selected = BEM.add_modifier item "selected"
end

type block_type = Num | Hex | Chr

type base = Hex | Dec | Bin

let base_to_string = function
  | Hex -> "hex"
  | Dec -> "dec"
  | Bin -> "bin"

let base_of_string = function
  | "hex" -> Hex
  | "dec" -> Dec
  | "bin" -> Bin
  | s -> failwith @@ Printf.sprintf "base_of_string: base value (%s)" s

let equal_base (a : base) (b : base) : bool =
  match a, b with
  | Hex, Hex | Dec, Dec | Bin, Bin -> true
  | _, _ -> false

let int_to_string_binary ?(prefix = false) (n : int) : string =
  let buf = Buffer.create 16 in
  let out = Buffer.add_char buf in
  let n = if n < 0 then (out '-'; -n) else n in
  if prefix then (out '0'; out 'b');
  let rec loop started bit n =
    if bit = 0 then (
      if not started then out '0')
    else (
      let b = n land bit in
      if b = 0 then (
        if started then out '0';
        loop started (bit lsr 1) n)
      else (
        out '1';
        loop true (bit lsr 1) n)) in
  let most_significant_bit = (-1) lxor ((-1) lsr 1) in
  loop false most_significant_bit n;
  Buffer.contents buf

let get_padding = function
  | Hex -> 2
  | Dec -> 3
  | Bin -> 8

let to_base_string = function
  | Hex -> Printf.sprintf "%x"
  | Dec -> Printf.sprintf "%d"
  | Bin -> int_to_string_binary ~prefix:false

let map_char_printable ?(_or = '.') (c : char) : char =
  match Char.code c with
  | x when x > 31 && x < 128 -> c
  | _ -> _or

let pad (need : int) (c : char) (s : string) : string =
  let len = String.length s in
  match need - len with
  | x when x > 0 -> (String.make x c) ^ s
  | _ -> s

let take n l =
  let rec direct i n l = match l with
    | [] -> []
    | _ when i = 0 -> safe n [] l
    | x :: l' ->
       if n > 0
       then x :: direct (i - 1) (n - 1) l'
       else []
  and safe n acc l = match l with
    | [] -> List.rev acc
    | _ when n = 0 -> List.rev acc
    | x :: l' -> safe (n - 1) (x :: acc) l'
  in
  direct 1000 n l

let rec drop n l = match l with
  | [] -> []
  | _ when n = 0 -> l
  | _ :: l' -> drop (n - 1) l'

let take_drop n l = take n l, drop n l

let string_to_list s =
  let rec aux s acc i len =
    if len = 0 then List.rev acc
    else aux s (s.[i] :: acc) (i + 1) (len - 1) in
  aux s [] 0 (String.length s)

let line_number_to_string (n : int) : string =
  let s = string_of_int n in
  pad Const.line_number_len '0' s

let should_insert_space ~(grouping : int) (cnt : int) =
  if grouping = 0 then false else ((succ cnt) mod grouping) = 0

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create_line_number ?(classes = []) ?(attrs = []) n () : 'a elt =
    span ~a:([a_class classes] @ attrs)
      [txt @@ line_number_to_string n]

  let create_char_empty ?(classes = []) ?(attrs = []) () : 'a elt =
    let classes = CSS.item :: classes in
    span ~a:([ a_class classes
             ; a_user_data "empty" "true"
             ] @ attrs)
      [txt (String.make 2 ' ')]

  let create_char ?(classes = []) ?(attrs = []) ?(selected = false)
        ~(id : int) (chr : char) () : 'a elt =
    let classes =
      classes
      |> cons_if selected CSS.item_selected
      |> List.cons CSS.item in
    let chr = map_char_printable chr in
    span ~a:([ a_class classes
             ; a_user_data "id" (string_of_int id)
             ] @ attrs)
      [txt (String.make 1 chr)]

  let create_hex_empty ?(classes = []) ?(attrs = []) ?(grouping = 1)
        ~(base : base) ~id () : 'a elt =
    let classes = CSS.item :: classes in
    let text = String.make (get_padding base) '.' in
    span ~a:([ a_class classes
             ; a_user_data "empty" "true"
             ] @ attrs)
      [txt (if should_insert_space ~grouping id
            then text ^ " " else text)]

  let create_hex ?(classes = []) ?(attrs = []) ?(grouping = 1) ?(selected = false)
        ~(base : base) ~(id : int) (hex : int) () : 'a elt =
    let classes =
      classes
      |> cons_if selected CSS.item_selected
      |> List.cons CSS.item in
    let text = pad (get_padding base) '0' @@ to_base_string base hex in
    span ~a:([ a_class classes
             ; a_user_data "id" (string_of_int id)
             ] @ attrs)
      [txt (if should_insert_space ~grouping id
            then text ^ " " else text)]

  let append_empty base ~width ~grouping i acc =
    let rec aux i acc = function
      | 0 -> acc
      | rest ->
         let hex, chr = acc in
         (* Insert space if this item is not last in a row *)
         let hex = create_hex_empty ~base ~grouping ~id:i () :: hex in
         let chr = create_char_empty () :: chr in
         aux (succ i) (hex, chr) (pred rest) in
    aux i acc (width - i)

  let create_row ?(from_id = 0) ~width ~grouping
        (base : base) (data : char list) =
    let rec aux id acc = function
      | [] -> append_empty base ~grouping ~width (id - from_id) acc
      | (hd : char) :: tl ->
         let hex, chr = acc in
         let code = Char.code hd in
         let hex = (create_hex ~base ~grouping ~id code ()) :: hex in
         let chr = (create_char ~id hd ()) :: chr in
         aux (succ id) (hex, chr) tl in
    aux from_id ([], []) data

  let create_rows ~width ~grouping ~base (data : string) =
    let rec aux acc bytes = match take_drop width bytes with
      | l, [] -> List.rev (l :: acc)
      | l, r -> aux (l :: acc) r in
    let bytes = aux [] (string_to_list data) in
    let _, num, hex, chr =
      List.fold_left (fun (id, num, hex, chr) (x : char list) ->
          let num' = create_line_number (id / width) () in
          let hex', chr' = create_row ~from_id:id ~width ~grouping base x in
          id + List.length x,
          br () :: num' :: num,
          br () :: hex' @ hex,
          br () :: chr' @ chr)
        (0, [], [], []) bytes in
    List.rev num,
    List.rev hex,
    List.rev chr

  let create_block ?(classes = []) ?(attrs = []) ~typ cells () : 'a elt =
    let typ_class = match typ with
      | Num -> CSS.block_line_numbers
      | Hex -> CSS.block_hex
      | Chr -> CSS.block_chars in
    let classes = CSS.block :: typ_class :: classes in
    div ~a:([a_class classes] @ attrs) cells

  let create ?(classes = []) ?(attrs = []) ?(no_line_numbers = false)
        ?(non_interactive = false) ~width ~grouping ~base ~blocks
        () : 'a elt =
    let classes =
      classes
      |> cons_if no_line_numbers CSS.no_line_numbers
      |> cons_if non_interactive CSS.non_interactive
      |> List.cons CSS.root in
    div ~a:([ a_class classes
            ; a_user_data "width" (string_of_int width)
            ; a_user_data "grouping" (string_of_int grouping)
            ; a_user_data "base" (base_to_string base)
            ] @ attrs) blocks

  let of_bytes ?classes ?attrs ?(width = 16) ?(grouping = 1)
        ?no_line_numbers ?non_interactive
        ?(base = Hex) (bytes : string) : 'a elt =
    let num, hex, chr = create_rows ~width ~grouping ~base bytes in
    create ?classes ?attrs ~width ~grouping ?no_line_numbers ?non_interactive
      ~base ~blocks:[ create_block ~typ:Num num ()
                    ; create_block ~typ:Hex hex ()
                    ; create_block ~typ:Chr chr () ]
      ()
end
