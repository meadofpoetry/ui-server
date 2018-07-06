open Containers
open Tyxml_js

let line_number_len = 8

module Markup = Components_markup.Hexdump.Make(Xml)(Svg)(Html)

type base = [`Hex | `Dec | `Bin ]

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

let pad (p:padding) s = p.prefix ^ " " ^ s ^ " " ^ p.suffix

let to_style ?(line_number={prefix="";suffix=":"})
      ?(hex ={prefix="" ;suffix=""})
      ?(char={prefix="|";suffix="|"})
      ?(empty_hex='.')
      ?(empty_char=' ')
      ?(non_printable_char='.')
      () =
  { line_number; hex; char; empty_hex; empty_char; non_printable_char }

let to_config ?(style=to_style ()) ?(base=`Hex) ?(width=16) ?(grouping=1) ?(line_numbers=true) () =
  { style; base; width; grouping; line_numbers }

let to_line_number i (config:config) =
  let cur_line_count = i * config.width in
  let s = String.pad ~side:`Left ~c:'0' line_number_len (string_of_int cur_line_count) in
  let s = pad config.style.line_number s in
  Tyxml.Html.(span ~a:[a_class [Markup.Line_number._class]] [pcdata s])
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let to_hex_span ?value (config:config) =
  let empty = config.style.empty_hex in
  let udata_id,udata_val,s = match value with
    | None -> "hex-empty","true",String.pad ~side:`Left ~c:empty
                                   (get_padding config.base)
                                   (String.of_char empty)
    | Some (id,v) ->
       let s = (get_converter config.base) v
               |> String.pad ~side:`Left ~c:'0' (get_padding config.base)
       in "hex-id",string_of_int id,s
  in
  Tyxml.Html.(span ~a:[ a_class [Markup.Hex._class]
                      ; a_user_data udata_id udata_val ]
                [pcdata s])
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let to_char_span ?value (config:config) =
  let empty = config.style.empty_char in
  let udata_id,udata_val,s = match value with
    | Some (id,v) -> "char-id",string_of_int id,String.of_char v
    | None        -> "char-empty","true",String.pad ~side:`Left ~c:empty 1 (String.of_char empty)
  in
  Tyxml.Html.(span ~a:[ a_class [Markup.Char._class]
                      ; a_user_data udata_id udata_val]
                [pcdata s])
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let make_data id (config:config) (data:char list) =
  let grouping  = pred config.grouping in
  let conv_hex ?value () = to_hex_span ?value config in
  let conv_chr ?value () = to_char_span ?value config in
  let rec aux_empty cnt hex_acc chr_acc = function
    | x when x > 0 -> let cnt,hex_acc = if cnt = grouping
                                        then 0,(conv_hex () ^ " ") :: hex_acc
                                        else (succ cnt),conv_hex () :: hex_acc
                      in aux_empty cnt hex_acc (conv_chr () :: chr_acc) (pred x)
    | _ -> hex_acc,chr_acc
  in
  let rec aux id cnt hex_acc chr_acc = function
    | [] -> let hex_acc,chr_acc = aux_empty cnt hex_acc chr_acc
                                    (config.width - List.length hex_acc)
            in id,hex_acc,chr_acc
    | hd::tl ->
       let cnt,hex_acc =
         let code = Char.code hd in
         if cnt = grouping
         then 0,((conv_hex ~value:(id,code) () ^ " ") :: hex_acc)
         else (succ cnt),((conv_hex ~value:(id,code) ()) :: hex_acc)
       in
       let chr_acc = (conv_chr ~value:(id,hd) ()) :: chr_acc in
       aux (succ id) cnt hex_acc chr_acc tl
  in
  let id,hex,chars = aux id 0 [] [] data in
  id,
  (String.concat "" (List.rev hex)   |> pad config.style.hex) ^ "\n",
  (String.concat "" (List.rev chars) |> pad config.style.char) ^ "\n"

class t ~(config:config) (data:string) () =
  let rec aux acc bytes = match List.take_drop config.width bytes with
    | l,[] -> List.rev (l :: acc)
    | l,r  -> aux (l :: acc) r
  in
  let bytes     = aux [] (String.to_list data) in
  let _,hex,chr =
    List.fold_left (fun (id,hex,chr) (x:char list) ->
        let id,hex',chr' = make_data id config x in
        id, hex ^ hex',chr ^ chr') (0,"","") bytes
  in
  let hex_elt = Markup.create [] () |> Tyxml_js.To_dom.of_element |> Widget.create in
  let chr_elt = Markup.create [] () |> Tyxml_js.To_dom.of_element |> Widget.create in
  object(self)
    inherit Hbox.t ~widgets:[hex_elt; chr_elt] ()
    initializer
      hex_elt#set_inner_html hex;
      chr_elt#set_inner_html chr
  end
