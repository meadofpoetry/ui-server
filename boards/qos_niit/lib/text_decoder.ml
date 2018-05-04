open Containers

[%%cenum
 type encoding =
   | Unknown
   | ISO8859_1
   | ISO8859_2
   | ISO8859_3
   | ISO8859_4
   | ISO8859_5
   | ISO8859_6
   | ISO8859_7
   | ISO8859_8
   | ISO8859_9
   | ISO8859_10
   | ISO8859_11
   | ISO8859_12
   | ISO8859_13
   | ISO8859_14
   | ISO8859_15
   | UCS_2BE
   | EUC_KR
   | GB2312
   | UTF_16_BE
   | ISO10646_UTF8
   | ISO6937
   | UTF8
[@@uint16_t]
]

let encoding_to_table_name = function
  | Unknown       -> ""
  | ISO8859_1     -> "iso-8859-1"
  | ISO8859_2     -> "iso-8859-2"
  | ISO8859_3     -> "iso-8859-3"
  | ISO8859_4     -> "iso-8859-4"
  | ISO8859_5     -> "iso-8859-5"
  | ISO8859_6     -> "iso-8859-6"
  | ISO8859_7     -> "iso-8859-7"
  | ISO8859_8     -> "iso-8859-8"
  | ISO8859_9     -> "iso-8859-9"
  | ISO8859_10    -> "iso-8859-10"
  | ISO8859_11    -> "iso-8859-11"
  | ISO8859_12    -> "iso-8859-12"
  | ISO8859_13    -> "iso-8859-13"
  | ISO8859_14    -> "iso-8859-14"
  | ISO8859_15    -> "iso-8859-15"
  | UCS_2BE       -> "UCS-2BE"
  | EUC_KR        -> "EUC-KR"
  | GB2312        -> "GB2312"
  | UTF_16_BE     -> "UTF-16BE"
  | ISO10646_UTF8 -> "ISO-10646/UTF8"
  | ISO6937       -> "iso6937"
  | UTF8          -> "utf-8"

type encoding_params =
  { encoding   : encoding
  ; start_text : int
  ; multibyte  : bool
  }

module Iconv = struct

  open Ctypes
  open Foreign

  type handl = unit ptr
  let handl : handl typ = ptr void

  let char_ptr : char ptr typ = ptr char

  let iconv_open  = foreign "iconv_open"  (string @-> string @-> returning handl)
  let iconv_close = foreign "iconv_close" (handl @-> returning int)
  let iconv       = foreign "iconv"       (handl
                                           @-> (ptr (ptr char)) @-> ptr size_t
                                           @-> (ptr (ptr char)) @-> ptr size_t
                                           @-> returning size_t)

  let convert ~(src:string) ~(dst:string)
              (text:Cbuffer.t) =
    let coef = 6 in
    let rec aux n size =
      let buf   = Cbuffer.create size in
      let ()    = Cbuffer.memset buf 0 in
      let handl = iconv_open dst src in
      let in_bytes_left  = allocate size_t @@ Unsigned.Size_t.of_int (Cbuffer.len text) in
      let out_bytes_left = allocate size_t @@ Unsigned.Size_t.of_int (Cbuffer.len buf) in
      let sz             = iconv handl
                                 (allocate char_ptr (Cbuffer.get_ptr text))
                                 in_bytes_left
                                 (allocate char_ptr (Cbuffer.get_ptr buf))
                                 out_bytes_left
      in
      let _        = iconv_close handl in
      let str_size = Cbuffer.len buf - Unsigned.Size_t.to_int !@out_bytes_left in
      match Unsigned.Size_t.to_int sz with
      | -1 -> (match !@(foreign_value "errno" int) with
               | 7 -> if n > 2 then Error 7 else aux (succ n) (size * coef)
               | x -> Error x)
      | _  -> Ok (Cbuffer.to_string @@ fst @@ Cbuffer.split buf str_size)
    in
    aux 0 (Cbuffer.len text * coef)

end

let get_encoding (text:Cbuffer.t) =
  let p         = { encoding = Unknown; start_text = 0; multibyte = false } in
  try
    let firstbyte = Cbuffer.get_uint8 text 0 in
    match firstbyte with
    | 0x00 -> p
    | x when x > 0x00 && x <= 0x0B -> let encoding = int_to_encoding (x + 4) |> Option.get_exn in
                                      { p with encoding; start_text = 1 }
    | x when x > 0x0B && x <= 0x0F -> p
    | 0x10 -> (match Cbuffer.BE.get_uint16 text 1 with
               | x when x < 17 -> { p with encoding = int_to_encoding x |> Option.get_exn; start_text = 3 }
               | _             -> { p with start_text = 3 })
    | 0x11 -> { encoding = UCS_2BE; start_text = 1; multibyte = true }
    | 0x12 -> { encoding = EUC_KR;  start_text = 1; multibyte = true }
    | 0x13 -> { p with encoding = GB2312; start_text = 1 }
    | 0x14 -> { encoding = UTF_16_BE; start_text = 1; multibyte = true }
    | 0x15 -> { p with encoding = ISO10646_UTF8; start_text = 1 }
    | x when x > 0x15 && x <= 0x1F -> p
    | _    -> { p with encoding = ISO6937 }
  with _ -> p

let fold_multibyte (text:Cbuffer.t) =
  let iter = Cbuffer.iter (fun b -> if Cbuffer.len b >= 2 && (not @@ Char.equal (Cbuffer.get_char b 0) '\000')
                                    then Some 2 else None)
                          (fun b -> b)
                          text in
  Cbuffer.fold (fun acc u16 ->
      let code = Cbuffer.BE.get_uint16 u16 0 in
      match code with
      | 0xE086 | 0xE087 -> acc (* 0xE086 - emphasis on, 0xE087 - emphasis off, skip these symbols *)
      | 0xE08A          -> let n  = Cbuffer.create 2 in
                           let () = Cbuffer.set_uint8 n 0 0x00 in
                           let () = Cbuffer.set_uint8 n 1 0x0A in
                           Cbuffer.append acc n
      | _               -> Cbuffer.append acc u16) iter (Cbuffer.create 0)

let fold_singlebyte (text:Cbuffer.t) =
  let iter = Cbuffer.iter (fun b -> if Cbuffer.len b >= 1 && (not @@ Char.equal (Cbuffer.get_char b 0) '\000')
                                    then Some 1 else None)
                          (fun b -> b)
                          text
  in
  Cbuffer.fold (fun acc u8 ->
      let code = Cbuffer.get_uint8 u8 0 in
      match code with
      | 0x86 | 0x87 -> acc (* 0x86 - emphasis on, 0x87 - emphasis off, skip these symbols *)
      | 0x8A        -> let n  = Cbuffer.create 1 in
                       let () = Cbuffer.set_uint8 n 0 0x0A in
                       Cbuffer.append acc n
      | _           -> Cbuffer.append acc u8) iter (Cbuffer.create 0)

(* Converts text to utf-8. Text may include pango markup (<b> and </b>) *)
let convert_to_utf8 (text:Cbuffer.t) (encoding:encoding_params) =
  let text = Cbuffer.shift text encoding.start_text
             |> (fun x -> if encoding.multibyte
                          then fold_multibyte x
                          else fold_singlebyte x)
  in
  Iconv.convert ~src:(encoding_to_table_name encoding.encoding)
                ~dst:(encoding_to_table_name UTF8)
                text

let get_encoding_and_convert (text:Cbuffer.t) =
  let to_default = fun () -> Cbuffer.to_string text |> String.filter (Char.equal '\000') in
  let enc = get_encoding text in
  match enc.encoding with
  | Unknown -> to_default ()
  | _       -> (match convert_to_utf8 text enc with
                | Ok s    -> s
                | Error _ ->
                   (match encoding_to_int enc.encoding with
                    | x when x >= (encoding_to_int ISO8859_2) && x <= (encoding_to_int ISO8859_15) ->
                       (* Sometimes using the standard 8859-1 set fixes issues *)
                       (match convert_to_utf8 text { enc with encoding = ISO8859_1 } with
                        | Ok s -> s
                        | Error _ -> to_default ())
                    | x when x = encoding_to_int ISO6937 ->
                       (* The first part of ISO 6937 is identical to ISO 8859-9, but
                        * they differ in the second part. Some channels don't
                        * provide the first byte that indicates ISO 8859-9 encoding.
                        * If decoding from ISO 6937 failed, we try ISO 8859-9 here.
                        *)
                       (match convert_to_utf8 text { enc with encoding = ISO8859_9 } with
                        | Ok s -> s
                        | Error _ -> to_default ())
                    | _ -> to_default ()))

