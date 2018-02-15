open Containers
include Cstruct

let get_ptr cs =
  Ctypes.(bigarray_start array1 cs.buffer)

exception Not_equal
  
let split_by_string on cs =
  let module BA = Bigarray.Array1 in
  
  let onsz  = String.length on in
  let bufsz = cs.len in
  if onsz >= bufsz
  then []
  else

    let rec lst_to_pairs = function
      | [] -> []
      | [_] -> failwith "odd number of elements"
      | x::y::xs -> (x,y)::(lst_to_pairs xs)
    in

    let rec find_subs i acc =
      if i < 0 then acc
      else try String.iteri
                 (fun j c ->
                   if not (Char.equal c (Cstruct.get_char cs (i + j)))
                   then raise_notrace Not_equal)
                 on;
               find_subs (i - onsz) (i::(i+onsz)::acc)
           with Not_equal -> find_subs (i - 1) acc
    in

    let points =
      ([0] @ (find_subs (bufsz - onsz) []) @ [bufsz])
      |> lst_to_pairs |> List.filter (fun (x,y) -> x <> y)
    in
    match points with
    | [] -> [cs]
    | _  ->
       List.map
         (fun (s,e) -> sub cs s (e - s))
         points

let split_size size cs =
  let module BA = Bigarray.Array1 in

  let bufsz = cs.len in
  let rec split point acc =
    if bufsz - point < size
    then
      let sz = (bufsz - point) in
      let r  = sub cs point sz in
      r::acc
    else
      let sz = size in
      let r  = sub cs point sz in
      split (point+size) (r::acc)
      
  in List.rev @@ split 0 []

let modify f cs =
  let module BA = Bigarray.Array1 in
  for i = cs.off to cs.len do
    BA.unsafe_set cs.buffer i @@ f @@ BA.unsafe_get cs.buffer i
  done

let modifyi f cs =
  let module BA = Bigarray.Array1 in
  for i = cs.off to cs.len do
    BA.unsafe_set cs.buffer i @@ f i @@ BA.unsafe_get cs.buffer i
  done

let hexdump_to_string ?(line_sz=16) b =
  let iter = Cstruct.iter (fun _ -> Some 1)
                          (fun buf -> Cstruct.get_uint8 buf 0)
                          b in
  let s = Cstruct.fold (fun acc el -> acc ^ (Printf.sprintf "%02x " el)) iter "" in
  let rec f = fun acc x -> if String.length x < (line_sz*3)
                           then List.rev (x :: acc)
                           else let s,res = String.take_drop (line_sz*3) x in
                                f ((s ^ "\n") :: acc) res in
  String.concat "" (f [] s)

module Cbitbuffer : sig

  type t =
    { buffer : Cstruct.t
    ; offset : int
    ; len    : int
    }

  val create   : Cstruct.t -> t
  val shift    : t -> int -> t
  val split    : t -> int -> t * t
  val get_int  : t -> int -> int -> int
  val get_bool : t -> int -> bool

end = struct

  type t =
    { buffer : Cstruct.t
    ; offset : int
    ; len    : int
    }

  let create buffer = { buffer; offset = 0; len = Cstruct.len buffer * 8 }

  let shift t offset =
    let buffer = Cstruct.shift t.buffer ((t.offset + offset) / 8) in
    let offset = (t.offset + offset) mod 8 in
    { buffer; offset; len = ((Cstruct.len buffer) * 8) - offset }

  let split t offset =
    let bytes      = (t.offset + offset) / 8 in
    let fst,_      = Cstruct.split t.buffer (bytes + 1) in
    let snd        = Cstruct.shift t.buffer bytes in
    let snd_offset = (t.offset + offset) mod 8 in
    { buffer = fst; offset = t.offset; len = offset },
    { buffer = snd; offset = snd_offset; len = ((Cstruct.len snd) * 8) - snd_offset }

  let get_mask bits = (1 lsl bits) - 1

  let get_sz_and_extract_fn = function
    | b when b < 9  -> 8, (fun x -> Cstruct.get_uint8 x 0)
    | b when b < 17 -> 16,(fun x -> Cstruct.BE.get_uint16 x 0)
    | b when b < 25 -> 24,(fun x -> ((Cstruct.get_uint8 x 0) lsl 16) lor Cstruct.BE.get_uint16 x 1)
    | _             -> 32,(fun x -> Int32.to_int @@ Cstruct.BE.get_uint32 x 0)

  let get_int t offset bits =
    if bits > 32 then raise (Invalid_argument "Cbitbuffer.get_int: bits size > 32");
    if (offset + bits) > t.len then raise (Invalid_argument "Cbitbuffer.get_int: not enough bits");

    let t         = shift t offset in
    let sz,fn     = get_sz_and_extract_fn bits in
    let available = sz - t.offset in
    let curr      = (fn t.buffer) land get_mask available in
    if bits < available
    then curr lsr (available - bits)
    else if bits = available
    then curr
    else (let new_buf  = shift t bits in
          let new_bits = bits - available in
          let sz,fn    = get_sz_and_extract_fn new_bits in
          let next     = (fn new_buf.buffer) lsr (sz - new_bits) in
          (curr lsl new_bits) lor next)

  let get_bool t offset = (get_int t offset 1) <> 0

end
