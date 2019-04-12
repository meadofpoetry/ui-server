open Lwt.Infix

[@@@ocaml.warning "-32"]

[%%cstruct
 type header =
   { prefix : uint16_t
   ; port : uint8_t
   ; length : uint8_t
   } [@@big_endian]]

[@@@ocaml.warning "+32"]

type 'a cc = 'a Boards.Board.cc

type t =
  { dispatch : (int * (Cstruct.t option -> 'c cc Lwt.t as 'c) cc) list ref
  ; send : int -> Cstruct.t -> unit Lwt.t
  ; usb : Cyusb.t
  }

type header =
  { len : int
  ; port : int
  }

let src = Logs.Src.create "USB parser"

let prefix = 0x44BB

let to_header port parity length =
  let hdr = Cstruct.create sizeof_header in
  set_header_prefix hdr prefix;
  set_header_port hdr ((if parity then 0x10 else 0) lor (port land 0xF));
  set_header_length hdr length;
  hdr

let serialize port buf =
  let buf_len = Cstruct.len buf in
  let parity = (buf_len mod 2) <> 0 in
  let len = (buf_len / 2) + (if parity then 1 else 0) in
  let buf' = if parity then Cstruct.append buf (Cstruct.create 1) else buf in
  Cstruct.append (to_header port parity len) buf'

type error =
  | Bad_prefix of int
  | Bad_length of int
  | Insufficient_payload of Cstruct.t
  | No_prefix_after_msg
  | Unknown_err of string

let pp_error ppf = function
  | Bad_prefix x -> Fmt.fmt "incorrect prefix tag: %d" ppf x
  | Bad_length x -> Fmt.fmt "incorrect length: %d" ppf x
  | Insufficient_payload _ -> Fmt.string ppf "insufficient payload"
  | No_prefix_after_msg -> Fmt.string ppf "no prefix after message"
  | Unknown_err s -> Fmt.fmt "unknown error: %s" ppf s

let check_prefix buf =
  let prefix' = get_header_prefix buf in
  if prefix <> prefix' then Error (Bad_prefix prefix') else Ok buf

let check_length buf =
  let hdr, buf' = Cstruct.split buf sizeof_header in
  let length = get_header_length hdr in
  if length <= 0 || length > 256
  then Error (Bad_length length)
  else let port' = get_header_port hdr in
       let parity = port' land 0x10 > 0 in
       let port = port' land 0xF in
       let len = (length * 2) - (if parity then 1 else 0) in
       try
         let msg', rest = Cstruct.split buf' (length * 2) in
         let msg, _ = Cstruct.split msg' len in
         Ok ((port, msg), rest)
       with _ -> Error (Insufficient_payload buf)

let check_next_prefix ((_, rest) as x) =
  if Cstruct.len rest < sizeof_header then Ok x
  else match check_prefix rest with
       | Ok _ -> Ok x
       | Error _ -> Error (No_prefix_after_msg)

let get_msg buf =
  let ( >>= ) x f = match x with Error e -> Error e | Ok x -> f x in
  try check_prefix buf
      >>= check_length
      >>= check_next_prefix
  with e -> Error (Unknown_err (Printexc.to_string e))

let assoc_set ~eq k (v : Cstruct.t) l =
  let rec aux acc = function
    | [] -> (k, v) :: l
    | ((k', v') as kv) :: tl ->
       if eq k k'
       then ((k, Cstruct.append v' v) :: tl) @ acc
       else aux (kv :: acc) tl in
  aux [] l

let deserialize acc buf =
  let buf = match acc with
    | Some x -> Cstruct.append x buf
    | None -> buf in
  let rec f acc b =
    if Cstruct.len b >= sizeof_header
    then (match get_msg b with
          | Ok ((k, v), rest) -> f (assoc_set ~eq:(=) k v acc) rest
          | Error e ->
             match e with
             | Insufficient_payload b -> acc, b
             | _e ->
                Logs.warn ~src (fun m -> m "parser - %a" pp_error _e);
                f acc (Cstruct.shift b 1))
    else acc, b in
  let msgs, new_acc = f [] buf in
  (if Cstruct.len new_acc > 0 then Some new_acc else None),
  msgs

let recv usb =
  Lwt_preemptive.detach (fun () ->
      Cstruct.of_bigarray @@ Cyusb.recv usb)

let send usb port data =
  Lwt_preemptive.detach (fun () ->
      let msg = Cstruct.to_bigarray @@ serialize port data in
      Cyusb.send usb msg) ()

let apply disp (msgs : (int * Cstruct.t) list) =
  let apply' (id, step) =
    let msg = List.assoc_opt id msgs in
    Boards.Board.apply step msg
    >>= fun next_step ->
    Lwt.return (id, next_step)
  in
  Lwt_list.map_p apply' disp

let create ?(sleep = 1.) () =
  let usb = Cyusb.create () in
  let dispatch = ref [] in
  let recv = recv usb in
  let send = send usb in

  let rec loop acc () =
    Lwt_unix.sleep sleep
    >>= fun () -> recv ()
    >>= fun buf ->
    let new_acc, msgs = deserialize acc buf in
    apply !dispatch msgs
    >>= fun new_dispatch ->
    dispatch := new_dispatch;
    loop new_acc ()
  in
  { usb; dispatch; send },
  (fun () ->
    apply !dispatch []
    >>= fun new_dispatch ->
    dispatch := new_dispatch;
    loop None ())

let subscribe obj id step =
  obj.dispatch := (id, step) :: !(obj.dispatch)

let get_send obj id = obj.send id

(* TODO add proper finalize *)

let finalize obj =
  Cyusb.send obj.usb (Cstruct.to_bigarray @@ Cstruct.create 10);
  Cyusb.close obj.usb

(* (TODO) Angstrom parser and Faraday serialyzer *)

(*
let prefix = 0x44BB

let msg_parser =
  let open Angstrom in
  let prefix_parser =
    BE.uint16 >>= fun v ->
    if v = prefix then return () else fail "Not a prefix"
  in
  let len_parser =
    any_uint8 >>= fun port' ->
    any_uint8 >>= fun length ->
    let parity    = port' land 0x10 > 0 in
    let port      = port' land 0xF in
    let len       = (length * 2) - (if parity then 1 else 0) in
    return (port,len)
  in
  let header_parser =
    prefix_parser *> len_parser
  in
  let msg_parser' = 
    header_parser >>= fun (port,len) ->
    take_bigstring len >>= fun msg ->
    return (port, Cstruct.of_bigarray msg)
  in
  many @@ fix (fun p ->
              msg_parser' <|> (any_char *> p))

let parse_msg ?(prev : Angstrom.Buffered.unconsumed option) s =
  let open Angstrom in
  let state  = Buffered.parse msg_parser in
  let state' = match prev with
    | None -> state
    | Some x -> Buffered.feed state (`Bigstring (Bigarray.Array1.sub x.buf x.off x.len))
  in
  Buffered.feed state' (`Bigstring (Cstruct.to_bigarray s))
  |> (fun state -> Buffered.feed state (`Eof))
  |> fun state ->
    let res = Buffered.state_to_result state in
    let unc = Option.(>>=) (Buffered.state_to_unconsumed state) (fun v -> if v.len = 0 then None else Some v) in
    (unc, res)

let msg_serializer t (port, msg) =
  let open Faraday in
  let buf_len = Cstruct.len msg in
  let parity  = (buf_len mod 2) <> 0 in
  let len     = (buf_len / 2) + (if parity then 1 else 0) in
  let buf     = if parity then Cstruct.append msg (Cstruct.create 1) else msg in
  BE.write_uint16 t prefix;
  write_uint8 t ((if parity then 0x10 else 0) lor (port land 0xF));
  write_uint8 t len;
  write_bigstring t ~off:(buf.off) ~len:(buf.len) (buf.buffer)
  
let serialize_msg (port, msg) =
  let open Faraday in
  let buf = Faraday.create 0x4096 in
  msg_serializer buf (port, msg);
  Cstruct.of_bigarray @@ serialize_to_bigstring buf
 *)
