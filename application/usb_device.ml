[@@@ocaml.warning "-32"]

[%%cstruct
 type header =
   { prefix : uint16_t
   ; port : uint8_t
   ; length : uint8_t
   } [@@big_endian]]

[@@@ocaml.warning "+32"]

module Int = struct
  type t = int
  let compare = compare
end

module Int_map = Map.Make(Int)

type t =
  { subscribers : ((Cstruct.t -> unit) * unit Lwt.t) Int_map.t ref
  ; send : int -> Cstruct.t -> unit Lwt.t
  ; usb : Cyusb.t
  }

let src = Logs.Src.create "USB parser" ~doc:"USB device module"
module Log = (val Logs.src_log src : Logs.LOG)

let prefix = 0x44BB
let max_length = 256

let ( >>= ) = Lwt.bind

let serialize port data =
  let data_length = Cstruct.len data in
  let parity = (data_length mod 2) <> 0 in
  let length = (data_length / 2) + (if parity then 1 else 0) in
  let header = Cstruct.create sizeof_header in
  set_header_prefix header prefix;
  set_header_port header ((if parity then 0x10 else 0) lor (port land 0xF));
  set_header_length header length;
  if parity
  then Cstruct.concat [header; data; Cstruct.create 1]
  else Cstruct.concat [header; data]

type error =
  | Bad_prefix of int
  | Bad_length of int
  | Insufficient_payload of Cstruct.t

let pp_error ppf = function
  | Bad_prefix x -> Fmt.fmt "incorrect prefix tag: %d" ppf x
  | Bad_length x -> Fmt.fmt "incorrect length: %d" ppf x
  | Insufficient_payload _ -> Fmt.string ppf "insufficient payload"

let check_prefix buf =
  let prefix' = get_header_prefix buf in
  if prefix = prefix' then Ok buf
  else Error (Bad_prefix prefix')

let check_length buf =
  let header, rest' = Cstruct.split buf sizeof_header in
  (* Length in 16-bit words. *)
  let length = get_header_length header in
  let flags_port = get_header_port header in
  let port = flags_port land 0xF in
  if length <= 0 || length > max_length
  then Error (Bad_length length)
  else
    let parity = (flags_port land 0x10) lsr 4 in
    let byte_length = length * 2 in
    try
      let body, rest = Cstruct.split rest' (byte_length - parity) in
      Ok ((port, body), snd @@ Cstruct.split rest parity)
    with _ -> Error (Insufficient_payload buf)

let get_msg buf =
  let ( >>= ) x f = match x with Error e -> Error e | Ok x -> f x in
  check_prefix buf
  >>= check_length

let deserialize acc buf =
  let buf = match acc with
    | None -> buf
    | Some acc -> Cstruct.append acc buf in
  let rec aux acc b =
    if Cstruct.len b >= sizeof_header
    then (match get_msg b with
          | Ok ((k, v), rest) ->
             let acc = Int_map.update k (function
                           | Some l -> Some (v :: l)
                           | None -> Some [v]) acc in
             aux acc rest
          | Error e ->
             match e with
             | Insufficient_payload b -> acc, b
             | _e ->
                Logs.warn (fun m -> m "%a" pp_error _e);
                aux acc (Cstruct.shift b 1))
    else acc, b in
  let msgs, rest = aux Int_map.empty buf in
  msgs, (if Cstruct.len rest > 0 then Some rest else None)

let recv usb =
  Lwt_preemptive.detach (fun () ->
      Cstruct.of_bigarray @@ Cyusb.recv usb)

let send usb port data =
  Lwt_preemptive.detach (fun () ->
      let msg = Cstruct.to_bigarray @@ serialize port data in
      Cyusb.send usb msg) ()

let apply (subscribers : ((Cstruct.t -> unit) * unit Lwt.t) Int_map.t)
    (msgs : Cstruct.t list Int_map.t) =
  Int_map.merge (fun id sub msgs ->
      match sub, msgs with
      | None, _ -> None
      | Some sub, None -> Some sub
      | Some sub, Some msgs ->
        let msg = Cstruct.concat @@ List.rev msgs in
        (fst sub) msg;
        Some sub)
    subscribers msgs

let create ?(sleep = 1.) () =
  let usb = Cyusb.create () in
  let subscribers = ref Int_map.empty in
  let recv = recv usb in
  let send = send usb in

  let rec loop acc () =
    Lwt_unix.sleep sleep
    >>= fun () -> recv ()
    >>= fun buf ->
    let msgs, rest = deserialize acc buf in
    subscribers := apply !subscribers msgs;
    loop rest ()
  in
  { usb; subscribers; send },
  (fun () -> loop None ())

let subscribe (t : t) id loop push =
  t.subscribers := Int_map.add id (push, loop ()) !(t.subscribers)

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
