open Containers
open Lwt.Infix
(*
[@@@ocaml.warning "-32"]

[%%cstruct
type header =
  { prefix : uint16_t
  ; port   : uint8_t
  ; length : uint8_t
  } [@@big_endian]]

[@@@ocaml.warning "+32"]
 *)

type 'a cc = 'a Meta_board.cc
           
type t = { dispatch : (int * (Cstruct.t list -> 'c cc as 'c) cc) list ref (* TODO why list? *)
         ; send     : int -> Cstruct.t -> unit Lwt.t
         ; usb      : Cyusb.t
         }

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

  (*
type err = Bad_prefix           of int
         | Bad_length           of int
         | Insufficient_payload of Cstruct.t
         | No_prefix_after_msg
         | Unknown_err          of string

let string_of_err = function
  | Bad_prefix x           -> "incorrect prefix tag: "    ^ (string_of_int x)
  | Bad_length x           -> "incorrect length: "       ^ (string_of_int x)
  | Insufficient_payload _ -> "insufficient payload"
  | No_prefix_after_msg    -> "no prefix after message"
  | Unknown_err s          -> s
   *)
(*
let deserialize acc buf =
  let buf = (match acc with
             | Some x -> Cstruct.append x buf
             | None   -> buf) in
  let rec f acc b =
    if Cstruct.len b >= sizeof_header
    then (match get_msg b with
          | Ok (msg,rest) -> f (msg :: acc) rest
          | Error e       -> (match e with
                              | Insufficient_payload b -> acc, b
                              | e                      -> Logs.err (fun m -> m "(Usb_device) %s" (string_of_err e));
                                                          f acc (Cstruct.shift b 1)))
    else acc,b in
  let msgs,new_acc = f [] buf in
  (if Cstruct.len new_acc > 0 then Some new_acc else None), msgs
 *)
  
let recv usb =
  Lwt_preemptive.detach (fun () -> Cyusb.recv usb)

let send usb port =
  let send' port data =
    let msg = serialize_msg (port, data) in
    Cyusb.send usb msg
  in
  Lwt_preemptive.detach (send' port)

let apply disp msg_list =
  let apply' (id, step) =
    (* TODO opt *)
    let msgs = List.filter_map
                 (fun (i,msg) -> if Int.equal i id then Some msg else None)
                 msg_list in
    (id, Meta_board.apply step msgs)
  in
  List.map apply' disp

let create ?(sleep = 1.) () =
  let usb      = Cyusb.create () in
  let dispatch = ref [] in
  let recv     = recv usb in
  let send     = send usb in

  let rec loop acc () =
    Lwt_unix.sleep sleep >>= fun () ->
    recv () >>= fun buf ->
    let new_acc, msgs = parse_msg ?prev:acc buf in
    begin match msgs with
    | Error e -> Logs.err (fun m -> m "(Usb_device) deserialize loop error: %s" e)
    | Ok msgs -> dispatch := apply !dispatch msgs
    end;
    loop new_acc ()
  in
  { usb; dispatch; send }, (fun () -> loop None ())

let subscribe obj id step =
  obj.dispatch := (id, step) :: !(obj.dispatch)

let get_send obj id = obj.send id

(* TODO add proper finalize *)
                    
let finalize obj =
  Cyusb.send obj.usb (Cstruct.create 10);
  Cyusb.finalize ()
