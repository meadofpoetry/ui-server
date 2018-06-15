open Containers
open Lwt.Infix

[@@@ocaml.warning "-32"]

[%%cstruct
type header =
  { prefix : uint16_t
  ; port   : uint8_t
  ; length : uint8_t
  } [@@big_endian]]

[@@@ocaml.warning "+32"]

type 'a cc = 'a Meta_board.cc
           
type t = { dispatch : (int * (Cstruct.t list -> 'c cc as 'c) cc) list ref (* TODO why list? *)
         ; send     : int -> Cstruct.t -> unit Lwt.t
         ; usb      : Cyusb.t
         }

let prefix = 0x44BB
let c_44 = Char.chr 0x44
let c_BB = Char.chr 0xBB

let msg_parser =
  let open Angstrom in
  let prefix_parser = BE.int16 prefix in
  let skip_until_prefix =
    scan_state false (fun prev cur ->
        match cur with
        | c when Char.equal c c_44 -> Some true
        | c when Char.equal c c_BB -> Some prev
        | _ -> if prev then None else Some false)
  in
  let len_parser =
    any_uint8 >>= fun port' ->
    any_uint8 >>= fun length ->
    let parity    = port' land 0x10 > 0 in
    let port      = port' land 0xF in
    let len       = (length * 2) - (if parity then 1 else 0) in
    return (port,len)
  in
  let header_parser = prefix_parser *> len_parser
    (* skip_until_prefix *> len_parser*)
  in
  let msg_parser' = 
    header_parser >>= fun (port,len) ->
    take_bigstring len >>= fun msg ->
    return (port, Cstruct.of_bigarray msg)
  in
  many msg_parser'

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
     let res = Result.map List.rev @@ Buffered.state_to_result state in
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

(* OLD PARSER BLOCK *)

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

let check_prefix buf =
  let prefix' = get_header_prefix buf in
  if prefix <> prefix' then Error (Bad_prefix prefix') else Ok buf

let check_length buf =
  let hdr,buf'  = Cstruct.split buf sizeof_header in
  let length    = get_header_length hdr in
  if (length <= 0) || (length > 256)
  then Error (Bad_length length)
  else let port'     = get_header_port hdr in
       let parity    = port' land 0x10 > 0 in
       let port      = port' land 0xF in
       let len       = (length * 2) - (if parity then 1 else 0) in
       try
         let msg',rest = Cstruct.split buf' (length * 2) in
         let msg,_     = Cstruct.split msg' len in
         Ok ((port,msg),rest)
       with _ -> Error (Insufficient_payload buf)

let check_next_prefix ((_,rest) as x) =
  if Cstruct.len rest < sizeof_header then Ok x
  else (match check_prefix rest with
        | Ok _    -> Ok x
        | Error _ -> Error (No_prefix_after_msg))

let get_msg buf =
  try
    Result.(check_prefix buf
            >>= check_length
            >>= check_next_prefix)
  with e -> Error (Unknown_err (Printexc.to_string e))

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
                              | e                      -> f acc (Cstruct.shift b 1)))
    else acc,b in
  let msgs,new_acc = f [] buf in
  (if Cstruct.len new_acc > 0 then Some new_acc else None), msgs

(* END OF OLD PARSER BLOCK *)

let sma (n, s, q) x =
  let l = Queue.length q and s = s +. x in
  Queue.push x q;
  if l < n then
    (n, s, q), s /. float (l + 1)
  else (
    let s = s -. Queue.pop q in
    (n, s, q), s /. float l
  )

let create ?(sleep = 1.) () =
  let usb      = Cyusb.create () in
  let dispatch = ref [] in
  let recv     = recv usb in
  let send     = send usb in

  let rec loop oacc acc sma_o sma_n () =
    Lwt_unix.sleep sleep >>= fun () ->
    recv () >>= fun buf ->
    let t1 = Unix.gettimeofday () in
    let oacc, omsgs = deserialize oacc buf in
    let t2 = Unix.gettimeofday () in
    let new_acc, msgs = parse_msg ?prev:acc buf in
    let t3 = Unix.gettimeofday () in
    let o = t2 -. t1 in
    let n = t3 -. t2 in
    let max = Float.max o n in
    let op  = o /. max *. 100. in
    let np  = n /. max *. 100. in
    let sma_o,oav = sma sma_o op in
    let sma_n,nav = sma sma_n np in
    Logs.debug (fun m ->
        match Cstruct.len buf with
        | 0 -> ()
        | l -> m "len = %d;
                  old: val = %f,\t pct = %.2f%%,\t pct_av = %.2f%%;
                  new: val = %f,\t pct = %.2f%%,\t pct_av = %.2f%%"
                 l o op oav n np nav);
    begin match msgs with
    | Error e -> Logs.err (fun m -> m "(Usb_device) deserialize loop error: %s" e)
    | Ok msgs -> dispatch := apply !dispatch omsgs
    end;
    loop oacc new_acc sma_o sma_n ()
  in
  { usb; dispatch; send }, (fun () -> loop None None (100,0.,Queue.create ()) (100,0.,Queue.create ()) ())

let subscribe obj id step =
  obj.dispatch := (id, step) :: !(obj.dispatch)

let get_send obj id = obj.send id

(* TODO add proper finalize *)

let finalize obj =
  Cyusb.send obj.usb (Cstruct.create 10);
  Cyusb.finalize ()
