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
           
type t = { dispatch : (int * (Cstruct.t list -> 'c cc as 'c) cc) list ref
         ; send     : int -> Cstruct.t -> unit Lwt.t
         ; usb      : Cyusb.t
         }

type header = { len    : int
              ; port   : int
              }

let prefix = 0x44BB

let to_header port parity length =
  let hdr = Cstruct.create sizeof_header in
  let ()  = set_header_prefix hdr prefix in
  let ()  = set_header_port hdr ((if parity then 0x10 else 0) lor (port land 0xF)) in
  let ()  = set_header_length hdr length in
  hdr

let serialize port buf =
  let buf_len = Cstruct.len buf in
  let parity  = (buf_len mod 2) <> 0 in
  let len     = (buf_len / 2) + (if parity then 1 else 0) in
  let buf'    = if parity then Cstruct.append buf (Cstruct.create 1) else buf in
  Cstruct.append (to_header port parity len) buf'


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
                              | e                      -> Logs.err (fun m -> m "(Usb_device) %s" (string_of_err e));
                                                          f acc (Cstruct.shift b 1)))
    else acc,b in
  let msgs,new_acc = f [] buf in
  (if Cstruct.len new_acc > 0 then Some new_acc else None), msgs

let recv usb =
  Lwt_preemptive.detach (fun () -> Cyusb.recv usb)

let send usb port =
  let send' port data =
    let msg = serialize port data in
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
    let new_acc, msgs = deserialize acc buf in
    dispatch := apply !dispatch msgs;
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
