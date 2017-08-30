open Containers
open Lwt.Infix
   
type t = { dispatch : (int, Cbuffer.t -> unit) Hashtbl.t
         ; send     : int -> Cbuffer.t -> unit Lwt.t
         ; usb      : Cyusb.t
         }

type header = { len    : int
              ; port   : int
              }
            
let divider = String.of_array [| (char_of_int 0x44); (char_of_int 0xBB) |]

let char_to_b c =
  let c = int_of_char c in
  let rec loop i s =
    if i = 8
    then s
    else loop (succ i) ((if ((c lsr i) land 1) = 0 then "0" else "1") ^ s)
  in "0b" ^ (loop 0 "")

let set_h parity port =
  let p   = if parity then (1 lsl 4) else 0 in
  char_of_int (p lor port)

let get_h h =
  let i = int_of_char h in
  let parity = (i land (1 lsl 4)) <> 0 in
  let port   = (i land 0xf) in
  parity, port
              
let of_header buf =
  let open Cbuffer in
  let h = to_string buf in
  let len = int_of_char h.[1] in
  let c   = h.[0] in
  let parity, port = get_h c in
  let len  = (len * 2) - (if parity then 1 else 0) in
  { port; len }

let to_header port parity length =
  let open Cbuffer in
  let cb = create 2 in
  let c      = set_h parity port in
  Cbuffer.set_char cb 0 c;
  Cbuffer.set_char cb 1 length;
  cb

let deserialize msg =
  let headersz = 2 in
  let headerb  = Cbuffer.sub msg 0 headersz in
  let header   = of_header headerb in
  let bodysz   = let sz = (msg.len - headersz) in
                 if sz < header.len then sz else header.len
  in
  let body     = Cbuffer.sub msg headersz bodysz in
  if  (body.len < header.len)
  then `Partial (header.port , header.len, body)
  else `Full    (header.port , body)

let serialize port buf =
  let open Cbuffer in
  let parity = not ((buf.len mod 2) = 0) in
  let len = (buf.len / 2) + (if parity then 1 else 0) |> char_of_int in
  let buf'   = if parity then Cbuffer.append buf (Cstruct.create 1) else buf in
  append (to_header port parity len) buf'
  
let parse ~mstart ~mend buf_list =
  let open Option.Infix in
  let msgs = List.map deserialize buf_list in

  let fixed = mend   >>= fun mend ->
              mstart >>= fun mstart ->
              let (port, len, body) = mstart in
              if Cbuffer.(mend.len + body.len) = len
              then Some (port, Cbuffer.append body mend)
              else None
  in
  let rec sieve acc = function
    | [] -> None, []
    | [`Partial x]  -> (Some x), acc
    | [`Full x]     -> None, (x::acc)
    | (`Full x)::xs -> sieve (x::acc) xs
    | (`Partial _)::xs -> sieve acc xs
  in
  let new_mend, msgs = sieve [] msgs in
  if Option.is_some fixed
  then new_mend, ((Option.get_exn fixed)::msgs)
  else new_mend, msgs

exception Not_equal
  
let msg_head divider msg =
  
  let rec find_head point =
    try
      String.iteri
        (fun i c -> if c <> Cbuffer.get_char msg (i + point) then raise_notrace Not_equal)
        divider;
      point
    with Not_equal -> find_head (succ point)
       | _ -> 0
  in
  match find_head 0 with
  | 0 -> None, msg
  | x -> let h,tl = Cbuffer.split msg ~start:x (msg.len - x) in
         Some h, tl

let recv usb =
  Lwt_preemptive.detach (fun () -> Cyusb.recv usb)

let send usb divider port =
  let open Cbuffer in
  let send' port data =
    let msg = concat [divider; serialize port data] in
    Cyusb.send usb msg
  in
  Lwt_preemptive.detach (send' port)

let forward disp (port, data) =
  try
    let push = Hashtbl.find disp port in
    push data
  with _ -> () (*print_buf data*)
  
let create ?(sleep = 1.) ?(divider = divider) () =
  let usb      = Cyusb.create () in
  let dispatch = Hashtbl.create 100 in
  let recv     = recv usb in
  let send     = send usb (Cbuffer.of_string divider) in

  let msg_head = msg_head divider in
  
  let rec loop acc () =
    Lwt_unix.sleep sleep >>= fun () -> 
    recv () >>= fun buf ->
    let head, rest = msg_head buf in
    let msg_list   = Cbuffer.split_by_string divider rest in
    let new_acc, msgs = parse ~mstart:acc ~mend:head msg_list in
    List.iter (forward dispatch) msgs;
    loop new_acc ()
  in
  { usb; dispatch; send }, (fun () -> loop None ())

let subscribe obj id push =
  try
    let _ = Hashtbl.find obj.dispatch id in
    failwith (Printf.sprintf "Usb_device: Board %d is already connected" id)
  with _ -> Hashtbl.add obj.dispatch id push

let get_send obj id = obj.send id

(* TODO add proper finalize *)
          
let finalize obj =
  Cyusb.send obj.usb (Cbuffer.create 10);
  Cyusb.finalize ()
