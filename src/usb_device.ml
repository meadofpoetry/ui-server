open Containers
open Lwt.Infix
   
type t = { dispatch : (int, Cbuffer.t -> unit) Hashtbl.t
         ; send     : int -> Cbuffer.t -> unit Lwt.t
         }

type header = { len    : int
              ; port   : int
              }

let divider = String.of_array [| (char_of_int 0x44); (char_of_int 0xBB) |]

let print_buf b =
  Cbuffer.iter (fun x -> Lwt_io.printf " %02x " (int_of_char x) |> ignore) b;
  Lwt_io.printf "\n\n" |> ignore

let char_to_b c =
  let c = int_of_char c in
  let rec loop i s =
    if i = 8
    then s
    else loop (succ i) ((if ((c lsr i) land 1) = 0 then "0" else "1") ^ s)
  in "0b" ^ (loop 0 "")

let set_h parity port =
  let p   = if parity then (1 lsl 4) else 0 in
  char_of_int (p land port)

let get_h h =
  let i = int_of_char h in
  let parity = (i land (1 lsl 4)) <> 0 in
  let port   = (i land 0xf) in
  parity, port
              
let of_header buf =
  let open Cbuffer in
  let h = to_bytes buf in
  let len = int_of_char h.[1] in
  let c   = h.[0] in
  let parity, port = get_h c in
  let len  = (len * 2) - (if parity then 1 else 0) in
  { port; len }

let to_header h =
  let open Cbuffer in
  let b = create 2 in
  let parity = not ((h.len mod 2) = 0) in
  let len    = char_of_int h.len   in
  let c      = set_h parity h.port in
  Bigarray.Array1.unsafe_set b.buf 0 c;
  Bigarray.Array1.unsafe_set b.buf 1 len;
  b

let deserialize msg =
  let headersz = 2 in
  let headerb  = Cbuffer.sub msg headersz in
  let header   = of_header headerb in
  let bodysz   = let sz = (msg.sz - headersz) in
                 if sz < header.len then sz else header.len
  in
  let body     =
    try Cbuffer.sub msg ~start:headersz bodysz
    with Cbuffer.Bad_boundaries -> failwith "bad msg";
  in
  if  (body.sz < header.len)
  then `Partial (header.port , header.len, body)
  else `Full    (header.port , body)

let serialize port buf =
  let open Cbuffer in
  let h = { len  = buf.sz
          ; port = port
          } in
  rev_concat [buf; (to_header h)]
  
let parse ~old buf_list =
  let open Option.Infix in
  (*  List.iter print_buf buf_list;*)
  let msgs = List.map deserialize buf_list in
  let head = List.head_opt msgs
             >>= function
             | `Full x -> Some x
             | `Partial (port, len, body) ->
                old >>= fun (old_port, old_len, old_body) ->
                if Cbuffer.(old_body.sz + body.sz) = len && old_port = port && old_len = len
                then Some (port, Cbuffer.rev_concat [body; old_body])
                else None
  in
  let rec sieve acc = function
    | [] -> None, []
    | [`Partial x]  -> (Some x), acc
    | [`Full x]     -> None, (x::acc)
    | (`Full x)::xs -> sieve (x::acc) xs
    | (`Partial _)::xs -> sieve acc xs
  in
  let new_old, msgs = sieve [] (try List.tl msgs with _ -> []) in
  if Option.is_some head
  then new_old, ((Option.get_exn head)::msgs)
  else new_old, msgs

let recv usb =
  Lwt_preemptive.detach (fun () -> Cyusb.recv usb)

let send usb port =
  let open Cbuffer in
  let send' port data =
    let h = { len = data.sz; port } in
    let msg = rev_concat [data; (to_header h)] in
    Cyusb.send usb msg
  in 
  Lwt_preemptive.detach (send' port)

let forward disp (port, data) =
  try 
    let push = Hashtbl.find disp port in
    push data
  with _ -> () (*print_buf data*)
  
let create ?(sleep = 1.) () =
  let usb      = Cyusb.create () in
  let dispatch = Hashtbl.create 100 in
  let recv     = recv usb in
  let send     = send usb in
  
  let rec loop acc () =
    Lwt_unix.sleep sleep >>= fun () -> 
    recv () >>= fun buf ->
    let blist = Cbuffer.split divider buf in
    let new_acc, msgs = parse ~old:acc blist in
    List.iter (forward dispatch) msgs;
    loop new_acc ()
  in
  { dispatch; send }, (fun () -> loop None ())

let subscribe obj id push =
  try
    let _ = Hashtbl.find obj.dispatch id in
    failwith (Printf.sprintf "Usb_device: Board %d is already connected" id)
  with _ -> Hashtbl.add obj.dispatch id push

let finalize () =
  Cyusb.finalize ()
