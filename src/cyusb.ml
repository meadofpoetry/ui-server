open Cyusb_raw

let in_point = 0x86

let out_point = 0x02

let split sz s =
  let rec split' s =
    match s with
    | "" -> []
    | s when CCString.length s < sz -> [s]
    | s ->
       let (x, xs) = CCString.take_drop sz s in
       x :: (split' xs)
  in split' s

let write arr s =
  CCString.iteri (fun i c -> Ctypes.CArray.unsafe_set arr i c) s

let read arr sz =
  let r = Bytes.create sz in
  Bytes.iteri (fun i _ ->
      let c = Ctypes.CArray.unsafe_get arr i in Bytes.set r i c) r;
  Bytes.unsafe_to_string r

type t = { handle  : Cyusb_raw.handl
         ; in_max  : int
         ; out_max : int
         ; inp     : int
         ; outp    : int
         }
              
let create ?(inp = in_point) ?(outp = out_point) () =
  let i = Cyusb_raw.init () in
  if i <> 1
  then failwith (Printf.sprintf "Usb init-tion failure %d" i);
  let handle = Cyusb_raw.get_handle 0 in
  if Cyusb_raw.get_vendor handle <> (Unsigned.UShort.of_int 0x04b4)
  then failwith "Cypress chipset not detected";
  if Cyusb_raw.kernel_driver_active handle 0 <> 0
  then failwith "kernel driver active. Exitting";
  if Cyusb_raw.claim_interface handle 0 <> 0
  then failwith "Error in claiming interface";
  let in_max = Cyusb_raw.get_max_iso_packet_size handle (Unsigned.UChar.of_int inp) in
  let out_max = Cyusb_raw.get_max_iso_packet_size handle (Unsigned.UChar.of_int outp) in
  { handle
  ; inp
  ; outp
  ; in_max
  ; out_max
  }

let finalize = Cyusb_raw.close

let send usb s =
  let open Ctypes in
  let slst = split usb.out_max s in
  let got  = allocate int32_t 0l in
  let buf  = CArray.make Ctypes_static.char usb.out_max in
  let write = write buf in

  let rec send' = function
    | [] -> ()
    | [x]   ->
       write x;
       let _ = Cyusb_raw.bulk_transfer usb.handle (Unsigned.UChar.of_int usb.outp)
                                       (CArray.start buf) (CCString.length x)
                                       got 2
       in ()
    | x::xs ->
       write x;
       let _ = Cyusb_raw.bulk_transfer usb.handle (Unsigned.UChar.of_int usb.outp)
                                       (CArray.start buf) usb.out_max
                                       got 2
       in send' xs
  in
  send' slst
    

let recv usb =
  let open Ctypes in
  let got  = allocate int32_t 1l in
  let buf  = CArray.make Ctypes_static.char usb.in_max in
  let read = read buf in
  let rec recv' got s =
    if (!@ got) <= 0l then s
    else
      let _ = Cyusb_raw.bulk_transfer usb.handle (Unsigned.UChar.of_int usb.inp)
                                      (CArray.start buf) usb.in_max
                                      got 2
      in
      Lwt_io.printf "Got %d\n" (Int32.to_int (!@ got)) |> ignore;
      let s = s ^ (read (Int32.to_int (!@ got))) in
      recv' got s
  in 
  recv' got ""
