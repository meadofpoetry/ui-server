open Cyusb_raw

let in_point = 0x86

let out_point = 0x02

type t = { handle  : Cyusb_raw.handl
         ; in_max  : int
         ; out_max : int
         ; inp     : int
         ; outp    : int
         }
              
let create ?(inp = in_point) ?(outp = out_point) () =
  (* let i = Cyusb_raw.init () in
   * if i <> 1 (\* Only one device should be connected *\)
   * then failwith (Printf.sprintf "Usb init-tion failure %d" i); *)
  let handle = Cyusb_raw.get_handle 0 in
  (* if Cyusb_raw.get_vendor handle <> (Unsigned.UShort.of_int 0x04b4)
   * then failwith "Cypress chipset not detected";
   * if Cyusb_raw.kernel_driver_active handle 0 <> 0
   * then failwith "kernel driver active. Exitting";
   * if Cyusb_raw.claim_interface handle 0 <> 0
   * then failwith "Error in claiming interface";
   * let in_max = Cyusb_raw.get_max_iso_packet_size handle (Unsigned.UChar.of_int inp) in
   * let out_max = Cyusb_raw.get_max_iso_packet_size handle (Unsigned.UChar.of_int outp) in *)
  { handle
  ; inp
  ; outp
  ; in_max = 1
  ; out_max = 1
  }

let finalize = Cyusb_raw.close
let send usb b = ()
  (* let open Cbuffer in
   * let open Ctypes in
   * let got     = allocate int32_t 0l in
   * let buf_lst = Cbuffer.split_size usb.out_max b in
   * 
   * let rec send' = function
   *   | [] -> ()
   *   | x::xs ->
   *      let _ = Cyusb_raw.bulk_transfer usb.handle (Unsigned.UChar.of_int usb.outp)
   *                                      (get_ptr x) (len x)
   *                                      got 2
   *      in send' xs
   * in
   * send' buf_lst *)

let recv usb = Cstruct.create 0
  (* let open Cbuffer in
   * let open Ctypes in
   * let got  = allocate int32_t 1l in
   * let rec recv' () =
   *   let buf = Cbuffer.create_unsafe usb.in_max in
   *   let _   = Cyusb_raw.bulk_transfer usb.handle (Unsigned.UChar.of_int usb.inp)
   *               (get_ptr buf) (len buf)
   *               got 2
   *   in
   *   match Int32.to_int (!@ got) with
   *   | 0 -> []
   *   | x when x < 0 -> failwith "usb: reading failure"
   *   | len ->
   *      let r = if len < (Cbuffer.len buf)
   *              then Cbuffer.sub buf 0 len
   *              else buf
   *      in r::(recv' ())
   * in 
   * Cbuffer.concat @@ $41`recv' () *)
