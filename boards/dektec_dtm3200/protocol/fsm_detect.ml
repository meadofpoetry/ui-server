open Boards
open Boards.Pools
open Board_dektec_dtm3200_types
open Request
open Netlib

let ( >>= ) = Lwt.bind

let timeout = 3. (* seconds *)

let sleep timeout =
  Lwt_unix.sleep timeout
  >>= fun () -> Lwt.return (Error "timeout")

let loop (type a) stream (req : a Request.t) : (a, string) result Lwt.t =
  let rec aux () =
    Lwt_stream.next stream
    >>= fun x ->
    match Parser.is_response req x with
    | None -> aux ()
    | Some x -> Lwt.return x in
  Lwt_stream.junk_old stream >>= aux

let step ~(address : int)
      ~return
      ~continue
      (src : Logs.src)
      (sender : Cstruct.t -> unit Lwt.t)
      (stream : Cstruct.t cmd Lwt_stream.t) =

  let (module Logs : Logs.LOG) = Logs.src_log src in

  let on_ok (type a) (req : a Request.t) to_string v =
    Logs.debug (fun m ->
        m "got %s (%s)" (Request.to_string req) (to_string v)) in

  let on_error (type a) (req : a Request.t) e =
    Logs.warn (fun m ->
        m "error while getting %s - %s" (to_string req) e) in

  let rec fpga_version () =
    let req = Device FPGA_version in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok x -> on_ok req string_of_int x; hw_version GList.(x :: [])

  and hw_version acc =
    let req = Device Hardware_version in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok x -> on_ok req string_of_int x; fw_version GList.(x :: acc)

  and fw_version acc =
    let req = Device Firmware_version in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok x -> on_ok req string_of_int x; serial GList.(x :: acc)

  and serial acc =
    let req = Device Serial_number in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok x -> on_ok req string_of_int x; typ GList.(x :: acc)

  and typ acc =
    let req = Device Type in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok x -> on_ok req string_of_int x; mac_address GList.(x :: acc)

  and mac_address acc =
    let req = Network MAC_address in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> on_error req e; return ()
    | Ok mac ->
       on_ok req Netlib.Macaddr.to_string mac;
       let devinfo = match acc with
         | typ :: serial :: fw_ver :: hw_ver :: fpga_ver :: [] ->
            { fpga_ver
            ; hw_ver
            ; fw_ver
            ; serial
            ; typ
            ; mac
            } in
       continue devinfo

  in fpga_version
