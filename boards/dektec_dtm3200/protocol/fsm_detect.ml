open Board_dektec_dtm3200_types
open Request
open Netlib
open Fsm_common

let ( >>= ) = Lwt.bind

let timeout = 3. (* seconds *)

let step ~(address : int)
    ~(return : unit -> unit Lwt.t)
    ~(continue : devinfo -> unit Lwt.t)
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (stream : Cstruct.t cmd Lwt_stream.t) =

  let rec fpga_version () =
    let req = Device FPGA_version in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; hw_version GList.(x :: [])

  and hw_version acc =
    let req = Device Hardware_version in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; fw_version GList.(x :: acc)

  and fw_version acc =
    let req = Device Firmware_version in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; serial GList.(x :: acc)

  and serial acc =
    let req = Device Serial_number in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; typ GList.(x :: acc)

  and typ acc =
    let req = Device Type in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok x -> log_ok src req x; mac_address GList.(x :: acc)

  and mac_address acc =
    let req = Network MAC_address in
    sender @@ Serializer.make_req ~address req
    >>= fun () -> Lwt.pick [loop stream req; sleep timeout]
    >>= function
    | Error e -> log_error src req e; return ()
    | Ok mac ->
      log_ok src req mac;
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
