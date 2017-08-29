open Common.Hardware
open Api_handler
open Interaction
open Board_meta

module V1 : BOARD = struct

  type err = Bad_tag_start of int
           | Bad_length of int
           | Bad_msg_code of int
           | Bad_module_addr of int
           | Bad_crc of (int * int)
           | Bad_tag_stop of int
           | Unknown_err of string

  [@@@ocaml.warning "-32"]

  [%%cenum
   type bool8 =
     | True  [@id 0xFF]
     | False [@id 0] [@@uint8_t]]

  [%%cenum
   type mode =
     | T2 [@id 1]
     | T  [@id 2]
     | C  [@id 3] [@@uint8_t]]

  [%%cenum
   type bw =
     | Bw8 [@id 1]
     | Bw7 [@id 2]
     | Bw6 [@id 3] [@@uint8_t]]

  [%%cenum
   type dvbc_qam =
     | Qam32
     | Qam64
     | Qam128
     | Qam256 [@@uint8_t]]

  [%%cstruct
   type prefix =
     { tag_start : uint16_t
     ; length    : uint8_t
     ; msg_code  : uint8_t
     } [@@little_endian]]

  [%%cstruct
   type suffix =
     { crc      : uint8_t
     ; tag_stop : uint8_t
     } [@@little_endian]]

  [%%cstruct
   type cmd_devinfo =
     { reset    : uint8_t
     } [@@little_endian]]

  [%%cstruct
   type rsp_devinfo =
     { serial    : uint16_t
     ; hw_ver    : uint8_t
     ; fpga_ver  : uint8_t
     ; soft_ver  : uint8_t
     ; hw_config : uint8_t
     ; rfu       : uint32_t
     } [@@little_endian]]

  [%%cstruct
   type settings =
     { mode       : uint8_t
     ; bw         : uint8_t
     ; hw_present : uint8_t  (* rfu in cmd *)
     ; dvbc_qam   : uint8_t
     ; freq       : uint32_t
     ; plp_id     : uint8_t
     ; lock       : uint8_t  (* rfu in cmd *)
     } [@@little_endian]]

  [%%cstruct
   type rsp_measure =
     { lock    : uint8_t
     ; power   : uint16_t
     ; mer     : uint16_t
     ; ber     : uint32_t
     ; freq    : uint32_t
     ; bitrate : uint32_t
     } [@@little_endian]]

  [%%cstruct
   type cmd_plp_set =
     { plp_id : uint8_t } [@@little_endian]]

  [%%cstruct
   type rsp_plp_set =
     { lock : uint8_t
     ; plp  : uint8_t } [@@little_endian]]

  (* [@@@ocaml.warning "+32"] *)

  type devinfo =
    { serial   : int
    ; hw_ver   : int
    ; fpga_ver : int
    ; soft_ver : int
    ; asi      : bool
    ; modules  : int list
    }

  type settings =
    { mode : mode
    ; bw   : bw
    ; dvbc_qam : dvbc_qam
    ; freq     : int32
    ; plp_id   : int
    }

  type settings_resp =
    { settings   : settings
    ; hw_present : bool
    ; lock       : bool
    }

  type measure =
    { lock    : bool
    ; power   : float option
    ; mer     : float option
    ; ber     : float option
    ; freq    : int32 option
    ; bitrate : int32 option
    }

  type plp_list =
    { lock    : bool
    ; plps    : int list
    }

  type plp_set =
    { lock    : bool
    ; plp     : int
    }

      [@@@ocaml.warning "-34-37"]

  type _ request =
    | Devinfo  : bool           -> devinfo request
    | Measure  : int            -> measure request
    | Plps     : int            -> plp_list request
    | Settings : int * settings -> settings_resp request
    | Plp      : int * int      -> plp_set request

  (* Constants and helper functions *)

  let tag_start = 0x55AA
  let tag_stop  = 0xFE

  let max_uint16 = Unsigned.(UInt16.to_int UInt16.max_int)
  let max_uint32 = Unsigned.(UInt32.to_int UInt32.max_int)

  let string_of_err = function
    | Bad_tag_start x   -> "incorrect start tag: "    ^ (string_of_int x)
    | Bad_length x      -> "incorrect length: "       ^ (string_of_int x)
    | Bad_msg_code x    -> "incorrect code: "         ^ (string_of_int x)
    | Bad_module_addr x -> "incorrect address: "      ^ (string_of_int x)
    | Bad_crc (x,y)     -> "incorrect crc: expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
    | Bad_tag_stop x    -> "incorrect stop tag: "     ^ (string_of_int x)
    | Unknown_err s     -> s

  let calc_crc (msg:Cbuffer.t) =
    let _,body = Cbuffer.split msg (sizeof_prefix - 1) in
    let iter = Cbuffer.iter (fun _ -> Some 1)
                            (fun buf -> Cbuffer.get_uint8 buf 0)
                            body in
    Cbuffer.fold (fun acc el -> el lxor acc) iter 0

  let length (msg:Cbuffer.t) =
    (Cbuffer.len msg) + 1

  let bool_of_bool8 = function
    | True  -> true
    | False -> false

  let to_prefix ~length ~msg_code =
    let prefix = Cbuffer.create sizeof_prefix in
    let () = set_prefix_tag_start prefix tag_start in
    let () = set_prefix_length prefix length in
    let () = set_prefix_msg_code prefix msg_code in
    prefix

  let to_suffix ~crc =
    let suffix = Cbuffer.create sizeof_suffix in
    let () = set_suffix_crc suffix crc in
    let () = set_suffix_tag_stop suffix tag_stop in
    suffix

  let to_msg ~msg_code ~body =
    let prefix = to_prefix ~length:(length body) ~msg_code in
    let msg = Cbuffer.append prefix body in
    let suffix  = to_suffix ~crc:(calc_crc msg) in
    Cbuffer.append msg suffix

  let to_empty_msg ~msg_code =
    let body = Cbuffer.create 1 in
    to_msg ~msg_code ~body

  (* Requests *)

  let cmd_devinfo reset =
    let body = Cbuffer.create sizeof_cmd_devinfo in
    let () = set_cmd_devinfo_reset body (if reset then 0xFF else 0) in
    to_msg ~msg_code:0x10 ~body

  let cmd_settings id settings =
    let body = Cbuffer.create sizeof_settings in
    let () = set_settings_mode body (mode_to_int settings.mode) in
    let () = set_settings_bw body (bw_to_int settings.bw) in
    let () = set_settings_dvbc_qam body (dvbc_qam_to_int settings.dvbc_qam) in
    let () = set_settings_freq body settings.freq in
    let () = set_settings_plp_id body settings.plp_id in
    to_msg ~msg_code:(0x20 lor id) ~body

  let cmd_measure id =
    to_empty_msg ~msg_code:(0x30 lor id)

  let cmd_plp_list id =
    to_empty_msg ~msg_code:(0x50 lor id)

  let cmd_plp_set id plp =
    let body = Cbuffer.create sizeof_cmd_plp_set in
    let () = set_cmd_plp_set_plp_id body plp in
    to_msg ~msg_code:(0x60 lor id) ~body

  (* Message validation *)

  let check_tag_start msg =
    let tag = get_prefix_tag_start msg in
    if tag != tag_start then Error (Bad_tag_start tag) else Ok msg

  let check_length msg =
    let length = get_prefix_length msg in
    if (length < 2) || (length > 41) then Error (Bad_length length) else Ok msg

  let check_msg_code msg =
    let code = get_prefix_msg_code msg in
    match (code lsr 4, code land 0x0F) with
    | 0x1,_ | 0x2,_ | 0x3,_ | 0x4,_ | 0x5,_ | 0x6,_ | 0xE,0xE -> Ok msg
    | _ -> Error (Bad_msg_code code)

  let check_msg_addr msg =
    let code = get_prefix_msg_code msg in
    match (code lsr 4, code land 0x0F) with
    | 0xE,0xE               -> Ok msg
    | _,0 | _,1 | _,2 | _,3 -> Ok msg
    | _,bad_id              -> Error (Bad_module_addr bad_id)

  let check_msg_crc msg =
    let prefix,msg' = Cbuffer.split msg sizeof_prefix in
    let length      = get_prefix_length prefix in
    let body,suffix = Cbuffer.split msg' (length - 1) in
    let crc         = (calc_crc (Cbuffer.append prefix body)) in
    let crc'        = get_suffix_crc suffix in
    if crc != crc' then Error (Bad_crc (crc,crc')) else Ok msg

  let check_tag_stop msg =
    let prefix,msg' = Cbuffer.split msg sizeof_prefix in
    let length      = get_prefix_length prefix in
    let _,suffix    = Cbuffer.split msg' (length - 1) in
    let tag         = get_suffix_tag_stop suffix in
    if tag != tag_stop then Error (Bad_tag_stop tag) else Ok msg

  let check_msg msg =
    try
      CCResult.(check_tag_start msg
                >>= check_length
                >>= check_msg_code
                >>= check_msg_addr
                >>= check_tag_stop
                >>= check_msg_crc)
    with e -> Error (Unknown_err (Printexc.to_string e))

  (* Message parsers *)

  let parse_devinfo_exn msg =
    let hw_cfg    = get_rsp_devinfo_hw_config msg in
    { serial   = get_rsp_devinfo_serial msg
    ; hw_ver   = get_rsp_devinfo_hw_ver msg
    ; fpga_ver = get_rsp_devinfo_fpga_ver msg
    ; soft_ver = get_rsp_devinfo_soft_ver msg
    ; asi      = if (hw_cfg land 16) > 0 then true else false
    ; modules  = List.fold_left (fun acc x -> let x' = float_of_int x in
                                              if (hw_cfg land (int_of_float (2. ** x'))) > 0
                                              then x :: acc
                                              else acc)
                                []
                                (CCList.range 0 3)
    }

  let parse_settings_exn msg =
    let open CCOpt in
    { lock       = int_to_bool8 (get_settings_lock msg)       |> get_exn |> bool_of_bool8
    ; hw_present = int_to_bool8 (get_settings_hw_present msg) |> get_exn |> bool_of_bool8
    ; settings   = { mode     = get_exn @@ int_to_mode (get_settings_mode msg)
                   ; bw       = get_exn @@ int_to_bw (get_settings_bw msg)
                   ; dvbc_qam = get_exn @@ int_to_dvbc_qam (get_settings_dvbc_qam msg)
                   ; freq     = get_settings_freq msg
                   ; plp_id   = get_settings_plp_id msg
                   }
    }

  let parse_measure_exn msg =
    { lock    = int_to_bool8 (get_rsp_measure_lock msg) |> CCOpt.get_exn |> bool_of_bool8
    ; power   = get_rsp_measure_power msg
                |> (fun x -> if x = max_uint16 then None else Some (-.((float_of_int x) /. 10.)))
    ; mer     = get_rsp_measure_power msg
                |> (fun x -> if x = max_uint32 then None else Some ((float_of_int x) /. 10.))
    ; ber     = Int32.to_int @@ get_rsp_measure_ber msg
                |> (fun x -> if x = max_uint32 then None else Some ((float_of_int x) /. (2.**24.)))
    ; freq    = get_rsp_measure_freq msg
                |> (fun x -> if x = Int32.of_int max_uint32 then None else Some x)
    ; bitrate = get_rsp_measure_bitrate msg
                |> (fun x -> if x = Int32.of_int max_uint32 then None else Some x)
    }

  let parse_plp_list_exn msg =
    let plp_num     = Cbuffer.get_uint8 msg 1 |> (fun x -> if x = 0xFF then None else Some x) in
    { lock = int_to_bool8 (Cbuffer.get_uint8 msg 0) |> CCOpt.get_exn |> bool_of_bool8
    ; plps = begin match plp_num with
             | Some _ -> let iter = Cbuffer.iter (fun _ -> Some 1)
                                                 (fun buf -> Cbuffer.get_uint8 buf 0)
                                                 (Cbuffer.shift msg 2) in
                         Cbuffer.fold (fun acc el -> el :: acc) iter []
             | None   -> []
             end
    }

  let parse_plp_exn msg =
    { lock = int_to_bool8 (get_rsp_plp_set_lock msg) |> CCOpt.get_exn |> bool_of_bool8
    ; plp  = get_rsp_plp_set_plp msg
    }

  let parse_msgs msgs =
    let parsed = List.map (fun m -> let code     = (get_prefix_msg_code m) land 0xF0 in
                                    let id       = (get_prefix_msg_code m) land 0x0F in
                                    try
                                      let (_,body) = Cbuffer.split m sizeof_prefix in
                                      match (id,code) with
                                      | 0xE,0xE0 -> `Ok
                                      | _,0x10   -> `Devinfo (parse_devinfo_exn body)
                                      | _,0x20   -> `Settings (id, (parse_settings_exn body))
                                      | _,0x30   -> `Measure (id, (parse_measure_exn body))
                                      | _,0x50   -> `Plps (id, (parse_plp_list_exn body))
                                      | _,0x60   -> `Plp (id, (parse_plp_exn body))
                                      | _        -> `Unknown
                                    with _ -> `Corrupted)
                          msgs in
    parsed
    (* List.filter (function *)
    (*              | `Devinfo _ | `Settings _ | `Measure _ | `Plps _ | `Plp _ -> true *)
    (*              | _ -> false) *)
    (*             parsed *)

  let parse ?old buf =
    let buf' = begin match old with
               | Some x -> Cbuffer.append x buf
               | None   -> buf
               end in
    let rec f acc b =
      if Cbuffer.len b > (sizeof_prefix + 1 + sizeof_suffix)
      then begin match check_msg b with
           | Ok x    -> let len     = ((get_prefix_length x) - 1) + sizeof_prefix + sizeof_suffix in
                        let msg,res = Cbuffer.split x len in
                        f (msg::acc) res
           | Error _ -> let _,res = Cbuffer.split b 1 in
                        f acc res
           end
      else List.rev acc,b in
    f [] buf'

  (* BOARD implementation *)

  let handle send _ id meth args _ _ _ =
    let open Redirect in
    let redirect_if_guest = redirect_if (User.eq id `Guest) in
    match meth, args with
    | `POST, ["settings"] -> redirect_if_guest not_found
    | `POST, ["plp"]      -> redirect_if_guest not_found
    | `GET,  ["devinfo"]  -> let cmd = (cmd_devinfo false) in
                             send cmd |> ignore;
                             respond_string "sent cmd devinfo!" ()
    | `GET,  ["params"]   -> not_found ()
    | `GET,  ["meas"]     -> not_found ()
    | `GET,  ["plps"]     -> let cmd = (cmd_plp_list 2) in
                             send cmd |> ignore;
                             respond_string "send cmd plp list" ()
    | _ -> not_found ()

  let handlers id send=
    [ (module struct
         let domain = get_api_path id
         let handle = handle send ()
       end : HANDLER) ]

  let create (b:topo_board) send =
    let e_msgs,push = React.E.create () in
    let e_map = React.E.fold (fun old buf -> let msgs,res = parse ?old buf in
                                             (parse_msgs msgs)
                                             |> List.map (function
                                                          | `Ok -> "rsp ok"
                                                          | `Devinfo _ -> "devinfo"
                                                          | `Settings _ -> "settings"
                                                          | `Measure _ -> "measure"
                                                          | `Plps _ -> "plps"
                                                          | `Plp _ -> "plp"
                                                          | `Corrupted -> "corrupted"
                                                          | _ -> "unknown")
                                             |> List.map (Lwt_io.printf "%s\n")
                                             |> ignore;
                                             (Some res))
                             None
                             e_msgs in
    let state = object method e_msgs = e_msgs; method emap = e_map end in
    { handlers       = handlers b.control send
    ; receiver       = push
    ; streams_signal = None
    ; is_converter   = false
    ; state          = (state :> < >)
    }

  let connect_db b _ = b
end

let create = function
  | 1 -> (module V1 : BOARD)
  | v -> failwith ("dvb board: unknown version " ^ (string_of_int v))
