type resp  = Test of unit
type req   = Resp of unit

let tag_start = 0x55AA
let tag_stop  = 0xFE

let max_uint16 = Unsigned.(UInt16.to_int UInt16.max_int)
let max_uint32 = Unsigned.(UInt32.to_int UInt32.max_int)

[@@@ocaml.warning "-32"]

[%%cenum
 type bool8 =
   | True  [@id 0xFF]
   | False [@id 0] [@@uint8_t]]

let bool_of_bool8 = function
  | True  -> true
  | False -> false

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

let mode_to_yojson x = `String (mode_to_string x)
let mode_of_yojson = function
  | `String s -> begin match string_to_mode s with
                 | Some x -> Ok x
                 | None   -> Error ("mode_of_yojson: unknown value " ^ s)
                 end
  | _ as e    -> Error ("mode_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let bw_to_yojson x = `String (bw_to_string x)
let bw_of_yojson = function
  | `String s -> begin match string_to_bw s with
                 | Some x -> Ok x
                 | None   -> Error ("bw_of_yojson: unknown value " ^ s)
                 end
  | _ as e    -> Error ("bw_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

let dvbc_qam_to_yojson x = `String (dvbc_qam_to_string x)
let dvbc_qam_of_yojson = function
  | `String s -> begin match string_to_dvbc_qam s with
                 | Some x -> Ok x
                 | None   -> Error ("dvbc_qam_of_yojson: unknown value " ^ s)
                 end
  | _ as e    -> Error ("dvbc_qam_of_yojson: unknown value " ^ (Yojson.Safe.to_string e))

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
   { plp_id : uint8_t
   } [@@little_endian]]

[%%cstruct
 type rsp_plp_set =
   { lock : uint8_t
   ; plp  : uint8_t
   } [@@little_endian]]

[@@@ocaml.warning "+32"]

type err = Bad_tag_start of int
         | Bad_length of int
         | Bad_msg_code of int
         | Bad_module_addr of int
         | Bad_crc of (int * int)
         | Bad_tag_stop of int
         | Unknown_err of string

let string_of_err = function
  | Bad_tag_start x   -> "incorrect start tag: "    ^ (string_of_int x)
  | Bad_length x      -> "incorrect length: "       ^ (string_of_int x)
  | Bad_msg_code x    -> "incorrect code: "         ^ (string_of_int x)
  | Bad_module_addr x -> "incorrect address: "      ^ (string_of_int x)
  | Bad_crc (x,y)     -> "incorrect crc: expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | Bad_tag_stop x    -> "incorrect stop tag: "     ^ (string_of_int x)
  | Unknown_err s     -> s

type rsp_devinfo =
  { serial   : int
  ; hw_ver   : int
  ; fpga_ver : int
  ; soft_ver : int
  ; asi      : bool
  ; modules  : int list
  } [@@deriving to_yojson]

type settings =
  { mode : mode
  ; bw   : bw
  ; dvbc_qam : dvbc_qam
  ; freq     : int32
  ; plp_id   : int
  } [@@deriving yojson]

type rsp_settings =
  { settings   : settings
  ; hw_present : bool
  ; lock       : bool
  } [@@deriving to_yojson]

type rsp_measure =
  { lock    : bool
  ; power   : float option
  ; mer     : float option
  ; ber     : float option
  ; freq    : int32 option
  ; bitrate : int32 option
  } [@@deriving to_yojson]

type rsp_plp_list =
  { lock    : bool
  ; plps    : int list
  } [@@deriving to_yojson]

type rsp_plp_set =
  { lock    : bool
  ; plp     : int
  } [@@deriving to_yojson]

(* Helper functions *)

let calc_crc (msg:Cbuffer.t) =
  let _,body = Cbuffer.split msg (sizeof_prefix - 1) in
  let iter = Cbuffer.iter (fun _ -> Some 1)
                          (fun buf -> Cbuffer.get_uint8 buf 0)
                          body in
  Cbuffer.fold (fun acc el -> el lxor acc) iter 0

(* Message constructors *)

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
  let prefix = to_prefix ~length:(Cbuffer.len body + 1) ~msg_code in
  let msg = Cbuffer.append prefix body in
  let suffix  = to_suffix ~crc:(calc_crc msg) in
  Cbuffer.append msg suffix

let to_empty_msg ~msg_code =
  let body = Cbuffer.create 1 in
  to_msg ~msg_code ~body

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

(* Requests/responses *)

(* Devinfo *)

let to_req_devinfo reset =
  let body = Cbuffer.create sizeof_cmd_devinfo in
  let () = set_cmd_devinfo_reset body (if reset then 0xFF else 0) in
  to_msg ~msg_code:0x10 ~body

let of_rsp_devinfo_exn msg =
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

(* Settings *)

let to_req_settings id settings =
  let body = Cbuffer.create sizeof_settings in
  let () = set_settings_mode body (mode_to_int settings.mode) in
  let () = set_settings_bw body (bw_to_int settings.bw) in
  let () = set_settings_dvbc_qam body (dvbc_qam_to_int settings.dvbc_qam) in
  let () = set_settings_freq body settings.freq in
  let () = set_settings_plp_id body settings.plp_id in
  to_msg ~msg_code:(0x20 lor id) ~body

let of_rsp_settings_exn msg =
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

(* Measure *)

let to_req_measure id =
  to_empty_msg ~msg_code:(0x30 lor id)

let of_rsp_measure_exn msg =
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

(* Plp list *)

let to_req_plp_list id =
  to_empty_msg ~msg_code:(0x50 lor id)

let of_rsp_plp_list_exn msg =
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

(* Plp set *)

let to_req_plp_set id plp =
  let body = Cbuffer.create sizeof_cmd_plp_set in
  let () = set_cmd_plp_set_plp_id body plp in
  to_msg ~msg_code:(0x60 lor id) ~body

let of_rsp_plp_set_exn msg =
  { lock = int_to_bool8 (get_rsp_plp_set_lock msg) |> CCOpt.get_exn |> bool_of_bool8
  ; plp  = get_rsp_plp_set_plp msg
  }

(* Deserialize *)

let parse_msgs msgs =
    let parsed = List.map (fun m -> let code     = (get_prefix_msg_code m) land 0xF0 in
                                    let id       = (get_prefix_msg_code m) land 0x0F in
                                    try
                                      let (_,body) = Cbuffer.split m sizeof_prefix in
                                      match (id,code) with
                                      | 0xE,0xE0 -> `Ok
                                      | _,0x10   -> `Devinfo (of_rsp_devinfo_exn body)
                                      | _,0x20   -> `Settings (id, (of_rsp_settings_exn body))
                                      | _,0x30   -> `Measure (id, (of_rsp_measure_exn body))
                                      | _,0x50   -> `Plps (id, (of_rsp_plp_list_exn body))
                                      | _,0x60   -> `Plp (id, (of_rsp_plp_set_exn body))
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

let (init : req) = Resp ()

let (probes : req list) = [Resp ()]

let period = 5

let (serialize : req -> Board_meta.req_typ * Cbuffer.t) = fun _ -> `Instant, (Cbuffer.create 5)

let deserialize = fun _ -> [], None

let is_response = fun _ _ -> None
