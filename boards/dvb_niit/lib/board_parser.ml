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
 type emode =
   | T2 [@id 1]
   | T  [@id 2]
   | C  [@id 3] [@@uint8_t]]

[%%cenum
 type ebw =
   | Bw8 [@id 1]
   | Bw7 [@id 2]
   | Bw6 [@id 3] [@@uint8_t]]

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
   ; rfu        : uint8_t  (* former dvb-c qam *)
   ; freq       : uint32_t
   ; plp        : uint8_t
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

open Board_types

exception Parse_error

(* Misc *)

type parsed =
  { id   : int
  ; code : int
  ; body : Cbuffer.t
  ; res  : Cbuffer.t
  }

type err = Bad_tag_start        of int
         | Bad_length           of int
         | Bad_msg_code         of int
         | Bad_crc              of (int * int)
         | Bad_tag_stop         of int
         | Insufficient_payload of Cbuffer.t
         | Unknown_err          of string

let string_of_err = function
  | Bad_tag_start x        -> "incorrect start tag: "    ^ (string_of_int x)
  | Bad_length x           -> "incorrect length: "       ^ (string_of_int x)
  | Bad_msg_code x         -> "incorrect code/addr: "    ^ (string_of_int x)
  | Bad_crc (x,y)          -> "incorrect crc: expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | Bad_tag_stop x         -> "incorrect stop tag: "     ^ (string_of_int x)
  | Insufficient_payload _ -> "insufficient payload"
  | Unknown_err s          -> s

type events = { measure : measure_response React.event
              ; config  : config React.event
              }

type api = { devinfo     : unit -> devinfo_response Lwt.t
           ; reset       : unit -> unit Lwt.t
           ; settings    : settings_request -> settings_response Lwt.t
           ; plp_setting : plp_setting_request -> plp_setting_response Lwt.t
           ; plps        : int -> plp_list_response Lwt.t
           ; config      : unit -> config Lwt.t
           }

type _ request = Get_devinfo  : devinfo request
               | Reset        : unit request
               | Set_settings : (int * settings) -> settings_response request
               | Set_plp      : int * int        -> plp_setting_response request
               | Get_plps     : int              -> plp_list_response request

type event = Measure of measure_response

type _ event_request = Get_measure : int -> event event_request

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

let check_tag_start buf =
  let tag = get_prefix_tag_start buf in
  if tag <> tag_start then Error (Bad_tag_start tag) else Ok buf

let check_length buf =
  let length = get_prefix_length buf in
  if (length < 2) || (length > 41) then Error (Bad_length length) else Ok buf

let check_msg_code buf =
  let code'     = get_prefix_msg_code buf in
  let (id,code) = (code' land 0x0F, code' lsr 4) in
  match (id,code) with
  | 0xE,0xE                                          -> Ok (id,code,buf)           (* ack*)
  | 0,1                                              -> Ok (id,code,buf)           (* devinfo *)
  | (x,y) when (x >= 0 && x < 4) && (y > 1 && y < 7) -> Ok (id,code,buf)           (* other *)
  | _                                                -> Error (Bad_msg_code code')

let check_msg_crc (id,code,buf) =
  let payload_len = (get_prefix_length buf) - 1 in
  let total_len   = payload_len + sizeof_prefix + sizeof_suffix in
  let msg,res     = Cbuffer.split buf total_len in
  let pfx,msg'    = Cbuffer.split msg sizeof_prefix in
  let body,sfx    = Cbuffer.split msg' payload_len in
  let tag         = get_suffix_tag_stop sfx in
  if tag <> tag_stop then Error (Bad_tag_stop tag)
  else let crc  = (calc_crc (Cbuffer.append pfx body)) in
       let crc' = get_suffix_crc sfx in
       if crc <> crc' then Error (Bad_crc (crc,crc'))
       else Ok { id; code; body; res }

let check_msg msg =
  try
    CCResult.(check_tag_start msg
              >>= check_length
              >>= check_msg_code
              >>= check_msg_crc)
  with
  | Invalid_argument _ -> Error (Insufficient_payload msg)
  | e                  -> Error (Unknown_err (Printexc.to_string e))

(* Requests/responses *)

let of_mode : mode -> emode = function
  | T2 -> T2 | T -> T | C -> C
                           
let to_mode : emode -> mode = function
  | T2 -> T2 | T -> T | C -> C

let of_bw : bw -> ebw = function
  | Bw8 -> Bw8 | Bw7 -> Bw7 | Bw6 -> Bw6
                                   
let to_bw : ebw -> bw = function
  | Bw8 -> Bw8 | Bw7 -> Bw7 | Bw6 -> Bw6
                                   
(* Devinfo *)

let to_req_devinfo reset =
  let body = Cbuffer.create sizeof_cmd_devinfo in
  let () = set_cmd_devinfo_reset body (if reset then 0xFF else 0) in
  to_msg ~msg_code:0x10 ~body

let of_rsp_devinfo_exn msg =
  try
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
  with _ -> raise Parse_error

(* Settings *)

let to_req_settings id (settings : settings) =
  let body = Cbuffer.create sizeof_settings in
  let () = set_settings_mode body (emode_to_int @@ of_mode settings.mode) in
  let () = set_settings_bw body (ebw_to_int @@ of_bw settings.channel.bw) in
  let () = set_settings_freq body settings.channel.freq in
  let () = set_settings_plp body settings.channel.plp in
  to_msg ~msg_code:(0x20 lor id) ~body

let of_rsp_settings_exn msg =
  try
    let open CCOpt in
    { lock       = int_to_bool8 (get_settings_lock msg)       |> get_exn |> bool_of_bool8
    ; hw_present = int_to_bool8 (get_settings_hw_present msg) |> get_exn |> bool_of_bool8
    ; settings   = { mode     = to_mode @@ get_exn @@ int_to_emode (get_settings_mode msg)
                   ; channel  = { bw       = to_bw @@ get_exn @@ int_to_ebw (get_settings_bw msg)
                                ; freq     = get_settings_freq msg
                                ; plp      = get_settings_plp msg
                                }
                   }
    }
  with _ -> raise Parse_error

(* Measure *)

let to_req_measure id =
  to_empty_msg ~msg_code:(0x30 lor id)

let of_rsp_measure_exn msg =
  try
    { timestamp = Unix.gettimeofday ()
    ; lock      = int_to_bool8 (get_rsp_measure_lock msg) |> CCOpt.get_exn |> bool_of_bool8
    ; power     = get_rsp_measure_power msg
                  |> (fun x -> if x = max_uint16 then None else Some (-.((float_of_int x) /. 10.)))
    ; mer       = get_rsp_measure_mer msg
                  |> (fun x -> if x = max_uint16 then None else Some ((float_of_int x) /. 10.))
    ; ber       = get_rsp_measure_ber msg
                  |> (fun x -> if x = Int32.of_int max_uint32 then None else Some ((Int32.to_float x) /. (2.**24.)))
    ; freq      = get_rsp_measure_freq msg
                  |> (fun x -> if x = Int32.of_int max_uint32 then None else Some x)
    ; bitrate   = get_rsp_measure_bitrate msg
                  |> (fun x -> if x = Int32.of_int max_uint32 then None else Some x)
    }
  with _ -> raise Parse_error

(* Plp list *)

let to_req_plp_list id =
  to_empty_msg ~msg_code:(0x50 lor id)

let of_rsp_plp_list_exn msg =
  try
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
  with _ -> raise Parse_error

(* Plp set *)

let to_req_plp_set id plp =
  let body = Cbuffer.create sizeof_cmd_plp_set in
  let () = set_cmd_plp_set_plp_id body plp in
  to_msg ~msg_code:(0x60 lor id) ~body

let of_rsp_plp_set_exn msg =
  try
    { lock = int_to_bool8 (get_rsp_plp_set_lock msg) |> CCOpt.get_exn |> bool_of_bool8
    ; plp  = get_rsp_plp_set_plp msg
    }
  with _ -> raise Parse_error

let deserialize buf =
  (* split buffer into valid messages and residue (if any) *)
  let parse_msg = fun {id;code;body;_} ->
    match (id,code) with
    | 0xE,0xE0 -> (* Lwt_io.printl "got ack" |> ignore; *) `R `Ack
    | 0,1      -> (* Lwt_io.printl "got devinfo" |> ignore; *) `R (`Devinfo body)
    | _,2      -> (* Lwt_io.printlf "got settings (id = %d)" id |> ignore; *) `R (`Settings (id, body))
    | _,3      -> (* Lwt_io.printlf "got measure (id = %d)" id |> ignore; *) `E (`Measure (id, body))
    | _,5      -> `R (`Plps (id, body))
    | _,6      -> `R (`Plp_setting (id, body))
    | _        -> `N in
  let rec f events responses b =
    if Cbuffer.len b > (sizeof_prefix + 1 + sizeof_suffix)
    then (match check_msg b with
          | Ok x    -> parse_msg x
                       |> (function
                           | `E e   -> f (e::events) responses x.res
                           | `R r   -> f events (r::responses) x.res
                           | `N     -> f events responses x.res)
          | Error e -> (match e with
                        | Insufficient_payload x -> List.rev events, List.rev responses, x
                        | _                      -> f events responses (Cbuffer.shift b 1)))
    else List.rev events, List.rev responses, b in
  let events, responses, res = f [] [] buf in
  events, responses, if Cbuffer.len res > 0 then Some res else None

let try_parse f x =
  try Some (f x) with Parse_error -> None

let parse_devinfo = function
  | `Devinfo buf -> try_parse of_rsp_devinfo_exn buf
  | _ -> None

let parse_reset = function `Ack -> Some () | _ -> None

let parse_settings id = function
  | `Settings (idx, buf) when idx = id -> try_parse (fun b -> id, (of_rsp_settings_exn b)) buf
  | _ -> None

let parse_plp_settings id = function
  | `Plp_settings (idx, buf) when idx = id -> try_parse (fun b -> id, (of_rsp_plp_set_exn b)) buf
  | _ -> None

let parse_measures id = function
  | `Measure (idx, buf) when idx = id -> try_parse (fun b -> Measure (id, (of_rsp_measure_exn b))) buf
  | _ -> None

let parse_plps id = function
  | `Plps (idx, buf) when idx = id -> try_parse (fun b -> id, (of_rsp_plp_list_exn b)) buf
  | _ -> None

let is_response (type a) (req : a request) msg : a option =
  match req with
  | Reset               -> parse_reset msg
  | Get_devinfo         -> parse_devinfo msg
  | Set_settings (id,_) -> parse_settings id msg
  | Set_plp (id,_)      -> parse_plp_settings id msg
  | Get_plps id         -> parse_plps id msg

let is_event (type a) (req : a event_request) msg : a option =
  match req with
  | Get_measure id -> parse_measures id msg
