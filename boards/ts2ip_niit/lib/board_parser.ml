open Board_types
open Board_msg_formats

type _ request         = Get_board_info   : devinfo request

type _ instant_request = Set_board_mode   : nw_settings * packer_setting list -> unit instant_request
                       | Set_factory_mode : factory_settings                  -> unit instant_request

type api    = { devinfo            : unit                   -> devinfo_response Lwt.t
              ; set_mode           : nw_settings            -> unit Lwt.t
              ; set_factory_mode   : factory_settings       -> unit Lwt.t
              ; set_streams_simple : Common.Stream.t list   -> (unit,string) Lwt_result.t
              ; set_streams_full   : streams_full_request   -> (unit,string) Lwt_result.t
              ; config             : unit                   -> config_response Lwt.t
              }

type events = { status : status React.event
              ; config : config_response React.event
              }

let prefix = 0x55AA

(* Message constructors *)

let to_header ~msg_code () =
  let hdr = Cbuffer.create sizeof_header in
  let ()  = set_header_prefix hdr prefix in
  let ()  = set_header_msg_code hdr msg_code in
  hdr

let to_msg ~msg_code ~body () =
  let hdr = to_header ~msg_code () in
  Cbuffer.append hdr body

(* Requests *)

module type Request = sig
  type req
  type rsp
  val req_code : int
  val rsp_code : int
  val to_cbuffer : req -> Cbuffer.t
  val of_cbuffer : Cbuffer.t -> rsp
end

module Get_board_info : (Request with type req := unit with type rsp := devinfo) = struct

  let req_code = 0x0080
  let rsp_code = 0x0140
  let to_cbuffer () = to_msg ~msg_code:req_code ~body:(Cbuffer.create 0) ()
  let of_cbuffer msg =
    { typ         = get_board_info_board_type msg
    ; ver         = get_board_info_board_version msg
    ; packers_num = match get_board_info_packers_num msg with
                    | 0 -> Some 8
                    | _ -> None
    }

end

(* Instant requests *)

module type Instant_request = sig
  type req
  val msg_code : int
  val to_cbuffer : req -> Cbuffer.t
end

module Set_factory_mode : (Instant_request with type req := factory_settings) = struct

  let msg_code = 0x0087
  let to_cbuffer mode =
    let body = Cbuffer.create sizeof_factory_settings in
    let () = set_factory_settings_mac (Macaddr.to_bytes mode.mac) 0 body in
    to_msg ~msg_code ~body ()

end

module Set_board_mode : (Instant_request with type req := (nw_settings * (packer_setting list))) = struct

  let msg_code  = 0x0088

  let reverse_ip x   = Ipaddr.V4.to_bytes x |> CCString.rev |> Ipaddr.V4.of_bytes_exn
  let reverse_port x = let msb = (x land 0xFF00) lsr 8 in
                       let lsb = (x land 0x00FF) in
                       (lsb lsl 8) lor msb
  let reverse_int32 x = reverse_ip @@ Ipaddr.V4.of_int32 x
                        |> Ipaddr.V4.to_int32

  let packer_settings_to_cbuffer (s:packer_setting) =
    let buf  = Cbuffer.create sizeof_packer_settings in
    let mode = (s.port lsl 1) |> (fun x -> if s.base.enabled then x lor 1 else x) in
    (* FIXME temp reverse *)
    let ()   = Ipaddr.V4.to_int32 (reverse_ip s.base.dst_ip) |> set_packer_settings_dst_ip buf in
    let ()   = (reverse_port s.base.dst_port) |> set_packer_settings_dst_port buf in
    let ()   = Ipaddr.V4.multicast_to_mac s.base.dst_ip |> Macaddr.to_bytes
               |> fun mac -> set_packer_settings_dst_mac mac 0 buf in
    let ()   = set_packer_settings_self_port buf (reverse_port s.self_port) in
    let ()   = set_packer_settings_mode buf mode in
    let ()   = set_packer_settings_stream_id buf (reverse_int32 s.stream_id) in
    buf

  let main_to_cbuffer ip mask gw (pkrs:packer_setting list) =
    let buf  = Cbuffer.create sizeof_req_settings_main in
    let ()   = set_req_settings_main_cmd buf 0 in
    (* FIXME temp reverse *)
    let ()   = Ipaddr.V4.to_int32 (reverse_ip ip)   |> set_req_settings_main_ip buf in
    let ()   = Ipaddr.V4.to_int32 (reverse_ip mask) |> set_req_settings_main_mask buf in
    let ()   = Ipaddr.V4.to_int32 (reverse_ip gw)   |> set_req_settings_main_gateway buf in
    let pkrs = CCList.map packer_settings_to_cbuffer pkrs in
    let len  = sizeof_req_settings_main + (4 * sizeof_packer_settings) in
    Cbuffer.concat @@ buf :: pkrs
    |> (fun b -> Cbuffer.append b @@ Cbuffer.create (len - Cbuffer.len b))
    |> (fun body -> to_msg ~msg_code ~body ())

  let rest_to_cbuffer i (pkrs:packer_setting list) =
    let buf  = Cbuffer.create sizeof_req_settings_packers in
    let ()   = set_req_settings_packers_cmd buf i in
    let pkrs = CCList.map packer_settings_to_cbuffer pkrs in
    let len  = sizeof_req_settings_packers + (6 * sizeof_packer_settings) in
    Cbuffer.concat @@ buf :: pkrs
    |> (fun b -> Cbuffer.append b @@ Cbuffer.create (len - Cbuffer.len b))
    |> (fun body -> to_msg ~msg_code ~body ())

  let to_cbuffer (nw,packers) =
    (* let default = { base = { enabled = false
     *                        ; dst_ip  = Ipaddr.V4.make 0 0 0 0
     *                        ; dst_port = 0
     *                        ; stream = { id = `Ts Single; description = None; source = Input { id = 0; input = RF }}}
     *               ; port = 0
     *               ; stream_id = 0l
     *               ; self_port = 0
     *               }
     * in
     * let packers = if CCList.length packers < 22
     *               then packers @ CCList.repeat (22 - CCList.length packers) [default]
     *               else packers
     * in *)
    let fst_pkrs,rest_pkrs =
      let hd,tl = CCList.take_drop 4 packers in
      hd,CCList.take 3 @@ CCList.sublists_of_len ~last:CCOpt.return 6 tl in
    let main = main_to_cbuffer nw.ip nw.mask nw.gateway fst_pkrs in
    let rest = CCList.mapi (fun i x -> rest_to_cbuffer (succ i) x) rest_pkrs in
    Cbuffer.concat (main :: rest)

end

(* Events  *)

module type Event = sig
  type msg
  val msg_code   : int
  val of_cbuffer : Cbuffer.t -> msg
end

module Status : (Event with type msg := (board_status * status_data)) = struct

  let msg_code = 0x0F40

  let parse_packers data =
    let iter data =
      Cbuffer.iter (fun _ -> Some 4)
                   (fun b -> let bs = Bitstring.bitstring_of_string @@ CCString.rev @@ Cbuffer.to_string b in
                             match%bitstring bs with
                             | {| overflow : 1
                                ; enabled  : 1
                                ; has_data : 1
                                ; br_en    : 1
                                ; bitrate  : 28 : map (fun x -> if br_en then Some (x * 8) else None)
                                |} -> { overflow; enabled; has_data; bitrate })
                   data
    in
    List.rev @@ Cbuffer.fold (fun acc el -> el :: acc) (iter data) []

  let of_cbuffer msg =
    let data = get_status_data msg in
    let phy  = get_status_phy msg in
    let cmd  = get_status_sub_cmd msg in
    let brd  = { phy_ok  = phy land 1 > 0
               ; link_ok = phy land 0x20 > 0
               ; speed   = if phy land 0x06 > 0 then Speed1000
                           else if phy land 0x0A > 0 then Speed100
                           else if phy land 0x12 > 0 then Speed10
                           else Speed_failure
               }
    in
    let data = match cmd with
      | 0 -> General (parse_packers data)
      | _ -> Unknown (Cbuffer.to_string data)
    in
    brd,data

end

(* Message deserialization *)

type err = Bad_prefix           of int
         | Bad_length           of int
         | Bad_msg_code         of int
         | No_prefix_after_msg  of int
         | Insufficient_payload of Cbuffer.t
         | Unknown_err          of string

let string_of_err = function
  | Bad_prefix x            -> "incorrect prefix: " ^ (string_of_int x)
  | Bad_length x            -> "incorrect length: " ^ (string_of_int x)
  | Bad_msg_code x          -> "incorrect code: "   ^ (string_of_int x)
  | No_prefix_after_msg x   -> Printf.sprintf "no prefix found after message with code = %d" x
  | Insufficient_payload _  -> "insufficient payload"
  | Unknown_err s           -> s


let check_prefix buf =
  let prefix' = get_header_prefix buf in
  if prefix <> prefix then Error (Bad_prefix prefix') else Ok buf

let check_msg_code buf =
  let hdr,rest = Cbuffer.split buf sizeof_header in
  let code     = get_header_msg_code hdr in
  let length   = (match code with
                  | x when x = Get_board_info.rsp_code -> Some sizeof_board_info
                  | x when x = Status.msg_code         -> Some sizeof_status
                  | _      -> None) in
  match length with
  | Some x -> Ok (x,code,rest)
  | None   -> Error (Bad_msg_code code)

let check_length (len,code,rest') =
  if len > 512 - sizeof_header
  then Error (Bad_length len)
  else let body,rest = Cbuffer.split rest' len in
       Ok (code,body,rest)

let check_next_prefix ((code,_,rest) as x) =
  if Cbuffer.len rest < sizeof_header
  then Ok x
  else (match check_prefix rest with
        | Ok _    -> Ok x
        | Error _ -> Error (No_prefix_after_msg code))

let get_msg buf =
  try
    CCResult.(check_prefix buf
              >>= check_msg_code
              >>= check_length
              >>= check_next_prefix)
  with
  | Invalid_argument _ -> Error (Insufficient_payload buf)
  | e -> Error (Unknown_err (Printexc.to_string e))

let deserialize buf =
  let parse_msg = fun (code,body) ->
    try
      (match code with
       | x when x = Get_board_info.rsp_code -> `R (`Board_info body)
       | x when x = Status.msg_code         -> `E (`Status (Status.of_cbuffer body))
       | _ -> `N)
    with _ -> `N in
  let rec f events responses b =
    if Cbuffer.len b >= sizeof_header
    then (match get_msg b with
          | Ok (code,body,rest) -> (match parse_msg (code,body) with
                                    | `E x -> f (x::events) responses rest
                                    | `R x -> f events (x::responses) rest
                                    | `N   -> f events responses rest)
          | Error e -> (match e with
                        | Insufficient_payload x -> List.rev events, List.rev responses, x
                        | _                      -> Cbuffer.split b 1 |> fun (_,x) -> f events responses x))
    else (List.rev events, List.rev responses, b) in
  let events,responses,res = f [] [] buf in
  events,responses, if Cbuffer.len res > 0 then Some res else None

let try_parse f x = try Some (f x) with _ -> None

let parse_get_board_info = function
  | `Board_info buf -> try_parse Get_board_info.of_cbuffer buf
  | _ -> None

let is_response (type a) (req : a request) msg : a option =
  match req with
  | Get_board_info -> parse_get_board_info msg
