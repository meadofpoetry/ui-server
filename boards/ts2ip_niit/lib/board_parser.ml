open Board_types
open Board_msg_formats

type _ request = Get_board_info : info request
type _ instant_request = Set_board_mode   : settings         -> unit instant_request
                       | Set_factory_mode : factory_settings -> unit instant_request
type api = { set_mode : settings -> unit Lwt.t }

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

module Get_board_info : (Request with type req := unit with type rsp := info) = struct

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

module Set_board_mode : (Instant_request with type req := settings) = struct

  let msg_code  = 0x0088
  let self_port = 2028

  (*
   * let blit_packer i settings offset body =
   *   let dstoff = i * sizeof_packer_settings + offset in
   *   let buf    = Cbuffer.create sizeof_packer_settings in
   *   let mode   = settings.port lsl 1 |> fun x -> if settings.enabled then x lor 1 else x in
   *   let ()     = Ipaddr.V4.to_int32 settings.dst_ip |> set_packer_settings_dst_ip buf in
   *   let ()     = settings.dst_port                  |> set_packer_settings_dst_port buf in
   *   let ()     = Ipaddr.V4.multicast_to_mac settings.dst_ip |> Macaddr.to_string
   *                |> fun mac -> set_packer_settings_dst_mac mac 0 buf in
   *   let ()     = set_packer_settings_self_port buf self_port in
   *   let ()     = set_packer_settings_mode buf mode in
   *   (\* let ()     = set_packer_settings_stream_id buf @@ Common.Stream.to_int32 settings.stream_id in *\)
   *   Cbuffer.blit buf 0 body dstoff sizeof_packer_settings
   * 
   * let pack_main s p =
   *   let body = Cbuffer.create (sizeof_req_settings_main + (4 * sizeof_packer_settings)) in
   *   let ()   = set_req_settings_main_cmd body 0 in
   *   let ()   = Ipaddr.V4.to_int32 s.ip      |> set_req_settings_main_ip body in
   *   let ()   = Ipaddr.V4.to_int32 s.mask    |> set_req_settings_main_mask body in
   *   let ()   = Ipaddr.V4.to_int32 s.gateway |> set_req_settings_main_gateway body in
   *   let ()   = List.iteri (fun i x -> blit_packer i x sizeof_req_settings_main body) p in
   *   to_msg ~msg_code:0x0088 ~body ()
   * 
   * let to_cbuffer (settings:settings) =
   *   let ppm         = 6 in (\* packers per message *\)
   *   let max_packers = ppm * 3 in
   *   let req_num     = List.length settings.packers - 4
   *                     |> (fun x -> if x > max_packers then max_packers else if x < 0 then 0 else x)
   *                     |> (fun x -> ( x / ppm ) + (if ( x mod ppm ) > 0 then 1 else 0))
   *                     |> (fun x -> x + 1) in
   *   let rec pack = (fun acc req_id l ->
   *       (match req_id with
   *        | 0 -> acc
   *        | x when x = req_num -> let (_,tl) = CCList.take_drop 4 l in
   *                                let msg     = pack_main settings @@ CCList.take 4 l in
   *                                pack (msg::acc) (req_id - 1) tl
   *        | _ -> let body    = Cbuffer.create (sizeof_req_settings_packers + (ppm * sizeof_packer_settings)) in
   *               let (hd,tl) = CCList.take_drop ppm l in
   *               let ()      = set_req_settings_packers_cmd body req_id in
   *               let ()      = List.iteri (fun i x -> blit_packer i x sizeof_req_settings_packers body) hd in
   *               let msg     = to_msg ~msg_code:0x0088 ~body () in
   *               pack (msg::acc) (req_id - 1) tl)) in
   *   Cbuffer.concat @@ List.rev (pack [] req_num settings.packers) **)

  let to_cbuffer (s:settings) =
    let body = Cbuffer.create sizeof_req_settings_main in
    let ()   = set_req_settings_main_cmd body 0 in
    let ()   = Ipaddr.V4.to_int32 s.ip      |> set_req_settings_main_ip body in
    let ()   = Ipaddr.V4.to_int32 s.mask    |> set_req_settings_main_mask body in
    let ()   = Ipaddr.V4.to_int32 s.gateway |> set_req_settings_main_gateway body in
    let pkrs =
      CCList.map (fun (ps:packer_settings) ->
          let buf  = Cbuffer.create sizeof_packer_settings in
          let mode = (ps.port lsl 1) |> (fun x -> if ps.enabled then x lor 1 else x) in
          let ip   = Ipaddr.V4.to_bytes ps.dst_ip |> CCString.rev |> Ipaddr.V4.of_bytes_exn in
          let ()   = Ipaddr.V4.to_int32 ip |> set_packer_settings_dst_ip buf in
          let ()   = ps.dst_port |> set_packer_settings_dst_port buf in
          let ()   = Ipaddr.V4.multicast_to_mac ps.dst_ip |> Macaddr.to_bytes
                     |> fun mac -> set_packer_settings_dst_mac mac 0 buf in
          let ()   = set_packer_settings_self_port buf self_port in
          let ()   = set_packer_settings_mode buf mode in
          buf) @@ CCList.take 4 s.packers in
    let body = Cbuffer.concat @@ body :: pkrs in
    let len  = sizeof_req_settings_main + (4 * sizeof_packer_settings) in
    let body = Cbuffer.append body @@ Cbuffer.create (len - Cbuffer.len body) in
    to_msg ~msg_code ~body ()
    |> fun x -> print_endline @@ string_of_int @@ Cbuffer.len x;
                print_endline @@ Cbuffer.hexdump_to_string x;
                x

end

(* Events  *)

module type Event = sig
  type msg
  val msg_code   : int
  val of_cbuffer : Cbuffer.t -> msg
end

module Status : (Event with type msg := status) = struct

  let msg_code = 0x0F40

  let parse_packers data =
    Cbuffer.split_size 4 data
    |> List.filter (fun x -> Cbuffer.len x > 0)
    |> List.map (fun x -> let b        = Cbuffer.LE.get_uint32 x 0 in
                          { bitrate  = (if (Int32.logand b 0x10000000l) = 0l then None
                                        else Some (((Int32.to_int b) land 0x0FFFFFFF) * 8))
                          ; enabled  = (Int32.logand b 0x40000000l) <> 0l
                          ; overflow = (Int32.logand b 0x80000000l) <> 0l
                })
    (* let iter data =
     *   Cbuffer.iter (fun _ -> Some 4)
     *                (fun b -> let bs = Bitstring.bitstring_of_string @@ Cbuffer.to_string b in
     *                          print_endline @@ Cbuffer.hexdump_to_string b;
     *                          match%bitstring bs with
     *                          | {| overflow : 1
     *                             ; enabled  : 1
     *                             ; rfu      : 1
     *                             ; br_ready : 1
     *                             ; bitrate  : 27 : map (fun x -> if br_ready then Some (x * 8) else None)
     *                             |} -> { overflow; enabled; bitrate })
     *                data
     * in
     * print_endline "";
     * List.rev @@ Cbuffer.fold (fun acc el -> el :: acc) (iter data) [] *)
  let of_cbuffer msg =
    let data = get_status_data msg in
    let phy  = get_status_phy msg in
    let cmd  = get_status_sub_cmd msg in
    { phy_ok  = phy land 1 > 0
    ; link_ok = phy land 0x20 > 0
    ; speed   = if phy land 0x06 > 0 then Speed1000
                else if phy land 0x0A > 0 then Speed100
                else if phy land 0x12 > 0 then Speed10
                else Speed_failure
    ; data    = match cmd with
                | 0 -> General (parse_packers data)
                | _ -> Unknown (Cbuffer.to_string data)
    }

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
       | _      -> `N)
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
