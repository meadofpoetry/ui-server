open Board_niitv_ts2ip_types
open Application_types

let take (n : int) (l : 'a list) =
  let rec aux i acc = function
    | [] -> List.rev acc
    | _ when i = 0 -> List.rev acc
    | hd :: tl -> aux (pred i) (hd :: acc) tl
  in
  aux n [] l

let to_prefix (msg : Request.msg) =
  let buf = Cstruct.create Message.sizeof_prefix in
  let tag = Request.tag_to_enum msg.tag in
  Message.set_prefix_tag_start buf Message.tag_start;
  Message.set_prefix_msg_code buf tag;
  buf

(* FIXME remove after board protocol update *)
let flip_int16 (x : int) =
  let msb, lsb = (x land 0xFF00) lsr 8, x land 0x00FF in
  (lsb lsl 8) lor msb

(* FIXME remove after board protocol update *)
let flip_ipaddr (x : Ipaddr.V4.t) =
  let msb, lsb = Ipaddr.V4.to_int16 x in
  Ipaddr.V4.of_int16 (flip_int16 lsb, flip_int16 msb)

let serialize_udp_mode (mode : udp_mode) =
  let buf = Cstruct.create Message.sizeof_udp_settings in
  let id =
    Stream.Multi_TS_ID.to_int32_pure
    @@
    match mode.stream with
    | ID x -> x
    | Full {orig_id = TS_multi x; _} -> x
    | Full _ -> Stream.Multi_TS_ID.forbidden
  in
  let mac = Netlib.Ipaddr.V4.multicast_to_mac mode.dst_ip in
  let sock = socket_to_enum mode.socket in
  let chan = (sock lsl 1) lor if mode.enabled then 1 else 0 in
  Message.set_udp_settings_dst_ip buf
  @@ Netlib.Ipaddr.V4.to_int32
  @@ flip_ipaddr mode.dst_ip;
  Message.set_udp_settings_dst_port buf @@ flip_int16 mode.dst_port;
  Message.set_udp_settings_dst_mac (Macaddr.to_octets mac) 0 buf;
  Message.set_udp_settings_self_port buf @@ flip_int16 mode.self_port;
  Message.set_udp_settings_mode buf chan;
  Message.set_udp_settings_stream_id buf id;
  buf

let serialize_mode_main (mode : mode) =
  let pfx = Cstruct.create Message.sizeof_req_mode_main_prefix in
  let sfx = Cstruct.create Message.sizeof_req_mode_main_suffix in
  let bdy =
    Cstruct.concat @@ List.map serialize_udp_mode @@ take Message.n_udp_main mode.udp
  in
  let pad =
    Cstruct.create
    @@ ((Message.n_udp_main * Message.sizeof_udp_settings) - Cstruct.len bdy)
  in
  Message.set_req_mode_main_prefix_cmd pfx 0;
  Ipaddr.(
    Message.set_req_mode_main_prefix_ip pfx @@ V4.to_int32 @@ flip_ipaddr mode.network.ip;
    Message.set_req_mode_main_prefix_mask pfx
    @@ V4.to_int32
    @@ flip_ipaddr mode.network.mask;
    Message.set_req_mode_main_prefix_gateway pfx
    @@ V4.to_int32
    @@ flip_ipaddr mode.network.gateway);
  Cstruct.concat [pfx; bdy; pad; sfx]

let serialize_mode_aux (i : int) (pkrs : udp_mode list) =
  let pfx = Cstruct.create Message.sizeof_req_mode_aux_prefix in
  let sfx = Cstruct.create Message.sizeof_req_mode_aux_suffix in
  let bdy =
    Cstruct.concat @@ List.map serialize_udp_mode @@ take Message.n_udp_aux pkrs
  in
  let pad =
    Cstruct.create
    @@ ((Message.n_udp_aux * Message.sizeof_udp_settings) - Cstruct.len bdy)
  in
  Message.set_req_mode_aux_prefix_cmd pfx i;
  Cstruct.concat [pfx; bdy; pad; sfx]

let to_msg (type a) (t : a Request.t) : Request.msg =
  let tag = Request.to_tag t in
  let data =
    match t with
    | Get_devinfo -> Cstruct.empty
    | Set_mac mac ->
        let data = Cstruct.create Message.sizeof_req_factory_mode in
        let mac = Netlib.Macaddr.to_octets mac in
        Message.set_req_factory_mode_mac mac 0 data;
        data
    | Set_mode_main mode -> serialize_mode_main mode
    | Set_mode_aux_1 pkrs -> serialize_mode_aux 1 pkrs
    | Set_mode_aux_2 pkrs -> serialize_mode_aux 2 pkrs
  in
  {tag; data}

let serialize (type a) (request : a Request.t) : Cstruct.t =
  let msg = to_msg request in
  let pfx = to_prefix msg in
  Cstruct.concat [pfx; msg.data]
