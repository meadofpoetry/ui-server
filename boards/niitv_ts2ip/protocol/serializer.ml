open Board_niitv_ts2ip_types
open Application_types

let to_prefix (msg : Request.msg) =
  let buf = Cstruct.create Message.sizeof_prefix in
  let tag = Request.req_tag_to_enum msg.tag in
  Message.set_prefix_tag_start buf Message.tag_start;
  Message.set_prefix_msg_code buf tag;
  buf

let serialize_udp_mode (mode : udp_mode) =
  let buf = Cstruct.create Message.sizeof_udp_settings in
  let id = Stream.Multi_TS_ID.to_int32_raw (Stream.to_multi_id mode.stream) in
  let mac = Netlib.Ipaddr.V4.multicast_to_mac mode.dst_ip in
  let sock = socket_to_enum mode.socket in
  let chan = (sock lsl 1) lor (if mode.enabled then 1 else 0) in
  Message.set_udp_settings_dst_ip buf @@ Netlib.Ipaddr.V4.to_int32 mode.dst_ip;
  Message.set_udp_settings_dst_port buf mode.dst_port;
  Message.set_udp_settings_dst_mac (Macaddr.to_bytes mac) 0 buf;
  Message.set_udp_settings_self_port buf mode.self_port;
  Message.set_udp_settings_mode buf chan;
  Message.set_udp_settings_stream_id buf id;
  buf

let serialize_mode_main (mode : mode) =
  if List.length mode.udp > Message.n_udp_main
  then (
    let err =
      Printf.sprintf "number of packers can't exceed %d"
        Message.n_udp_main in
    raise (Invalid_argument err));
  let buf = Cstruct.create Message.sizeof_req_mode_main in
  Message.set_req_mode_main_cmd buf 0;
  Message.set_req_mode_main_ip buf @@ Netlib.Ipaddr.V4.to_int32 mode.ip;
  Message.set_req_mode_main_mask buf @@ Netlib.Ipaddr.V4.to_int32 mode.mask;
  Message.set_req_mode_main_gateway buf @@ Netlib.Ipaddr.V4.to_int32 mode.gateway;
  List.iteri (fun (i : int) (pkr : udp_mode) ->
      let bytes = serialize_udp_mode pkr in
      let len = Cstruct.len bytes in
      Cstruct.blit bytes 0 buf (i * len) len) mode.udp;
  buf

let serialize_mode_aux (i : int) (pkrs : udp_mode list) =
  if List.length pkrs > Message.n_udp_aux
  then (
    let err =
      Printf.sprintf "number of packers can't exceed %d"
        Message.n_udp_aux in
    raise (Invalid_argument err));
  let buf = Cstruct.create Message.sizeof_req_mode_aux in
  Message.set_req_mode_aux_cmd buf i;
  List.iteri (fun (i : int) (pkr : udp_mode) ->
      let bytes = serialize_udp_mode pkr in
      let len = Cstruct.len bytes in
      Cstruct.blit bytes 0 buf (i * len) len) pkrs;
  buf

let to_msg (type a) (t : a Request.t) : Request.msg =
  let tag = Request.to_tag t in
  let data = match t with
    | Get_devinfo -> Cstruct.empty
    | Set_mac mac ->
      let data = Cstruct.create Message.sizeof_req_factory_mode in
      let mac = Netlib.Macaddr.to_bytes mac in
      Message.set_req_factory_mode_mac mac 0 data;
      data
    | Set_mode_main mode -> serialize_mode_main mode
    | Set_mode_aux_1 pkrs -> serialize_mode_aux 1 pkrs
    | Set_mode_aux_2 pkrs -> serialize_mode_aux 2 pkrs
  in
  { tag; data }

let make_req (type a) (request : a Request.t) : Cstruct.t =
  let msg = to_msg request in
  let pfx = to_prefix msg in
  Cstruct.concat [pfx; msg.data]
