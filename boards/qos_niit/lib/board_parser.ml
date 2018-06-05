open Containers

let prefix = 0x55AA

include Board_msg_formats

open Board_types
open Types
open Common.Dvb_t2_types

type part =
  { first : bool
  ; param : int32
  ; data  : Cbuffer.t
  }

type jitter_req =
  { request_id : int
  ; pointer    : int32
  }

type t2mi_info_req =
  { request_id : int
  ; stream_id  : int
  }

type t2mi_frame_seq_req =
  { request_id : int
  ; seconds    : int
  }

type section_req =
  { request_id : int
  ; params     : section_params
  }
and section_params =
  { stream_id      : Common.Stream.id
  ; table_id       : int
  ; section        : int option (* needed for tables containing multiple sections *)
  ; table_id_ext   : int option (* needed for tables with extra parameter, like ts id for PAT *)
  ; eit_ts_id      : int option (* ts id for EIT *)
  ; eit_orig_nw_id : int option (* original network ID for EIT *)
  }

type probe_response =
  | Board_errors of board_errors
  | Bitrate      of Types.bitrate list
  | Struct       of Streams.TS.structures
  | T2mi_info    of Streams.T2MI.structure
  | Jitter       of Types.jitter_raw

type _ instant_request =
  | Set_board_mode  : Types.mode         -> unit instant_request
  | Set_jitter_mode : jitter_mode option -> unit instant_request
  | Reset           : unit instant_request

type _ probe_request =
  | Get_board_errors : int           -> probe_response probe_request
  | Get_jitter       : jitter_req    -> probe_response probe_request
  | Get_ts_structs   : int           -> probe_response probe_request
  | Get_bitrates     : int           -> probe_response probe_request
  | Get_t2mi_info    : t2mi_info_req -> probe_response probe_request

type _ request =
  | Get_board_info     : devinfo request
  | Get_board_mode     : Types.mode request
  | Get_t2mi_frame_seq : t2mi_frame_seq_req -> Streams.T2MI.sequence request
  | Get_section        : section_req -> (Streams.TS.section,Streams.TS.section_error) result request

(* ------------------- Misc ------------------- *)

let input_to_int = function
  | SPI -> 0 | ASI -> 1
let input_of_int = function
  | 0 -> Some SPI | 1 -> Some ASI | _ -> None

let int_to_bool_list x = List.map (fun i -> (x land Int.pow 2 i) > 0) (List.range 0 7)

let int_to_t2mi_sync_list x = int_to_bool_list x
                              |> List.foldi (fun acc i x -> if x then i :: acc else acc) []

(* -------------------- Message constructors ------------------*)

let to_common_header ~msg_code () =
  let hdr = Cbuffer.create sizeof_common_header in
  let () = set_common_header_prefix hdr prefix in
  let () = set_common_header_msg_code hdr msg_code in
  hdr

let to_complex_req_header ?(client_id=0) ?(request_id=0) ~msg_code ~length () =
  let hdr = to_common_header ~msg_code () in
  let complex_hdr = Cbuffer.create sizeof_complex_req_header in
  let () = set_complex_req_header_client_id complex_hdr client_id in
  let () = set_complex_req_header_length complex_hdr length in
  let () = set_complex_req_header_request_id complex_hdr request_id in
  Cbuffer.append hdr complex_hdr

let to_simple_req ~msg_code ~body () =
  let hdr = to_common_header ~msg_code () in
  Cbuffer.append hdr body

let to_complex_req ?client_id ?request_id ~msg_code ~body () =
  let length = (Cbuffer.len body / 2) + 1 in
  let hdr = to_complex_req_header ?client_id ?request_id ~msg_code ~length () in
  Cbuffer.append hdr body

let to_mode_exn mode t2mi_pid stream_id : Types.mode =
  { input = Option.get_exn @@ input_of_int (mode land 1)
  ; t2mi  = Some { enabled        = if (mode land 4) > 0 then true else false
                 ; pid            = t2mi_pid land 0x1fff
                 ; t2mi_stream_id = (t2mi_pid lsr 13) land 0x7
                 ; stream         = Common.Stream.id_of_int32 stream_id
                 }
  }

(* -------------------- Requests/responses/events ------------------*)

module type Request = sig

  type req
  type rsp

  val req_code   : int
  val rsp_code   : int
  val to_cbuffer : req -> Cbuffer.t
  val of_cbuffer : Cbuffer.t -> rsp

end

module type Event = sig

  type msg

  val msg_code : int
  val of_cbuffer : Cbuffer.t -> msg

end

module Get_board_info : (Request with type req := unit with type rsp := devinfo) = struct

  let req_code = 0x0080
  let rsp_code = 0x01
  let to_cbuffer _ = to_common_header ~msg_code:req_code ()
  let of_cbuffer msg =
    { typ = get_board_info_board_type msg
    ; ver = get_board_info_board_version msg
    }

end

module Get_board_mode : (Request with type req := unit with type rsp := Types.mode) = struct

  let req_code = 0x0081
  let rsp_code = 0x02
  let to_cbuffer _   = to_common_header ~msg_code:req_code ()
  let of_cbuffer msg = to_mode_exn (get_board_mode_mode msg)
                                   (get_board_mode_t2mi_pid msg)
                                   (get_board_mode_t2mi_stream_id msg)

end

module Get_board_errors : (Request with type req := int with type rsp := board_errors) = struct

  let req_code = 0x0110
  let rsp_code = req_code
  let to_cbuffer request_id = to_complex_req ~request_id ~msg_code:req_code ~body:(Cbuffer.create 0) ()
  let of_cbuffer msg =
    let timestamp = Common.Time.Clock.now () in
    let iter      = Cbuffer.iter (fun _ -> Some sizeof_t2mi_frame_seq_item)
                                 (fun buf -> Cbuffer.LE.get_uint32 buf 0)
                                 (get_board_errors_errors msg) in
    List.rev @@ Cbuffer.fold (fun acc el -> el :: acc) iter []
    |> List.foldi (fun acc i x ->
           let count = Int32.to_int x in
           if count <> 0 && i >= 0 && i <= 16 then acc
           else { timestamp; err_code = i; count } :: acc) []

end

module Get_section : (Request
                      with type req = section_req
                      with type rsp = (Streams.TS.section,Streams.TS.section_error) result) = struct

  open Streams.TS

  type req = section_req
  type rsp = (section,section_error) result

  let req_code = 0x0302
  let rsp_code = req_code

  let to_cbuffer { request_id;params } =
    let body = Cbuffer.create sizeof_req_get_section in
    let ()   = set_req_get_section_stream_id body @@ Common.Stream.id_to_int32 params.stream_id in
    let ()   = Option.iter (set_req_get_section_section body) params.section in
    let ()   = Option.iter (set_req_get_section_table_id_ext body) params.table_id_ext in
    let ()   = Option.iter (set_req_get_section_adv_info_1 body) params.eit_ts_id in
    let ()   = Option.iter (set_req_get_section_adv_info_2 body) params.eit_orig_nw_id in
    to_complex_req ~request_id ~msg_code:req_code ~body ()

  let of_cbuffer msg =
    let hdr,bdy   = Cbuffer.split msg sizeof_section in
    let length    = get_section_length hdr in
    let result    = get_section_result hdr in
    if length > 0 && result = 0
    then let sid,data  = Cbuffer.split bdy 4 in
         let stream_id = Common.Stream.id_of_int32 @@ Cbuffer.LE.get_uint32 sid 0 in
         Ok { section = Cbuffer.to_string data
            ; stream_id
            ; table_id = 0
            }
    else (Error (match result with
                 | 0 | 3 -> Zero_length
                 | 1     -> Table_not_found
                 | 2     -> Section_not_found
                 | 4     -> Stream_not_found
                 | _     -> Unknown))

end

module Get_t2mi_frame_seq : (Request
                             with type req := t2mi_frame_seq_req
                             with type rsp = Streams.T2MI.sequence) = struct
  open Streams.T2MI

  type rsp = sequence

  let req_code = 0x0306
  let rsp_code = req_code

  let to_cbuffer { seconds; request_id } =
    let body = Cbuffer.create sizeof_req_get_t2mi_frame_seq in
    let ()   = set_req_get_t2mi_frame_seq_time body seconds in
    to_complex_req ~request_id:request_id ~msg_code:req_code ~body ()

  let of_cbuffer msg =
    let iter = Cbuffer.iter (fun _ -> Some sizeof_t2mi_frame_seq_item) (fun buf -> buf) msg in
    Cbuffer.fold (fun (acc : sequence) el ->
        let sframe_stream = get_t2mi_frame_seq_item_sframe_stream el in
        { typ         = get_t2mi_frame_seq_item_typ el
        ; super_frame = (sframe_stream land 0xF0) lsr 4
        ; stream_id   = sframe_stream land 0x07
        ; frame       = get_t2mi_frame_seq_item_frame el
        ; count       = Int32.to_int @@ get_t2mi_frame_seq_item_count el
        ; plp         = get_t2mi_frame_seq_item_plp el
        ; l1_param_1  = get_t2mi_frame_seq_item_dyn1_frame el
        ; l1_param_2  = get_t2mi_frame_seq_item_dyn2_frame el
        ; ts_packet   = Int32.to_int @@ get_t2mi_frame_seq_item_time el
        } :: acc)
                 iter []
    |> List.rev

end

module Get_jitter : (Request with type req := jitter_req with type rsp := Types.jitter_raw) = struct

  let req_code = 0x0307
  let rsp_code = req_code

  let to_cbuffer { request_id; pointer } =
    let body = Cbuffer.create sizeof_req_get_jitter in
    let ()   = set_req_get_jitter_ptr body pointer in
    to_complex_req ~request_id ~msg_code:req_code ~body ()

  let of_cbuffer_item el packet_time : Jitter.measure =
    let status = get_jitter_item_status el in
    let d_pack = get_jitter_item_d_packet el in
    let d_pcr  = get_jitter_item_d_pcr el in
    let drift  = get_jitter_item_drift el in
    let fo     = get_jitter_item_fo el in
    let jitter = get_jitter_item_jitter el in
    let t_pcr    = ((Int32.to_float d_pcr) *. 10e+9) /. 27e+6 in
    let t_pcr_br = (Int32.to_float packet_time) *. (float_of_int d_pack) in
    let accuracy = t_pcr /. t_pcr_br in
    { discont_err = status land 0x8000 <> 0
    ; discont_ok  = status land 0x4000 <> 0
    ; t_pcr       = t_pcr_br
    ; accuracy
    ; jitter
    ; drift       = Int32.float_of_bits drift
    ; fo          = Int32.float_of_bits fo
    ; period      = t_pcr_br /. 10e+6
    }

  let of_cbuffer msg : Types.jitter_raw =
    let hdr,bdy'    = Cbuffer.split msg sizeof_jitter in
    let count       = get_jitter_count hdr in
    let bdy,_       = Cbuffer.split bdy' @@ sizeof_jitter_item * count in
    let pid         = get_jitter_pid hdr in
    let t_pcr       = Int32.float_of_bits @@ get_jitter_t_pcr hdr in
    let time        = Int32.to_int @@ get_jitter_time hdr in
    let next_ptr    = get_jitter_req_next hdr in
    let packet_time = get_jitter_packet_time hdr in
    let iter        = Cbuffer.iter (fun _ -> Some sizeof_jitter_item) (fun buf -> buf) bdy in
    let timestamp   = Common.Time.Clock.now () in
    let measures    = List.rev @@ Cbuffer.fold (fun acc el -> (of_cbuffer_item el packet_time) :: acc)
                                               iter [] in
    { measures; next_ptr; time; timestamp; pid; t_pcr }

end

module Get_ts_structs : (Request with type req := int
                                  and type rsp := Streams.TS.structures) = struct

  open Streams.TS

  let req_code = 0x0309
  let rsp_code = req_code

  let to_cbuffer request_id =
    let body = Cbuffer.create sizeof_req_get_ts_struct in
    let ()   = set_req_get_ts_struct_stream_id body 0xFFFFFFFFl in
    to_complex_req ~request_id ~msg_code:req_code ~body ()

  let of_general_struct_block msg =
    let bdy,rest   = Cbuffer.split msg sizeof_general_struct_block in
    let string_len = get_general_struct_block_string_len bdy in
    let nw_pid'    = get_general_struct_block_network_pid bdy in
    let strings,_  = Cbuffer.split rest (string_len * 2) in
    let nw_name,bq_name  = Cbuffer.split strings string_len in
    { services_num = get_general_struct_block_services_num bdy
    ; nw_pid       = nw_pid' land 0x1FFF
    ; complete     = nw_pid' land 0x4000 <> 0
    ; ts_id        = get_general_struct_block_ts_id bdy
    ; nw_id        = get_general_struct_block_nw_id bdy
    ; orig_nw_id   = get_general_struct_block_orig_nw_id bdy
    ; nw_name      = Text_decoder.get_encoding_and_convert nw_name
    ; bouquet_name = Text_decoder.get_encoding_and_convert bq_name
    }, string_len

  let of_pids_struct_block msg =
    let iter = Cbuffer.iter (fun _ -> Some 2) (fun buf -> Cbuffer.LE.get_uint16 buf 0) msg in
    List.rev @@ Cbuffer.fold (fun acc el -> { pid       = el land 0x1FFF
                                            ; bitrate   = None
                                            ; has_pts   = el land 0x8000 <> 0
                                            ; scrambled = el land 0x4000 <> 0
                                            ; present   = el land 0x2000 <> 0 } :: acc) iter []

  let of_services_struct_block string_len msg =
    let bdy,rest   = Cbuffer.split msg sizeof_services_struct_block in
    let flags      = get_services_struct_block_flags bdy in
    let strings,_  = Cbuffer.split rest (string_len * 2) in
    let sn,pn      = Cbuffer.split strings string_len in
    { id             = get_services_struct_block_id bdy
    ; bitrate        = None
    ; name           = Text_decoder.get_encoding_and_convert sn
    ; provider_name  = Text_decoder.get_encoding_and_convert pn
    ; pmt_pid        = get_services_struct_block_pmt_pid bdy
    ; pcr_pid        = get_services_struct_block_pcr_pid bdy
    ; has_pmt        = flags land 0x8000 <> 0
    ; has_sdt        = flags land 0x4000 <> 0
    ; dscr           = flags land 0x2000 <> 0
    ; list_dscr      = flags land 0x1000 <> 0
    ; eit_schedule   = flags land 0x0080 <> 0
    ; eit_pf         = flags land 0x0040 <> 0
    ; free_ca_mode   = flags land 0x0020 <> 0
    ; running_status = flags land 0x0007
    ; service_type_1 = get_services_struct_block_service_type_1 bdy
    ; service_type_2 = get_services_struct_block_service_type_2 bdy
    ; es             = []
    ; ecm            = []
    }

  let of_es_struct_block msg =
    let iter = Cbuffer.iter (fun _ -> Some 4) (fun buf -> buf) msg in
    List.rev @@ Cbuffer.fold (fun acc x -> let pid' = get_es_struct_block_pid x in
                                           { pid          = pid' land 0x1FFF
                                           ; bitrate      = None
                                           ; has_pts      = pid' land 0x8000 > 0
                                           ; es_type      = get_es_struct_block_es_type x
                                           ; es_stream_id = get_es_struct_block_es_stream_id x
                                           } :: acc) iter []

  let of_ecm_struct_block msg =
    let iter = Cbuffer.iter (fun _ -> Some 4) (fun buf -> buf) msg in
    List.rev @@ Cbuffer.fold (fun acc x -> { pid       = get_ecm_struct_block_pid x land 0x1FFF
                                           ; bitrate   = None
                                           ; ca_sys_id = get_ecm_struct_block_ca_system_id x} :: acc) iter []

  let of_table_struct_block msg =
    let bdy,rest = Cbuffer.split msg sizeof_table_struct_block in
    let iter     = Cbuffer.iter (fun _ -> Some 2)
                     (fun buf -> Cbuffer.LE.get_uint16 buf 0) rest in
    let sections = Cbuffer.fold (fun acc x -> { id = List.length acc
                                              ; analyzed = x land 0x8000 > 0
                                              ; length   = x land 0x0FFF } :: acc) iter []
                   |> List.filter (fun x -> x.length > 0)
                   |> List.rev in
    let pid'   = get_table_struct_block_pid msg in
    let id     = get_table_struct_block_id msg in
    let id_ext = get_table_struct_block_id_ext bdy in
    let common =
      { version        = get_table_struct_block_version bdy
      ; bitrate        = None
      ; id             = get_table_struct_block_id bdy
      ; pid            = pid' land 0x1FFF
      ; lsn            = get_table_struct_block_lsn bdy
      ; section_syntax = pid' land 0x8000 > 0
      ; sections
      } in
    let params =
      { ts_id         = get_table_struct_block_adv_info_1 bdy
      ; orig_nw_id    = get_table_struct_block_adv_info_2 bdy
      ; segment_lsn   = get_table_struct_block_adv_info_3 bdy
      ; last_table_id = get_table_struct_block_adv_info_4 bdy
      }  in
    (match table_label_of_int id with
     | `PAT   -> PAT  { common; ts_id = id_ext }
     | `CAT   -> CAT  common
     | `PMT   -> PMT  { common; program_number = id_ext }
     | `TSDT  -> TSDT common
     | `NITa  -> NIT  { common; nw_id = id_ext; ts = Actual }
     | `NITo  -> NIT  { common; nw_id = id_ext; ts = Other }
     | `SDTa  -> SDT  { common; ts_id = id_ext; ts = Actual }
     | `SDTo  -> SDT  { common; ts_id = id_ext; ts = Other }
     | `BAT   -> BAT  { common; bouquet_id = id_ext }
     | `EITap -> EIT  { common; service_id = id_ext; params; ts = Actual; typ = Present }
     | `EITop -> EIT  { common; service_id = id_ext; params; ts = Other ; typ = Present }
     | `EITas -> EIT  { common; service_id = id_ext; params; ts = Actual; typ = Schedule }
     | `EITos -> EIT  { common; service_id = id_ext; params; ts = Other ; typ = Schedule }
     | `TDT   -> TDT  common
     | `RST   -> RST  common
     | `ST    -> ST   common
     | `TOT   -> TOT  common
     | `DIT   -> DIT  common
     | `SIT   -> SIT  common
     | _      -> Unknown common)

  let of_ts_struct_blocks msg =
    let str_len = ref 0 in
    let rec aux msg acc =
      match Cbuffer.len msg with
      | 0 -> acc
      | _ -> let hdr,data   = Cbuffer.split msg sizeof_struct_block_header in
             let typ        = get_struct_block_header_code hdr in
             let len        = get_struct_block_header_length hdr in
             let block,rest = Cbuffer.split data len in
             (match typ with
              | 0x2000 -> let gen,len = of_general_struct_block block in
                          str_len := len; `General gen
              | 0x2100 -> `Pids (of_pids_struct_block block)
              | 0x2200 -> `Services (of_services_struct_block !str_len block)
              | 0x2201 -> `Es (of_es_struct_block block)
              | 0x2202 -> `Ecm (of_ecm_struct_block block)
              | 0x2300 -> `Emm (of_ecm_struct_block block)
              | 0x2400 -> `Tables (of_table_struct_block block)
              | _      -> `Unknown)
             |> (function
                 | `Es es -> (match acc with
                              | [] -> failwith "of_ts_struct_blocks: no blocks before es block"
                              | hd::tl ->
                                 (match hd with
                                  | `Services s -> `Services ({ s with es = es }) :: tl
                                                   |> aux rest
                                  | _ -> failwith "of_ts_struct_blocks: no services block before es block"))
                 | `Ecm ecm -> (match acc with
                                | [] -> failwith "of_ts_struct_blocks: no blocks before ecm block"
                                | hd::tl ->
                                   (match hd with
                                    | `Services s -> `Services ({ s with ecm = ecm }) :: tl
                                                     |> aux rest
                                    | _ -> failwith "of_ts_struct_blocks: no services block before ecm block"))
                 | x -> aux rest (x :: acc))
    in
    aux msg []

  let of_ts_struct msg : structure * Cbuffer.t option =
    let open Option in
    let hdr,rest = Cbuffer.split msg sizeof_ts_struct in
    let len      = (Int32.to_int @@ get_ts_struct_length hdr) in
    let bdy,rest = Cbuffer.split rest len in
    let blocks   = of_ts_struct_blocks bdy in
    { stream    = Common.Stream.id_of_int32 @@ get_ts_struct_stream_id hdr
    ; bitrate   = None
    ; general   = get_exn @@ List.find_map (function `General x -> Some x | _ -> None) blocks
    ; pids      = get_exn @@ List.find_map (function `Pids x -> Some x | _ -> None) blocks
    ; services  = List.filter_map (function `Services x -> Some x | _ -> None) blocks
    ; emm       = get_exn @@ List.find_map (function `Emm x -> Some x | _ -> None) blocks
    ; tables    = List.filter_map (function `Tables x -> Some x | _ -> None) blocks
    ; timestamp = Common.Time.Clock.now ()
    }, if Cbuffer.len rest > 0 then Some rest else None

  let of_cbuffer msg : structures =
    let hdr,bdy'  = Cbuffer.split msg sizeof_ts_structs in
    let count     = get_ts_structs_count hdr in
    let _,bdy     = Cbuffer.split bdy' (count * 4) in
    let rec parse = (fun acc buf -> let x,rest = of_ts_struct buf in
                                    match rest with
                                    | Some b -> parse (x :: acc) b
                                    | None   -> List.rev (x :: acc)) in
    if count > 0 then parse [] bdy else []

end

module Get_bitrates : (Request with type req := int with type rsp = Types.bitrate list) = struct

  open Types

  type rsp = bitrate list

  let req_code = 0x030A
  let rsp_code = req_code

  let to_cbuffer request_id = to_complex_req ~request_id ~msg_code:req_code ~body:(Cbuffer.create 0) ()

  let of_pids_bitrate total_pids br_per_pkt buf =
    let msg,rest = Cbuffer.split buf (sizeof_pid_bitrate * total_pids) in
    let iter     = Cbuffer.iter (fun _ -> Some sizeof_pid_bitrate) (fun buf -> buf) msg in
    let pids     = (Cbuffer.fold (fun acc el ->
                        let packets = get_pid_bitrate_packets el in
                        { pid     = get_pid_bitrate_pid el land 0x1FFF
                        ; bitrate = int_of_float @@ br_per_pkt *. (Int32.to_float packets) } :: acc)
                                 iter []) in
    List.rev pids, rest

  let of_tbls_bitrate total_tbls br_per_pkt buf =
    let msg,_ = Cbuffer.split buf (sizeof_table_bitrate * total_tbls) in
    let iter  = Cbuffer.iter (fun _ -> Some sizeof_table_bitrate) (fun buf -> buf) msg in
    Cbuffer.fold (fun acc el -> let packets    = get_table_bitrate_packets el in
                                let flags      = get_table_bitrate_flags el in
                                let adv_info_1 = get_table_bitrate_adv_info_1 el in
                                let adv_info_2 = get_table_bitrate_adv_info_2 el in
                                { id             = get_table_bitrate_table_id el
                                ; id_ext         = get_table_bitrate_table_id_ext el
                                ; fully_analyzed = flags land 2 > 0
                                ; section_syntax = flags land 1 > 0
                                ; eit_info       = Some (adv_info_1, adv_info_2)
                                ; bitrate        = int_of_float @@ br_per_pkt *. (Int32.to_float packets) } :: acc)
                 iter []
    |> List.rev

  let of_stream_bitrate timestamp buf =
    let length     = (Int32.to_int @@ get_stream_bitrate_length buf) in
    let msg,rest   = Cbuffer.split buf (length + 8) in
    let hdr,bdy    = Cbuffer.split msg sizeof_stream_bitrate in
    let ts_bitrate = Int32.to_int @@ get_stream_bitrate_ts_bitrate hdr in
    let total_pkts = get_stream_bitrate_total_packets hdr in
    let br_per_pkt = (float_of_int ts_bitrate) /. (Int32.to_float total_pkts)  in
    let total_pids = get_stream_bitrate_total_pids hdr in
    let total_tbls = get_stream_bitrate_total_tables hdr in
    let pids,tbls  = of_pids_bitrate total_pids br_per_pkt bdy in
    let tables     = of_tbls_bitrate total_tbls br_per_pkt tbls in
    let stream     = Common.Stream.id_of_int32 @@ get_stream_bitrate_stream_id hdr in
    { stream; ts_bitrate; pids; tables; timestamp },
    if Cbuffer.len rest > 0 then Some rest else None

  let of_cbuffer msg =
    let hdr,bdy   = Cbuffer.split msg sizeof_bitrates in
    let count     = get_bitrates_count hdr in
    let timestamp = Common.Time.Clock.now () in
    let rec parse = (fun acc buf -> let x,rest = of_stream_bitrate timestamp buf in
                                    match rest with
                                    | Some b -> parse (x :: acc) b
                                    | None   -> List.rev (x :: acc)) in
    if count > 0 then parse [] bdy else []

end

module Get_t2mi_info : (Request with type req := t2mi_info_req
                                 and type rsp := Streams.T2MI.structure) = struct

  open Streams.T2MI

  let req_code = 0x030B
  let rsp_code = req_code

  let to_cbuffer ({ request_id; stream_id } : t2mi_info_req) =
    let body = Cbuffer.create sizeof_req_get_t2mi_info in
    let ()   = set_req_get_t2mi_info_stream_id body stream_id in
    to_complex_req ~request_id ~msg_code:req_code ~body ()

  let of_cbuffer msg =
    let hdr,rest  = Cbuffer.split msg sizeof_t2mi_info in
    let iter      = Cbuffer.iter (fun _ -> Some 1)
                                 (fun buf -> Cbuffer.get_uint8 buf 0)
                                 (get_t2mi_info_packets hdr) in
    let packets   = Cbuffer.fold (fun acc el -> (List.rev @@ int_to_bool_list el) @ acc) iter []
                    |> List.rev
                    |> List.foldi (fun acc i x -> if x then i :: acc else acc) []
    in
    let sid       = get_t2mi_info_stream_id hdr in
    let length    = get_t2mi_info_length hdr in
    let timestamp = Common.Time.Clock.now () in
    match length with
    | 0 -> { packets; timestamp; stream_id = sid; t2mi_pid = None; l1_pre = None; l1_post_conf = None }
    | l -> let body,_   = Cbuffer.split rest l in
           let conf_len = get_t2mi_info_ext_conf_len body
                          |> fun x -> let r,d = x mod 8, x / 8 in d + (if r > 0 then 1 else 0)
           in
           let _,conf   = Cbuffer.split body sizeof_t2mi_info_ext in
           let conf,_   = Cbuffer.split conf conf_len in
           let l1_pre   = Cbuffer.to_string @@ get_t2mi_info_ext_l1_pre body in
           let l1_post  = Cbuffer.to_string conf in
           { packets
           ; timestamp
           ; t2mi_pid     = Some (get_t2mi_info_ext_t2mi_pid body)
           ; stream_id    = sid
           ; l1_pre       = Some l1_pre
           ; l1_post_conf = Some l1_post
           }

end

(* ---------------------- Events --------------------- *)

module Status : (Event with type msg := status_raw) = struct

  let msg_code = 0x03

  let of_cbuffer msg : status_raw =
    let timestamp = Common.Time.Clock.now () in
    let iter      = fun x -> Cbuffer.iter (fun _ -> Some 1) (fun buf -> Cbuffer.get_uint8 buf 0) x in
    let flags     = get_status_flags msg in
    let has_sync  = not (flags land 0x04 > 0) in
    let ts_num    = get_status_ts_num msg in
    let flags2    = get_status_flags_2 msg in
    let jpid      = get_status_jitter_pid msg in
    let mode      = to_mode_exn (get_status_mode msg)
                                (get_status_t2mi_pid msg)
                                (get_status_t2mi_stream_id msg)
    in
    { status    = { timestamp
                  ; load         = (float_of_int ((get_status_load msg) * 100)) /. 255.
                  ; ts_num       = if has_sync then ts_num else 0
                  ; services_num = if has_sync then get_status_services_num msg else 0
                  ; bitrate      = Int32.to_int @@ get_status_bitrate msg
                  ; packet_sz    = if      (flags land 0x08) <> 0 then Ts192
                                   else if (flags land 0x10) <> 0 then Ts204
                                   else Ts188
                  ; has_sync
                  ; has_stream   = flags land 0x80 = 0
                  }
    ; reset       = flags2 land 0x02 <> 0
    ; input       = mode.input
    ; t2mi_mode   = mode.t2mi
    ; jitter_mode = if not @@ Int.equal jpid 0x1fff
                    then Some { stream  = Common.Stream.id_of_int32 (get_status_jitter_stream_id msg)
                              ; pid     = jpid
                              }
                    else None
    ; errors      = flags land 0x20 <> 0
    ; t2mi_sync   = int_to_bool_list (get_status_t2mi_sync msg)
                    |> (fun l -> List.foldi (fun acc i x -> if x then i :: acc else acc) [] l)
    ; versions    = { streams_ver      = get_status_streams_ver msg
                    ; ts_ver_com       = get_status_ts_ver_com msg
                    ; ts_ver_lst       = Cbuffer.fold (fun acc el -> el :: acc)
                                                      (iter @@ get_status_ts_ver_lst msg) []
                                         |> List.rev
                                         |> List.take ts_num
                    ; t2mi_ver_lst     = get_status_t2mi_ver_lst msg
                                         |> (fun v -> List.map (fun x -> Int32.shift_right v (4 * x)
                                                                         |> Int32.logand 0xfl
                                                                         |> Int32.to_int)
                                                               (List.range 0 7))
                    }
    ; streams     = []
    }

end

module TS_streams : (Event with type msg = Common.Stream.id list) = struct

  type msg = Common.Stream.id list

  let msg_code = 0x0B

  let of_cbuffer msg =
    let hdr,bdy' = Cbuffer.split msg sizeof_streams_list_event in
    let count    = get_streams_list_event_count hdr in
    let bdy,_    = Cbuffer.split bdy' (count * 4) in
    let iter     = Cbuffer.iter (fun _ -> Some 4) (fun buf -> Cbuffer.LE.get_uint32 buf 0) bdy in
    List.rev @@ Cbuffer.fold (fun acc el -> (Common.Stream.id_of_int32 el) :: acc) iter []

end

module Ts_errors : (Event with type msg := Errors.TS.t list) = struct

  open Board_types.Errors.TS

  let msg_code = 0x04

  let prioriry_of_err_code = function
    | x when x >= 0x11 && x <= 0x16 -> Some 1
    | x when x >= 0x21 && x <= 0x26 -> Some 2
    | x when x >= 0x31 && x <= 0x38 -> Some 3
    | _                             -> None

  let of_cbuffer msg : Errors.TS.t list =
    let common,rest = Cbuffer.split msg sizeof_ts_errors in
    let number      = get_ts_errors_count common in
    let errors,_    = Cbuffer.split rest (number * sizeof_ts_error) in
    let stream_id   = Common.Stream.id_of_int32 (get_ts_errors_stream_id common) in
    let timestamp   = Common.Time.Clock.now () in
    let iter        = Cbuffer.iter (fun _ -> Some sizeof_ts_error) (fun buf -> buf) errors in
    Cbuffer.fold (fun acc el ->
        let pid'      = get_ts_error_pid el in
        let pid       = pid' land 0x1FFF in
        let err_code  = get_ts_error_err_code el in
        match prioriry_of_err_code err_code with
        | Some priority ->
           { stream    = stream_id
           ; timestamp
           ; count     = get_ts_error_count el
           ; err_code
           ; err_ext   = get_ts_error_err_ext el
           ; priority
           ; multi_pid = (pid' land 0x8000) > 0
           ; pid
           ; packet    = get_ts_error_packet el
           ; param_1   = get_ts_error_param_1 el
           ; param_2   = get_ts_error_param_2 el
           } :: acc
        | None -> acc) iter []

end

module T2mi_errors : (Event with type msg := Errors.T2MI.t list) = struct

  open Board_types.Errors.T2MI

  let msg_code = 0x05

  let ts_parser_error_code   = 0xF0
  let t2mi_parser_error_code = 0xF1

  let get_relevant_t2mi_adv_code = function
    | 0  -> Some 2 | 1  -> Some 3 | 4  -> Some 4
    | 5  -> Some 5 | 6  -> Some 6 | 9  -> Some 7
    | 20 -> Some 8 | _  -> None

  (* Merge t2mi errors with counter and advanced errors. Result is common t2mi error type *)
  let merge (stream,timestamp,pid,sync)
            (count:t2mi_error_raw list)
            (param:t2mi_error_adv_raw list) : t list =
    List.map (fun (x:t2mi_error_raw) ->
        let param = get_relevant_t2mi_adv_code x.code
                    |> Option.flat_map (fun c ->
                           List.find_opt (fun a -> a.code = c && a.stream_id = x.stream_id) param
                           |> Option.map (fun (x:t2mi_error_adv_raw) -> x.param))
        in
        { stream
        ; timestamp
        ; pid
        ; sync      = List.mem ~eq:(Int.equal) x.stream_id sync
        ; stream_id = x.stream_id
        ; err_code  = x.code
        ; count     = x.count
        ; param }) count

  (* Convert t2mi advanced errors with 'code' = 0 to common t2mi error type *)
  let convert_other (stream,timestamp,pid,sync)
                    (other:t2mi_error_adv_raw list) : t list =
    List.map (fun (x:t2mi_error_adv_raw) ->
        { stream
        ; timestamp
        ; pid
        ; sync      = List.mem ~eq:(Int.equal) x.stream_id sync
        ; stream_id = x.stream_id
        ; err_code  = t2mi_parser_error_code
        ; count     = 1
        ; param     = Some x.param }) other

  (* Convert ts parser flags to common t2mi error type *)
  let convert_ts (stream,timestamp,pid,_) (ts:int list) : t list =
    List.map (fun (x:int) ->
        { stream
        ; timestamp
        ; pid
        ; sync      = true
        ; stream_id = 0
        ; err_code  = ts_parser_error_code
        ; count     = 1
        ; param     = Some x }) ts

  let of_cbuffer msg : t list =
    let timestamp   = Common.Time.Clock.now () in
    let common,rest = Cbuffer.split msg sizeof_t2mi_errors in
    let number      = get_t2mi_errors_count common in
    let errors,_    = Cbuffer.split rest (number * sizeof_t2mi_error) in
    let stream_id   = Common.Stream.id_of_int32 (get_t2mi_errors_stream_id common) in
    let pid         = get_t2mi_errors_pid common in
    let sync        = int_to_t2mi_sync_list (get_t2mi_errors_sync common) in
    let iter        = Cbuffer.iter (fun _ -> Some sizeof_t2mi_error) (fun buf -> buf) errors in
    let cnt,adv,oth =
      Cbuffer.fold (fun (cnt,adv,oth) el ->
          let index = get_t2mi_error_index el in
          let data  = get_t2mi_error_data el in
          let code  = index lsr 4 in
          let sid   = index land 7 in
          match index land 8 with
          | 0 -> if data > 0 (* filter zero errors *)
                 then let (x:Types.t2mi_error_raw) = { code; stream_id = sid; count = data } in
                      (x::cnt),adv,oth
                 else cnt,adv,oth
          | _ -> let f x = { code = x; stream_id = sid; param = data } in
                 if code = 0 (* t2mi parser error *)
                 then cnt,adv,((f t2mi_parser_error_code)::oth)
                 else cnt,((f code)::adv),oth)
                   iter ([],[],[])
    in
    let cm = stream_id,timestamp,pid,sync in
    let pe = get_t2mi_errors_err_flags common in
    let ts = List.filter_map (fun x -> if pe land (Int.pow 2 x) <> 0 then Some x else None)
                             (List.range 0 3)
    in (merge cm cnt adv) @ (convert_other cm oth) @ (convert_ts cm ts)

end


(* ----------------- Message deserialization ---------------- *)

type err = Bad_prefix           of int
         | Bad_length           of int
         | Bad_msg_code         of int
         | Bad_crc              of int * int * int
         | No_prefix_after_msg  of int
         | Insufficient_payload of Cbuffer.t
         | Unknown_err          of string

let string_of_err = function
  | Bad_prefix x           -> "incorrect prefix: " ^ (string_of_int x)
  | Bad_length x           -> "incorrect length: " ^ (string_of_int x)
  | Bad_msg_code x         -> "incorrect code: "   ^ (string_of_int x)
  | Bad_crc (code,x,y)     -> (Printf.sprintf "incorrect crc in msg with code = 0x%x, expected %d, got %d"
                                 code x y)
  | No_prefix_after_msg x  -> (Printf.sprintf "no prefix found after message payload, code = 0x%x" x)
  | Insufficient_payload _ -> "insufficient payload"
  | Unknown_err s          -> s

let check_prefix buf =
  let prefix' = get_common_header_prefix buf in
  if prefix <> prefix' then Error (Bad_prefix prefix') else Ok buf

let check_msg_code buf =
  let hdr,rest = Cbuffer.split buf sizeof_common_header in
  let code     = get_common_header_msg_code hdr in
  let has_crc  = (code land 2) > 0 in
  let length   = (match code lsr 8 with
                  | x when x = Get_board_info.rsp_code -> Some sizeof_board_info
                  | x when x = Get_board_mode.rsp_code -> Some sizeof_board_mode
                  | x when x = Status.msg_code         -> Some sizeof_status
                  | x when x = Ts_errors.msg_code      -> Some ((get_ts_errors_length rest * 2) + 2)
                  | x when x = T2mi_errors.msg_code    -> Some ((get_t2mi_errors_length rest * 2) + 2)
                  | x when x = TS_streams.msg_code     -> Some ((get_streams_list_event_length rest * 2) + 2)
                  | 0x09 -> Some ((get_complex_rsp_header_length rest * 2) + 2)
                  | 0xFD -> Some 4 (* end of errors *)
                  | 0xFF -> Some 0 (* end of transmission *)
                  | _    -> None) in
  match length with
  | Some x -> Ok (x + (if has_crc then 2 else 0), has_crc, code, rest)
  | None   -> Error (Bad_msg_code code)

let check_length (len,has_crc,code,rest') =
  if len > 512 - sizeof_common_header then Error (Bad_length len)
  else let body,rest = Cbuffer.split rest' len in
       Ok (has_crc,code,body,rest)

let check_next_prefix ((code,_,rest) as x) =
  if Cbuffer.len rest < sizeof_common_header then Ok x
  else (match check_prefix rest with
        | Ok _    -> Ok x
        | Error _ -> Error (No_prefix_after_msg code))

let check_crc (code,body,rest) =
  let b         = Cbuffer.create 2 |> (fun b -> Cbuffer.LE.set_uint16 b 0 code; b) in
  let body,crc' = Cbuffer.split body ((Cbuffer.len body) - 2) in
  let iter      = Cbuffer.iter (fun _ -> Some 2) (fun buf -> Cbuffer.LE.get_uint16 buf 0) (Cbuffer.append b body) in
  let crc       = (Cbuffer.fold (fun acc el -> el + acc) iter 0) land 0xFFFF in
  let crc'      = Cbuffer.LE.get_uint16 crc' 0 in
  if crc <> crc' then Error (Bad_crc (code,crc,crc')) else Ok (code,body,rest)

let get_msg buf =
  try
    Result.(check_prefix buf
            >>= check_msg_code
            >>= check_length
            >>= (fun (has_crc,code,body,rest) -> if has_crc then check_crc (code,body,rest)
                                                 else check_next_prefix (code,body,rest)))
  with
  | Invalid_argument _ -> Error (Insufficient_payload buf)
  | e                  -> Error (Unknown_err (Printexc.to_string e))

let parse_simple_msg = fun (code,body,parts) ->
  try
    (match code lsr 8 with
     | x when x = Get_board_info.rsp_code -> `R (`Board_info body)
     | x when x = Get_board_mode.rsp_code -> `R (`Board_mode body)
     | x when x = Status.msg_code         -> `E (`Status        (Status.of_cbuffer body))
     | x when x = Ts_errors.msg_code      -> `E (`Ts_errors     (Ts_errors.of_cbuffer body))
     | x when x = T2mi_errors.msg_code    -> `E (`T2mi_errors   (T2mi_errors.of_cbuffer body))
     | x when x = TS_streams.msg_code     -> `E (`Streams_event (TS_streams.of_cbuffer body))
     | 0xFD -> `E `End_of_errors;
     | 0x09 -> let code_ext = get_complex_rsp_header_code_ext body in
               let long     = code_ext land 0x2000 <> 0 in
               let parity   = if code_ext land 0x1000 <> 0 then 1 else 0 in
               let data'    = if long then Cbuffer.shift body sizeof_complex_rsp_header_ext
                              else Cbuffer.shift body sizeof_complex_rsp_header in
               let data,_   = Cbuffer.split data' (Cbuffer.len data' - parity) in
               let part     = { first = code_ext land 0x8000 <> 0
                              ; param = Int32.mul 2l (if long then get_complex_rsp_header_ext_param body
                                                      else Int32.of_int @@ get_complex_rsp_header_param body)
                                        |> (fun x -> Int32.sub x (Int32.of_int parity))
                              ; data
                              } in
               let code     = code_ext land 0x0FFF in
               let req_id   = get_complex_rsp_header_request_id body in
               `P (List.Assoc.update (code,req_id)
                     ~eq:(Pair.equal (=) (=))
                     ~f:(function
                       | Some x -> Some (part :: x)
                       | None   -> Some ([part]))
                     parts)
     | _ -> `N)
  with _ -> `N

let parse_complex_msg = fun ((code,r_id),msg) ->
  try
    let data = (r_id,msg) in
    (match code with
     | x when x = Get_board_errors.rsp_code   -> `ER (`Board_errors data)
     | x when x = Get_section.rsp_code        -> Get_section.of_cbuffer msg |> ignore;
                                                 `R  (`Section data)
     | x when x = Get_t2mi_frame_seq.rsp_code -> `R  (`T2mi_frame_seq data)
     | x when x = Get_jitter.rsp_code         -> `ER (`Jitter (r_id,(get_jitter_req_ptr msg),msg))
     | x when x = Get_ts_structs.rsp_code     -> `ER (`Struct (r_id,(get_ts_structs_version msg),msg))
     | x when x = Get_bitrates.rsp_code       -> `ER (`Bitrates (r_id,(get_bitrates_version msg),msg))
     | x when x = Get_t2mi_info.rsp_code      -> `ER (`T2mi_info (r_id,
                                                                  (get_t2mi_info_version msg),
                                                                  (get_t2mi_info_stream_id msg),
                                                                  msg))
     | _ -> `N)
  with _ -> `N

let try_compose_parts ((id,gp) as x) =
  let gp = List.sort (fun x y -> if x.first then (-1)
                                 else if y.first then 1
                                 else Int32.compare x.param y.param) gp in
  let first,rest = List.hd_tl gp in
  try (let acc = List.fold_left (fun acc x -> if Int32.equal x.param (Int32.of_int (Cbuffer.len acc))
                                              then Cbuffer.append acc x.data
                                              else failwith "Incorrect part offset")
                   first.data rest in
       if Int32.equal first.param (Int32.of_int (Cbuffer.len acc)) then `F (id,acc) else `P x)
  with _ -> `N

let deserialize parts buf =
  let rec f events event_rsps rsps parts b =
    if Cbuffer.len b >= sizeof_common_header
    then (match get_msg b with
          | Ok (code,bdy,rest) -> (match parse_simple_msg (code,bdy,parts) with
                                   | `E x  -> f (x::events) event_rsps rsps parts rest
                                   | `ER x -> f events (x::event_rsps) rsps parts rest
                                   | `R x  -> f events event_rsps (x::rsps) parts rest
                                   | `P x  -> f events event_rsps rsps x rest
                                   | `N    -> f events event_rsps rsps parts rest)
          | Error e ->
             (match e with
              | Insufficient_payload x -> (List.rev events, List.rev event_rsps, List.rev rsps, List.rev parts, x)
              | _                      -> f events event_rsps rsps parts (Cbuffer.shift b 1)))
    else (List.rev events, List.rev event_rsps, List.rev rsps, List.rev parts, b) in
  let ev,ev_rsps,rsps,parts,res = f [] [] [] parts buf in
  let parts = List.filter (fun (_,x) -> let first_msgs = List.find_all (fun x -> x.first) x in
                                        match first_msgs with
                                        | [_] -> true
                                        | _   -> false) parts in
  let ev_rsps,rsps,parts = List.fold_left (fun acc x -> let e,r,p = acc in
                                                        (match try_compose_parts x with
                                                         | `F x -> (match (parse_complex_msg x) with
                                                                    | `ER x -> x::e,r,p
                                                                    | `R x -> e,x::r,p
                                                                    | `N   -> acc)
                                                         | `P x -> e,r,x::p
                                                         | `N   -> e,r,p))
                             (ev_rsps,rsps,[]) parts in
  ev, ev_rsps, rsps, parts, if Cbuffer.len res > 0 then Some res else None

let try_parse f x = try Some (f x) with _ -> None

let parse_get_board_info = function
  | `Board_info buf -> try_parse Get_board_info.of_cbuffer buf
  |  _ -> None

let parse_get_board_mode = function
  | `Board_mode buf -> try_parse Get_board_mode.of_cbuffer buf
  | _ -> None

let parse_get_board_errors req_id = function
  | `Board_errors (r_id,buf) -> if req_id <> r_id then None
                                else (match try_parse Get_board_errors.of_cbuffer buf with
                                      | Some x -> Some (Board_errors x)
                                      | None   -> None)
  | _ -> None

let parse_get_section (req : section_req) = function
  | `Section (r_id,buf) ->
     let open Streams.TS in
     if req.request_id <> r_id then None
     else Option.map (Result.map (fun x -> { x with table_id = req.params.table_id }))
                     (try_parse Get_section.of_cbuffer buf)
  | _ -> None

let parse_get_t2mi_frame_seq (req : t2mi_frame_seq_req) = function
  | `T2mi_frame_seq (r_id,buf) -> if req.request_id <> r_id then None
                                  else try_parse Get_t2mi_frame_seq.of_cbuffer buf
  | _ -> None

let parse_get_jitter (req : jitter_req) = function
  | `Jitter (r_id,pointer,buf) -> if req.request_id <> r_id then None
                                  else if not (Int32.equal req.pointer pointer) then None
                                  else (match try_parse Get_jitter.of_cbuffer buf with
                                        | Some x -> Some (Jitter x)
                                        | None   -> None)
  | _ -> None

let parse_get_ts_structs req_id = function
  | `Struct (r_id,_,buf) -> if req_id <> r_id then None
                            else (match try_parse Get_ts_structs.of_cbuffer buf with
                                  | Some x -> Some (Struct x)
                                  | None   -> None)
  | _ -> None

let parse_get_bitrates req_id = function
  | `Bitrates (r_id,_,buf) -> if req_id <> r_id then None
                              else (match try_parse Get_bitrates.of_cbuffer buf with
                                    | Some x -> Some (Bitrate x)
                                    | None   -> None)
  | _ -> None


let parse_get_t2mi_info (req : t2mi_info_req) = function
  | `T2mi_info (r_id,_,stream_id,buf) -> if req.request_id <> r_id then None
                                         else if req.stream_id <> stream_id then None
                                         else (match try_parse Get_t2mi_info.of_cbuffer buf with
                                               | Some x -> Some (T2mi_info x)
                                               | None   -> None)
  | _ -> None

let is_response (type a) (req : a request) msg : a option =
  match req with
  | Get_board_info       -> parse_get_board_info msg
  | Get_board_mode       -> parse_get_board_mode msg
  | Get_t2mi_frame_seq x -> parse_get_t2mi_frame_seq x msg
  | Get_section x        -> parse_get_section x msg

let is_probe_response (type a) (req : a probe_request) msg : a option =
  match req with
  | Get_board_errors id -> parse_get_board_errors id msg
  | Get_jitter x        -> parse_get_jitter x msg
  | Get_ts_structs x    -> parse_get_ts_structs x msg
  | Get_bitrates x      -> parse_get_bitrates x msg
  | Get_t2mi_info x     -> parse_get_t2mi_info x msg
