open Containers

let prefix = 0x55AA

include Board_msg_formats

open Board_types
open Types
open Common

module Multi_TS_ID = Stream.Multi_TS_ID

let ( % ) = Fun.( % )

type part =
  { first : bool
  ; param : int32
  ; data : Cstruct.t
  }

type _ instant_request =
  | Set_board_init : Types.init -> unit instant_request
  | Set_board_mode : input * t2mi_mode_raw -> unit instant_request
  | Set_jitter_mode : jitter_mode option -> unit instant_request
  | Reset : unit instant_request

type probe_response =
  | Board_errors of board_error list
  | Bitrate of (Multi_TS_ID.t * Bitrate.t timestamped) list
  | Struct of (Multi_TS_ID.t * structure) list
  | T2mi_info of (Multi_TS_ID.t * T2mi_info.t)
  | Jitter of Types.jitter_raw

type _ probe_request =
  | Get_board_errors : int -> probe_response probe_request
  | Get_jitter : jitter_req -> probe_response probe_request
  | Get_ts_struct : ts_struct_req -> probe_response probe_request
  | Get_bitrates : int -> probe_response probe_request
  | Get_t2mi_info : t2mi_info_req -> probe_response probe_request

type _ request =
  | Get_board_info : devinfo request
  | Get_board_mode : (input * t2mi_mode_raw) request
  | Get_t2mi_frame_seq : t2mi_frame_seq_req -> T2mi_sequence.t timestamped request
  | Get_section : section_req -> (SI_PSI_section.t timestamped,
                                  SI_PSI_section.dump_error) result request

(* ------------------- Misc ------------------- *)

let int_to_bool_list x =
  List.map (fun i -> (x land Int.pow 2 i) > 0) (List.range 0 7)

let int_to_t2mi_sync_list x =
  int_to_bool_list x
  |> List.foldi (fun acc i x -> if x then i :: acc else acc) []

(* -------------------- Message constructors ------------------*)

let to_common_header ~msg_code () =
  let hdr = Cstruct.create sizeof_common_header in
  let () = set_common_header_prefix hdr prefix in
  let () = set_common_header_msg_code hdr msg_code in
  hdr

let to_complex_req_header ?(client_id=0) ?(request_id=0) ~msg_code ~length () =
  let hdr = to_common_header ~msg_code () in
  let complex_hdr = Cstruct.create sizeof_complex_req_header in
  let () = set_complex_req_header_client_id complex_hdr client_id in
  let () = set_complex_req_header_length complex_hdr length in
  let () = set_complex_req_header_request_id complex_hdr request_id in
  Cstruct.append hdr complex_hdr

let to_simple_req ~msg_code ~body () =
  let hdr = to_common_header ~msg_code () in
  Cstruct.append hdr body

let to_complex_req ?client_id ?request_id ~msg_code ~body () =
  let length = (Cstruct.len body / 2) + 1 in
  let hdr = to_complex_req_header ?client_id ?request_id ~msg_code ~length () in
  Cstruct.append hdr body

let to_set_board_init_req (src : Types.init) =
  let body = Cstruct.create sizeof_req_set_init in
  let () = set_req_set_init_input_src_id body src.input in
  let () = set_req_set_init_t2mi_src_id body src.t2mi in
  to_simple_req ~msg_code:0x0089  ~body ()

let to_set_board_mode_req ((input : input), (mode : t2mi_mode_raw)) =
  let { pid; enabled; stream; t2mi_stream_id } = mode in
  let body = Cstruct.create sizeof_board_mode in
  let pid = (t2mi_stream_id lsl 13) lor (pid land 0x1FFF) in
  input_to_int input
  |> (lor) (if enabled then 4 else 0)
  |> (lor) 8 (* disable board storage by default *)
  |> set_board_mode_mode body;
  set_board_mode_t2mi_pid body pid;
  set_board_mode_stream_id body @@ Multi_TS_ID.to_int32_pure stream;
  to_simple_req ~msg_code:0x0082 ~body ()

let to_set_jitter_mode_req (mode : jitter_mode option) =
  let pid, stream = match mode with
    | Some m -> m.pid, Multi_TS_ID.to_int32_pure m.stream
    | None -> 0, 0l in
  let body = Cstruct.create sizeof_req_set_jitter_mode in
  let () = set_req_set_jitter_mode_stream_id body stream in
  let () = set_req_set_jitter_mode_pid body pid in
  to_complex_req ~msg_code:0x0112 ~body ()

(* -------------------- Requests/responses/events ------------------*)

let to_mode_exn mode t2mi_pid stream_id : input * t2mi_mode_raw =
  Option.get_exn @@ input_of_int (mode land 1),
  { enabled = if (mode land 4) > 0 then true else false
  ; pid = t2mi_pid land 0x1fff
  ; t2mi_stream_id = (t2mi_pid lsr 13) land 0x7
  ; stream = stream_id }

module type Request = sig

  type req
  type rsp

  val req_code : int
  val rsp_code : int
  val serialize : req -> Cstruct.t
  val parse : req -> Cstruct.t -> rsp

end

module type Event = sig

  type msg

  val msg_code : int
  val parse : Cstruct.t -> msg

end

module Get_board_info : (Request
                         with type req := unit
                         with type rsp := devinfo) = struct

  let req_code = 0x0080
  let rsp_code = 0x01

  let serialize () = to_common_header ~msg_code:req_code ()
  let parse _ msg =
    { typ = get_board_info_board_type msg
    ; ver = get_board_info_board_version msg
    }

end

module Get_board_mode : (Request
                         with type req := unit
                         with type rsp := input * t2mi_mode_raw) = struct

  let req_code = 0x0081
  let rsp_code = 0x02

  let serialize () = to_common_header ~msg_code:req_code ()
  let parse _ msg =
    to_mode_exn (get_board_mode_mode msg)
      (get_board_mode_t2mi_pid msg)
      (Multi_TS_ID.of_int32_pure (get_board_mode_stream_id msg))

end

module Get_board_errors : (Request
                           with type req := int
                           with type rsp := board_error list) = struct

  let req_code = 0x0110
  let rsp_code = req_code

  let serialize request_id =
    to_complex_req ~request_id
      ~msg_code:req_code
      ~body:(Cstruct.create 0) ()

  let parse _ msg =
    let timestamp = Time.Clock.now () in
    let iter =
      Cstruct.iter (fun _ -> Some 4)
        (fun buf -> Cstruct.LE.get_uint32 buf 0)
        (get_board_errors_errors msg)
    in
    List.rev @@ Cstruct.fold (fun acc el -> el :: acc) iter []
    |> List.foldi (fun acc i x ->
           let count = Int32.to_int x in
           if count = 0 || i < 0 || i > 16 then acc
           else { timestamp; err_code = i; count } :: acc) []

end

module Get_section
       : (Request
          with type req = section_req
          with type rsp = (SI_PSI_section.t timestamped,
                           SI_PSI_section.dump_error) result) = struct

  open Board_types.SI_PSI_section

  type req = section_req
  type rsp = (t timestamped, dump_error) result

  let req_code = 0x0302
  let rsp_code = req_code

  let serialize { request_id; params } =
    let body = Cstruct.create sizeof_req_get_section in
    let id = Multi_TS_ID.to_int32_pure params.stream_id in
    let () = set_req_get_section_stream_id body id in
    let () = set_req_get_section_table_id body params.table_id in
    let () = Option.iter (set_req_get_section_section body) params.section in
    let () = Option.iter (set_req_get_section_table_id_ext body) params.table_id_ext in
    let () = Option.iter (set_req_get_section_ext_info_1 body) params.ext_info_1 in
    let () = Option.iter (set_req_get_section_ext_info_2 body) params.ext_info_2 in
    to_complex_req ~request_id ~msg_code:req_code ~body ()

  let parse ({ params; _ } : req) msg =
    let hdr, bdy = Cstruct.split msg sizeof_section in
    let length = get_section_length hdr in
    let result = get_section_result hdr in
    let timestamp = Time.Clock.now_s () in
    if length > 0 && result = 0
    then let sid,data  = Cstruct.split bdy 4 in
         let table_id  = params.table_id in
         let stream_id = Cstruct.LE.get_uint32 sid 0 in
         let raw = Cstruct.to_string data in
         let table = Mpeg_ts.table_of_int table_id in
         let data =
           { section = List.map (Char.code) @@ String.to_list raw
           ; stream_id = Multi_TS_ID.of_int32_pure stream_id
           ; table_id
           ; section_id = Option.get_or ~default:0 params.section
           ; content = Si_psi_parser.table_to_yojson raw table } in
         Ok { timestamp; data }
    else (Error (match result with
                 | 0 | 3 -> Zero_length
                 | 1 -> Table_not_found
                 | 2 -> Section_not_found
                 | 4 -> Stream_not_found
                 | _ -> Unknown))

end

module Get_t2mi_frame_seq : (Request
                             with type req := t2mi_frame_seq_req
                             with type rsp := T2mi_sequence.t timestamped) = struct
  open Board_types.T2mi_sequence

  let req_code = 0x0306
  let rsp_code = req_code

  let serialize ({ request_id; params } : t2mi_frame_seq_req) =
    let body = Cstruct.create sizeof_req_get_t2mi_frame_seq in
    let ()   = set_req_get_t2mi_frame_seq_time body params.seconds in
    to_complex_req ~request_id:request_id ~msg_code:req_code ~body ()

  let parse _ msg : t timestamped =
    let iter = Cstruct.iter (fun _ -> Some sizeof_t2mi_frame_seq_item)
                 (fun buf -> buf) msg in
    let items =
      Cstruct.fold (fun (acc : item list) el ->
          let sframe_stream = get_t2mi_frame_seq_item_sframe_stream el in
          { typ = get_t2mi_frame_seq_item_typ el
          ; super_frame = (sframe_stream land 0xF0) lsr 4
          ; stream_id = sframe_stream land 0x07
          ; frame = get_t2mi_frame_seq_item_frame el
          ; count = Int32.to_int @@ get_t2mi_frame_seq_item_count el
          ; plp = get_t2mi_frame_seq_item_plp el
          ; l1_param_1 = get_t2mi_frame_seq_item_dyn1_frame el
          ; l1_param_2 = get_t2mi_frame_seq_item_dyn2_frame el
          ; ts_packet = Int32.to_int @@ get_t2mi_frame_seq_item_time el
          } :: acc)
        iter []
      |> List.rev in
    { timestamp = Time.Clock.now_s ()
    ; data = items }

end

module Get_jitter : (Request
                     with type req := jitter_req
                     with type rsp := Types.jitter_raw) = struct

  let req_code = 0x0307
  let rsp_code = req_code

  let serialize { request_id; pointer } =
    let body = Cstruct.create sizeof_req_get_jitter in
    let () = set_req_get_jitter_ptr body pointer in
    to_complex_req ~request_id ~msg_code:req_code ~body ()

  let parse_item el packet_time : Jitter.measure =
    let status = get_jitter_item_status el in
    let d_pack = get_jitter_item_d_packet el in
    let d_pcr = get_jitter_item_d_pcr el in
    let drift = get_jitter_item_drift el in
    let fo = get_jitter_item_fo el in
    let jitter = get_jitter_item_jitter el in
    let t_pcr = ((Int32.to_float d_pcr) *. 10e+9) /. 27e+6 in
    let t_pcr_br = (Int32.to_float packet_time) *. (float_of_int d_pack) in
    let accuracy = t_pcr /. t_pcr_br in
    { discont_err = status land 0x8000 <> 0
    ; discont_ok = status land 0x4000 <> 0
    ; t_pcr = t_pcr_br
    ; accuracy
    ; jitter
    ; drift = Int32.float_of_bits drift
    ; fo = Int32.float_of_bits fo
    ; period = t_pcr_br /. 10e+6
    }

  let parse _ msg : Types.jitter_raw =
    let hdr, bdy' = Cstruct.split msg sizeof_jitter in
    let count  = get_jitter_count hdr in
    let bdy, _  = Cstruct.split bdy' @@ sizeof_jitter_item * count in
    let pid = get_jitter_pid hdr in
    let t_pcr = Int32.float_of_bits @@ get_jitter_t_pcr hdr in
    let time = Int32.to_int @@ get_jitter_time hdr in
    let next_ptr = get_jitter_req_next hdr in
    let packet_time = get_jitter_packet_time hdr in
    let iter = Cstruct.iter (fun _ -> Some sizeof_jitter_item) (fun buf -> buf) bdy in
    let timestamp = Time.Clock.now () in
    let measures =
      Cstruct.fold (fun acc el -> (parse_item el packet_time) :: acc)
        iter [] |> List.rev in
    { measures; next_ptr; time; timestamp; pid; t_pcr }

end

module Get_ts_structs
       : (Request
          with type req := ts_struct_req
          with type rsp := (Multi_TS_ID.t * structure) list) = struct

  type service_acc =
    { info : Cstruct.t
    ; es : Cstruct.t
    ; ecm : Cstruct.t
    }

  type acc =
    { general : Cstruct.t
    ; pids : Cstruct.t
    ; services : service_acc list
    ; emm : Cstruct.t
    ; tables : Cstruct.t list
    ; service : service_acc option
    }

  let service_acc_empty =
    { info = Cstruct.empty
    ; es = Cstruct.empty
    ; ecm = Cstruct.empty
    }

  let acc_empty : acc =
    { general = Cstruct.empty
    ; pids = Cstruct.empty
    ; services = []
    ; emm = Cstruct.empty
    ; tables = []
    ; service = None }

  let req_code = 0x0309
  let rsp_code = req_code

  let serialize ({ stream; request_id } : ts_struct_req) =
    let id = match stream with
      | `All -> 0xFFFF_FFFFl
      | `Single x -> Multi_TS_ID.to_int32_pure x in
    let body = Cstruct.create sizeof_req_get_ts_struct in
    let () = set_req_get_ts_struct_stream_id body id in
    to_complex_req ~request_id ~msg_code:req_code ~body ()

  let of_general_block (msg : Cstruct.t) : Ts_info.t * int =
    let bdy, rest = Cstruct.split msg sizeof_general_struct_block in
    let string_len = get_general_struct_block_string_len bdy in
    let nw_pid' = get_general_struct_block_network_pid bdy in
    let strings, _ = Cstruct.split rest (string_len * 2) in
    let nw_name, bq_name = Cstruct.split strings string_len in
    { services_num = get_general_struct_block_services_num bdy
    ; nw_pid = nw_pid' land 0x1FFF
    ; complete = (nw_pid' land 0x4000) <> 0
    ; ts_id = get_general_struct_block_ts_id bdy
    ; nw_id = get_general_struct_block_nw_id bdy
    ; orig_nw_id = get_general_struct_block_orig_nw_id bdy
    ; nw_name = Result.get_or ~default:"" @@ Text_decoder.decode nw_name
    ; bouquet_name = Result.get_or ~default:"" @@ Text_decoder.decode bq_name
    }, string_len

  let of_pids_block (msg : Cstruct.t) : (Pid.id * Pid.t) list =
    let open Pid in
    let iter = Cstruct.iter (fun _ -> Some 2)
                 (fun buf -> Cstruct.LE.get_uint16 buf 0) msg in
    List.rev
    @@ Cstruct.fold (fun acc el ->
           let pid = el land 0x1FFF in
           let description =
             { has_pts = (el land 0x8000) <> 0
             ; has_pcr = false (* Filled in later *)
             ; scrambled = (el land 0x4000) <> 0
             ; typ = Private (* Filled in later *)
             ; service_id = None (* Filled in later *)
             ; present = (el land 0x2000) <> 0 } in
           (pid, description) :: acc) iter []

  let of_service_block (string_len : int)
        (msg : Cstruct.t) : Service.t =
    let bdy, rest = Cstruct.split msg sizeof_services_struct_block in
    let flags = get_services_struct_block_flags bdy in
    let strings, _ = Cstruct.split rest (string_len * 2) in
    let sn, pn = Cstruct.split strings string_len in
    let id = get_services_struct_block_id bdy in
    let default = Printf.sprintf "Service %d" id in
    { id
    ; name = Result.get_or ~default @@ Text_decoder.decode sn
    ; provider_name = Result.get_or ~default:"" @@ Text_decoder.decode pn
    ; pmt_pid = get_services_struct_block_pmt_pid bdy
    ; pcr_pid = get_services_struct_block_pcr_pid bdy
    ; has_pmt = (flags land 0x8000) <> 0
    ; has_sdt = (flags land 0x4000) <> 0
    ; dscr = (flags land 0x2000) <> 0
    ; dscr_list = (flags land 0x1000) <> 0
    ; eit_schedule = (flags land 0x0080) <> 0
    ; eit_pf = (flags land 0x0040) <> 0
    ; free_ca_mode = (flags land 0x0020) <> 0
    ; running_status = flags land 0x0007
    ; service_type = get_services_struct_block_service_type bdy
    ; service_type_list = get_services_struct_block_service_type_list bdy
    ; elements = [] (* Filled in later *)
    }

  let of_es_block (msg : Cstruct.t) : Service.element list =
    let iter = Cstruct.iter (fun _ -> Some 4) (fun buf -> buf) msg in
    Cstruct.fold (fun acc x ->
        let open Pid in
        let open Service in
        let pid = (get_es_struct_block_pid x) land 0x1FFF in
        { pid
        ; info = PES { stream_type = get_es_struct_block_es_type x
                     ; stream_id = get_es_struct_block_es_stream_id x }
        } :: acc) iter []
    |> List.rev

  let of_ecm_block (msg : Cstruct.t) : Service.element list =
    let iter = Cstruct.iter (fun _ -> Some 4) (fun buf -> buf) msg in
    Cstruct.fold (fun acc x ->
        let open Pid in
        let open Service in
        let pid = get_ecm_struct_block_pid x land 0x1FFF in
        let ca_sys_id = get_ecm_struct_block_ca_system_id x in
        { pid
        ; info = ECM { ca_sys_id }
        } :: acc)
      iter []
    |> List.rev

  let of_emm_block (msg : Cstruct.t) : Service.element list =
    of_ecm_block msg

  let of_table_block (msg : Cstruct.t) : SI_PSI_table.t =
    let open SI_PSI_table in
    let bdy, rest = Cstruct.split msg sizeof_table_struct_block in
    let iter = Cstruct.iter (fun _ -> Some 2)
                 (fun buf -> Cstruct.LE.get_uint16 buf 0) rest in
    let sections =
      Cstruct.fold (fun acc x ->
          let id = List.length acc in
          let _ = (x land 0x8000) > 0 in (* analyzed *)
          let length = (x land 0x0FFF) in
          ({ id; length } : section_info) :: acc) iter []
      |> List.filter (fun ({ length; _ } : section_info) -> length > 0)
      |> List.rev in
    let pid' = get_table_struct_block_pid msg in
    let id = get_table_struct_block_id bdy in
    let id_ext = get_table_struct_block_id_ext bdy in
    let ext_info =
      { ext_1 = get_table_struct_block_ext_info_1 bdy
      ; ext_2 = get_table_struct_block_ext_info_2 bdy
      ; ext_3 = get_table_struct_block_ext_info_3 bdy
      ; ext_4 = get_table_struct_block_ext_info_4 bdy
      } in
    let main =
      { version = get_table_struct_block_version bdy
      ; id
      ; id_ext
      ; ext_info
      ; pid = pid' land 0x1FFF
      ; service_id = None (* Filled in later *)
      ; last_section = get_table_struct_block_lsn bdy
      ; section_syntax = (pid' land 0x8000) > 0
      }
    in
    { main; sections }

  let split_into_blocks (msg : Cstruct.t) : acc =
    let rec aux msg acc = match Cstruct.len msg with
      | 0 -> acc
      | _ ->
         let hdr, data = Cstruct.split msg sizeof_struct_block_header in
         let len = get_struct_block_header_length hdr in
         let block, rest = Cstruct.split data len in
         let acc = match get_struct_block_header_code hdr with
           | 0x2000 -> { acc with general = block }
           | 0x2100 -> { acc with pids = Cstruct.append acc.pids block }
           | 0x2200 ->
              let acc = match acc.service with
                | None -> acc
                | Some x -> { acc with services = x :: acc.services } in
              let service = Some { service_acc_empty with info = block } in
              { acc with service }
           | 0x2201 ->
              begin match acc.service with
              | None -> failwith "got es block before service info block"
              | Some ({ es; _ } as service) ->
                 let es = Cstruct.append es block in
                 let service = Some { service with es } in
                 { acc with service }
              end
           | 0x2202 ->
              begin match acc.service with
              | None -> failwith "got ecm block before service info block"
              | Some ({ ecm; _ } as service) ->
                 let ecm = Cstruct.append ecm block in
                 let service = Some { service with ecm } in
                 { acc with service }
              end
           | 0x2300 -> { acc with emm = Cstruct.append acc.emm block }
           | 0x2400 -> { acc with tables = block :: acc.tables }
           | x ->
              let s = Printf.sprintf "unknown structure block: 0x%X" x in
              failwith s in
         aux rest acc
    in
    aux msg acc_empty

  let parse_service (slen : int) (acc : service_acc) : Service.t =
    let es = of_es_block acc.es in
    let ecm = of_ecm_block acc.ecm in
    let info = of_service_block slen acc.info in
    { info with elements = es @ ecm }

  let update_if_null (pid, info) : (Pid.id * Pid.t) option =
    if pid = 0x1FFF
    then Some (pid, { info with typ = Null }) else None

  let find_in_elements ((pid, _) : Pid.id * Pid.t)
        (elements : Service.element list) : Pid.typ option =
    List.find_map (fun (x : Service.element) ->
        if x.pid = pid then Some x.info else None)
      elements

  let update_if_in_services (services : Service.t list)
        ((pid, info) : Pid.id * Pid.t) : (Pid.id * Pid.t) option =
    let result =
      List.find_map (fun (x : Service.t) ->
          match find_in_elements (pid, info) x.elements with
          | None -> None
          | Some t -> Some (x.id, x.pcr_pid, t)) services in
    match result with
    | None -> None
    | Some (id, pcr_pid, typ) ->
       let has_pcr = pid = pcr_pid in
       Some (pid, { info with service_id = Some id; typ; has_pcr })

  let update_if_in_emm (emm : Service.element list)
        (pid, info) : (Pid.id * Pid.t) option =
    match find_in_elements (pid, info) emm with
    | None -> None
    | Some typ -> Some (pid, { info with typ })

  let update_if_in_sections (tables : SI_PSI_table.t list)
        (pid, (info : Pid.t)) : (Pid.id * Pid.t) option =
    List.find_all (fun (x : SI_PSI_table.t) -> x.main.pid = pid) tables
    |> (function
        | [ ] -> None
        | [x] ->
           let open Pid in
           begin match Mpeg_ts.table_of_int x.main.id with
           | `PMT -> Some (x.main.service_id, SEC [x.main.id])
           | _ -> Some (None, SEC [x.main.id])
           end
        | l ->
           let ids =
             List.map (fun (x : SI_PSI_table.t) -> x.main.id) l
             |> List.sort_uniq ~cmp:Int.compare in
           Some (None, SEC ids))
    |> function
      | None -> None
      | Some (id, typ) -> Some (pid, { info with service_id = id; typ })

  let update_pid (services : Service.t list)
        (tables : SI_PSI_table.t list)
        (emm : Service.element list)
        (pid : Pid.id * Pid.t) : Pid.id * Pid.t =
    let ( >>= ) x f = match x with
      | Some x -> Some x
      | None -> f pid in
    update_if_null pid
    >>= update_if_in_services services
    >>= update_if_in_emm emm
    >>= update_if_in_sections tables
    |> function None -> pid | Some x -> x

  let update_section (services : Service.t list)
        (table : SI_PSI_table.t) : SI_PSI_table.t =
    let service_id = match Mpeg_ts.table_of_int table.main.id with
      | `EIT (`Actual, _) ->
         List.find_map (fun (s : Service.t) ->
             if s.id = table.main.id_ext then Some s.id else None)
           services
      | `PMT ->
         List.find_map (fun (s : Service.t) ->
             if s.has_pmt && s.pmt_pid = table.main.pid
             then Some s.id else None)
           services
      | _ -> None
    in
    { table with main = { table.main with service_id }}

  let parse_blocks (msg : Cstruct.t) : structure =
    let acc = split_into_blocks msg in
    let info, slen = of_general_block acc.general in
    let services = List.rev_map (parse_service slen) acc.services in
    let tables = List.rev_map (update_section services % of_table_block)
                   acc.tables in
    let emm = of_emm_block acc.emm in
    let pids = List.map (update_pid services tables emm)
               @@ of_pids_block acc.pids in
    { info; services; tables; pids }

  let of_ts_struct msg : (Multi_TS_ID.t * structure) * Cstruct.t option =
    let hdr, rest = Cstruct.split msg sizeof_ts_struct in
    let len = (Int32.to_int @@ get_ts_struct_length hdr) in
    let bdy, rest = Cstruct.split rest len in
    let structure = parse_blocks bdy in
    let stream = Multi_TS_ID.of_int32_pure @@ get_ts_struct_stream_id hdr in
    let rest = if Cstruct.len rest > 0 then Some rest else None in
    (stream, structure), rest

  let parse ({ stream; _ } : ts_struct_req)
        (msg : Cstruct.t) : (Multi_TS_ID.t * structure) list =
    let hdr, bdy' = Cstruct.split msg sizeof_ts_structs in
    let count = get_ts_structs_count hdr in
    (* stream id list *)
    let _, bdy = Cstruct.split bdy' (count * 4) in
    (* FIXME stuffing bytes when requesting for a single stream *)
    let parse = match stream with
      | `All ->
         let rec f = fun acc buf ->
           let x, rest = of_ts_struct buf in
           match rest with
           | Some b -> f (x :: acc) b
           | None -> List.rev (x :: acc) in
         f []
      | `Single _ ->
         fun buf -> let s, _ = of_ts_struct buf in [s] in
    if count > 0 then parse bdy else []

end

module Get_bitrates : (
  Request
  with type req := int
  with type rsp := (Multi_TS_ID.t * Bitrate.t timestamped) list) = struct

  open Board_types.Bitrate

  let req_code = 0x030A
  let rsp_code = req_code

  let serialize request_id =
    to_complex_req ~request_id ~msg_code:req_code ~body:(Cstruct.create 0) ()

  let of_pids_bitrate total_pids br_per_pkt buf =
    let msg, rest = Cstruct.split buf (sizeof_pid_bitrate * total_pids) in
    let iter = Cstruct.iter (fun _ -> Some sizeof_pid_bitrate)
                 (fun buf -> buf) msg in
    let pids =
      Cstruct.fold (fun acc el ->
          let packets = get_pid_bitrate_packets el in
          let pid = get_pid_bitrate_pid el land 0x1FFF in
          let br = int_of_float @@ br_per_pkt *. (Int32.to_float packets) in
          (pid, br) :: acc)
        iter []
    in
    List.rev pids, rest

  let of_tables_bitrate (total_tbls : int)
        (br_per_pkt : float)
        (buf : Cstruct.t) : table list =
    let msg, _ = Cstruct.split buf (sizeof_table_bitrate * total_tbls) in
    let iter = Cstruct.iter (fun _ -> Some sizeof_table_bitrate) Fun.id msg in
    let tables =
      Cstruct.fold (fun acc el ->
          let packets = get_table_bitrate_packets el in
          let flags = get_table_bitrate_flags el in
          let ext_info_1 = get_table_bitrate_adv_info_1 el in
          let ext_info_2 = get_table_bitrate_adv_info_2 el in
          let rate = int_of_float @@ br_per_pkt *. (Int32.to_float packets) in
          { id = get_table_bitrate_table_id el
          ; id_ext = get_table_bitrate_table_id_ext el
          ; fully_analyzed = flags land 2 > 0
          ; section_syntax = flags land 1 > 0
          ; ext_info_1
          ; ext_info_2
          ; bitrate = rate } :: acc)
        iter []
    in
    List.rev tables

  let of_stream_bitrate (timestamp : Ptime.t) (buf : Cstruct.t) =
    let length = (Int32.to_int @@ get_stream_bitrate_length buf) in
    let msg, rest = Cstruct.split buf (length + 8) in
    let hdr, bdy = Cstruct.split msg sizeof_stream_bitrate in
    let total = Int32.to_int @@ get_stream_bitrate_ts_bitrate hdr in
    let total_pkts = get_stream_bitrate_total_packets hdr in
    let br_per_pkt = (float_of_int total) /. (Int32.to_float total_pkts)  in
    let total_pids = get_stream_bitrate_total_pids hdr in
    let total_tbls = get_stream_bitrate_total_tables hdr in
    let pids, tbls = of_pids_bitrate total_pids br_per_pkt bdy in
    let tables = of_tables_bitrate total_tbls br_per_pkt tbls in
    let stream = get_stream_bitrate_stream_id hdr in
    let stamped = { timestamp; data = { total; pids; tables }} in
    let rsp = Multi_TS_ID.of_int32_pure stream, stamped in
    let rest = if Cstruct.len rest > 0 then Some rest else None in
    rsp, rest

  let parse _ (msg : Cstruct.t) : (Multi_TS_ID.t * t timestamped) list =
    let hdr, bdy = Cstruct.split msg sizeof_bitrates in
    let count = get_bitrates_count hdr in
    let timestamp = Time.Clock.now () in
    let rec parse = fun acc buf ->
      let x, rest = of_stream_bitrate timestamp buf in
      match rest with
      | Some b -> parse (x :: acc) b
      | None -> List.rev (x :: acc) in
    if count > 0 then parse [] bdy else []

end

module Get_t2mi_info : (Request
                        with type req := t2mi_info_req
                         and type rsp := Multi_TS_ID.t * T2mi_info.t) =
  struct

    open Board_types.T2mi_info

    let req_code = 0x030B
    let rsp_code = req_code

    let serialize ({ request_id; stream_id; _ } : t2mi_info_req) =
      let body = Cstruct.create sizeof_req_get_t2mi_info in
      let ()   = set_req_get_t2mi_info_stream_id body stream_id in
      to_complex_req ~request_id ~msg_code:req_code ~body ()

    let parse ({ stream; _ } : t2mi_info_req) msg =
      let hdr, rest = Cstruct.split msg sizeof_t2mi_info in
      let iter = Cstruct.iter (fun _ -> Some 1)
                   (fun buf -> Cstruct.get_uint8 buf 0)
                   (get_t2mi_info_packets hdr) in
      let packets =
        Cstruct.fold (fun acc el ->
            (List.rev @@ int_to_bool_list el) @ acc) iter []
        |> List.rev
        |> List.foldi (fun acc i x -> if x then i :: acc else acc) [] in
      let sid = get_t2mi_info_stream_id hdr in
      let length = get_t2mi_info_length hdr in
      match length with
      | 0 ->
         stream,
         { t2mi_stream_id = sid
         ; packets
         ; t2mi_pid = None
         ; l1_pre = Error Empty
         ; l1_post_conf = Error Empty
         }
      | l ->
         let body, _ = Cstruct.split rest l in
         let conf_len =
           get_t2mi_info_ext_conf_len body
           |> fun x -> let r, d = x mod 8,
                                  x / 8 in d + (if r > 0 then 1 else 0) in
         let _, conf = Cstruct.split body sizeof_t2mi_info_ext in
         let conf, _ = Cstruct.split conf conf_len in
         let l1_pre' = Cstruct.to_string @@ get_t2mi_info_ext_l1_pre body in
         let l1_post' = Cstruct.to_string conf in
         let l1_pre = match L1_parser.l1_pre_of_string l1_pre' with
           | Some x -> Ok x
           | None -> Error (Parser_error "bad L1 pre format") in
         let l1_post = match l1_pre with
           | Error e -> Error e
           | Ok x ->
              begin match L1_parser.l1_post_conf_of_string x l1_post' with
              | None -> Error (Parser_error "bad L1 post conf format")
              | Some x -> Ok x
              end in
         stream,
         { t2mi_stream_id = sid
         ; packets
         ; t2mi_pid = Some (get_t2mi_info_ext_t2mi_pid body)
         ; l1_pre
         ; l1_post_conf = l1_post
         }

  end

(* ---------------------- Events --------------------- *)

module Status : (Event with type msg := status_raw) = struct

  let msg_code = 0x03

  let parse msg : status_raw =
    let timestamp = Time.Clock.now () in
    let iter x = Cstruct.iter (fun _ -> Some 1)
                   (fun buf -> Cstruct.get_uint8 buf 0) x in
    let flags = get_status_flags msg in
    let has_sync = not (flags land 0x04 > 0) in
    let ts_num = get_status_ts_num msg in
    let flags2 = get_status_flags_2 msg in
    let input, t2mi_mode =
      to_mode_exn (get_status_mode msg)
        (get_status_t2mi_pid msg)
        (Multi_TS_ID.of_int32_pure @@ get_status_t2mi_stream_id msg) in
    { status =
        { timestamp
        ; load = (float_of_int ((get_status_load msg) * 100)) /. 255.
        ; ts_num = if has_sync then ts_num else 0
        ; services_num = if has_sync then get_status_services_num msg else 0
        ; bitrate = Int32.to_int @@ get_status_bitrate msg
        ; packet_sz = if (flags land 0x08) <> 0 then Ts192
                      else if (flags land 0x10) <> 0 then Ts204
                      else Ts188
        ; has_sync
        ; has_stream = flags land 0x80 = 0
        }
    ; reset = flags2 land 0x02 <> 0
    ; input
    ; t2mi_mode
    ; jitter_mode =
        (let pid = get_status_jitter_pid msg in
         let id = get_status_jitter_stream_id msg in
         if not @@ Int.equal pid 0x1fff
         then Some { stream = Multi_TS_ID.of_int32_pure id; pid }
         else None)
    ; errors = flags land 0x20 <> 0
    ; t2mi_sync =
        int_to_bool_list (get_status_t2mi_sync msg)
        |> (fun l -> List.foldi (fun acc i x ->
                         if x then i :: acc else acc) [] l)
    ; version = get_status_version msg
    ; versions =
        { streams_ver = get_status_streams_ver msg
        ; ts_ver_com = get_status_ts_ver_com msg
        ; ts_ver_lst = Cstruct.fold (fun acc el -> el :: acc)
                         (iter @@ get_status_ts_ver_lst msg) []
                       |> List.rev
                       |> List.take ts_num
        ; t2mi_ver_lst = get_status_t2mi_ver_lst msg
                         |> (fun v -> List.map (fun x ->
                                          Int32.shift_right v (4 * x)
                                          |> Int32.logand 0xfl
                                          |> Int32.to_int)
                                        (List.range 0 7))
        }
    ; streams = [] (* Filled in later *)
    }

end

module TS_streams : (Event with type msg := Multi_TS_ID.t list) = struct

  let msg_code = 0x0B

  let parse msg =
    let hdr, bdy' = Cstruct.split msg sizeof_streams_list_event in
    let count = get_streams_list_event_count hdr in
    let bdy, _ = Cstruct.split bdy' (count * 4) in
    let iter =
      Cstruct.iter (fun _ -> Some 4)
        (fun buf -> Multi_TS_ID.of_int32_pure
                    @@ Cstruct.LE.get_uint32 buf 0) bdy in
    List.rev @@ Cstruct.fold (fun acc el -> el :: acc) iter []

end

module Ts_errors : (Event with type msg := Multi_TS_ID.t * (Error.t list)) = struct

  open Board_types.Error

  let msg_code = 0x04

  let prioriry_of_err_code = function
    | x when x >= 0x11 && x <= 0x16 -> 1
    | x when x >= 0x21 && x <= 0x26 -> 2
    | x when x >= 0x31 && x <= 0x38 -> 3
    | _ -> 0 (* Unknown *)

  let compare = fun x y ->
    match Time.compare x.timestamp y.timestamp with
    | 0 -> Int32.compare x.packet y.packet
    | x -> x

  let parse msg : Multi_TS_ID.t * (t list) =
    let common, rest = Cstruct.split msg sizeof_ts_errors in
    let number = get_ts_errors_count common in
    let errors, _ = Cstruct.split rest (number * sizeof_ts_error) in
    let stream_id = Multi_TS_ID.of_int32_pure
                    @@ get_ts_errors_stream_id common in
    let timestamp = Time.Clock.now () in
    let iter = Cstruct.iter (fun _ -> Some sizeof_ts_error) (fun x -> x) errors in
    Cstruct.fold (fun acc el ->
        let pid' = get_ts_error_pid el in
        let pid = pid' land 0x1FFF in
        let err_code = get_ts_error_err_code el in
        { timestamp
        ; count = get_ts_error_count el
        ; err_code
        ; err_ext = get_ts_error_err_ext el
        ; priority = prioriry_of_err_code err_code
        ; multi_pid = (pid' land 0x8000) > 0
        ; pid
        ; service = None (* Filled in later *)
        ; packet = get_ts_error_packet el
        ; param_1 = get_ts_error_param_1 el
        ; param_2 = get_ts_error_param_2 el
        } :: acc) iter []
    |> List.sort compare
    |> Pair.make stream_id

end

module T2mi_errors : (Event with type msg := Multi_TS_ID.t * (Error.t list)) = struct

  open Board_types.Error

  let msg_code = 0x05

  let ts_parser_error_code   = 0xF0
  let t2mi_parser_error_code = 0xF1

  let get_relevant_t2mi_adv_code = function
    | 0 -> Some 2 | 1 -> Some 3 | 4 -> Some 4
    | 5 -> Some 5 | 6 -> Some 6 | 9 -> Some 7
    | 20 -> Some 8 | _ -> None

  (* Merge t2mi errors with counter and advanced errors.
     Result is common t2mi error type *)
  let merge timestamp pid (count : t2mi_error_raw list)
        (param : t2mi_error_adv_raw list) : t list =
    List.map (fun (x : t2mi_error_raw) ->
        let open Option in
        let param =
          get_relevant_t2mi_adv_code x.code
          |> flat_map (fun c ->
                 List.find_opt (fun a ->
                     a.code = c
                     && a.stream_id = x.stream_id) param
                 |> map (fun (x : t2mi_error_adv_raw) -> Int32.of_int x.param))
          |> get_or ~default:0l
        in
        { timestamp
        ; count = x.count
        ; err_code = x.code
        ; err_ext = 0
        ; priority = 0
        ; multi_pid = false
        ; pid
        ; service = None
        ; packet = 0l
        ; param_1 = param
        ; param_2 = Int32.of_int x.stream_id }) count

  (* Convert t2mi advanced errors with 'code' = 0 to common t2mi error type *)
  let convert_other timestamp pid (other : t2mi_error_adv_raw list) : t list =
    List.map (fun (x : t2mi_error_adv_raw) ->
        { timestamp
        ; count = 1
        ; err_code = t2mi_parser_error_code
        ; err_ext = 0
        ; priority = 0
        ; multi_pid = false
        ; pid
        ; service = None
        ; packet = 0l
        ; param_1 = Int32.of_int x.param
        ; param_2 = Int32.of_int x.stream_id }) other

  (* Convert ts parser flags to common t2mi error type *)
  let convert_ts timestamp pid (ts : int list) : t list =
    List.map (fun (x : int) ->
        { timestamp
        ; count = 1
        ; err_code = ts_parser_error_code
        ; err_ext = 0
        ; priority = 0
        ; multi_pid = false
        ; pid
        ; service = None
        ; packet = 0l
        ; param_1 = Int32.of_int x
        ; param_2 = 0l }) ts

  let compare = fun x y -> Time.compare x.timestamp y.timestamp

  let parse msg : Multi_TS_ID.t * (t list) =
    let timestamp = Time.Clock.now () in
    let common, rest = Cstruct.split msg sizeof_t2mi_errors in
    let number = get_t2mi_errors_count common in
    let errors, _ = Cstruct.split rest (number * sizeof_t2mi_error) in
    let stream_id = get_t2mi_errors_stream_id common in
    let pid = get_t2mi_errors_pid common in
    let _ = int_to_t2mi_sync_list (get_t2mi_errors_sync common) in
    let iter = Cstruct.iter (fun _ -> Some sizeof_t2mi_error)
                 (fun buf -> buf) errors in
    let cnt,adv,oth =
      Cstruct.fold (fun (cnt, adv, oth) el ->
          let index = get_t2mi_error_index el in
          let data = get_t2mi_error_data el in
          let code = index lsr 4 in
          let sid = index land 7 in
          match index land 8 with
          | 0 -> if data > 0 (* filter zero errors *)
                 then
                   let (x : Types.t2mi_error_raw) =
                     { code; stream_id = sid; count = data } in
                   (x :: cnt), adv, oth
                 else cnt,adv,oth
          | _ -> let f x = { code = x; stream_id = sid; param = data } in
                 if code = 0 (* t2mi parser error *)
                 then cnt, adv, ((f t2mi_parser_error_code) :: oth)
                 else cnt, ((f code) :: adv), oth)
        iter ([], [], [])
    in
    let pe = get_t2mi_errors_err_flags common in
    let ts = List.filter_map (fun x ->
                 if pe land (Int.pow 2 x) <> 0
                 then Some x else None)
               (List.range 0 3) in
    let errors = merge timestamp pid cnt adv
                 @ convert_other timestamp pid oth
                 @ convert_ts timestamp pid ts
                 |> List.sort compare
    in
    Multi_TS_ID.of_int32_pure stream_id, errors

end


(* ----------------- Message deserialization ---------------- *)

let try_parse f x = try Some (f x) with _ -> None

let parse_get_board_info = function
  | `Board_info buf -> try_parse (Get_board_info.parse ()) buf
  |  _ -> None

let parse_get_board_mode = function
  | `Board_mode buf -> try_parse (Get_board_mode.parse ()) buf
  | _ -> None

let parse_get_board_errors id = function
  | `Board_errors (r_id, buf) when r_id = id ->
     Option.map (fun x -> Board_errors x)
     @@ try_parse (Get_board_errors.parse id) buf
  | _ -> None

let parse_get_section (req : section_req) = function
  | `Section (r_id, buf) when r_id = req.request_id ->
     try_parse (Get_section.parse req) buf
  | _ -> None

let parse_get_t2mi_frame_seq (req : t2mi_frame_seq_req) = function
  | `T2mi_frame_seq (r_id, buf) when r_id = req.request_id ->
     try_parse (Get_t2mi_frame_seq.parse req) buf
  | _ -> None

let parse_get_jitter (({ request_id = id
                       ; pointer = ptr
                       ; _ } as req) : jitter_req) = function
  | `Jitter (r_id, pointer, buf) when id = r_id && Int32.equal ptr pointer ->
     Option.map (fun x -> Jitter x)
     @@ try_parse (Get_jitter.parse req) buf
  | _ -> None

let parse_get_ts_struct (req : ts_struct_req) = function
  | `Struct (r_id, _, buf) when r_id = req.request_id ->
     Option.map (fun x -> Struct x)
     @@ try_parse (Get_ts_structs.parse req) buf
  | _ -> None

let parse_get_bitrates id = function
  | `Bitrates (r_id, _, buf) when id = r_id ->
     Option.map (fun x -> Bitrate x)
     @@ try_parse (Get_bitrates.parse id) buf
  | _ -> None

let parse_get_t2mi_info (req : t2mi_info_req) = function
  | `T2mi_info (r_id, _, stream_id, buf)
       when r_id = req.request_id && stream_id = req.stream_id ->
     Option.map (fun x -> T2mi_info x)
     @@ try_parse (Get_t2mi_info.parse req) buf
  | _ -> None

let is_response (type a) (req : a request) msg : a option =
  match req with
  | Get_board_info -> parse_get_board_info msg
  | Get_board_mode -> parse_get_board_mode msg
  | Get_t2mi_frame_seq x -> parse_get_t2mi_frame_seq x msg
  | Get_section x -> parse_get_section x msg

let is_probe_response (type a) (req : a probe_request) msg : a option =
  match req with
  | Get_board_errors id -> parse_get_board_errors id msg
  | Get_jitter x -> parse_get_jitter x msg
  | Get_ts_struct x -> parse_get_ts_struct x msg
  | Get_bitrates x -> parse_get_bitrates x msg
  | Get_t2mi_info x -> parse_get_t2mi_info x msg

module Make(Logs:Logs.LOG) = struct

  type err =
    | Bad_prefix of int
    | Bad_length of int
    | Bad_msg_code of int
    | Bad_crc of int * int * int
    | No_prefix_after_msg of int
    | Insufficient_payload of Cstruct.t
    | Unknown_err of string

  let err_to_string = function
    | Bad_prefix x -> "incorrect prefix: " ^ (string_of_int x)
    | Bad_length x -> "incorrect length: " ^ (string_of_int x)
    | Bad_msg_code x -> "incorrect code: " ^ (string_of_int x)
    | Bad_crc (code, x, y) ->
       Printf.sprintf "incorrect crc in msg with code = 0x%x,\
                       expected %d, got %d"
         code x y
    | No_prefix_after_msg x ->
       Printf.sprintf "no prefix found after message payload, code = 0x%x" x
    | Insufficient_payload _ -> "insufficient payload"
    | Unknown_err s -> s

  let check_prefix buf =
    let prefix' = get_common_header_prefix buf in
    if prefix <> prefix' then Error (Bad_prefix prefix') else Ok buf

  let check_msg_code buf =
    let hdr, rest = Cstruct.split buf sizeof_common_header in
    let code = get_common_header_msg_code hdr in
    let has_crc = (code land 2) > 0 in
    let length = match code lsr 8 with
      | x when x = Get_board_info.rsp_code ->
         Some sizeof_board_info
      | x when x = Get_board_mode.rsp_code ->
         Some sizeof_board_mode
      | x when x = Status.msg_code ->
         Some sizeof_status
      | x when x = Ts_errors.msg_code ->
         Some ((get_ts_errors_length rest * 2) + 2)
      | x when x = T2mi_errors.msg_code ->
         Some ((get_t2mi_errors_length rest * 2) + 2)
      | x when x = TS_streams.msg_code ->
         Some ((get_streams_list_event_length rest * 2) + 2)
      | 0x09 -> Some ((get_complex_rsp_header_length rest * 2) + 2)
      | 0xFD -> Some 4 (* end of errors *)
      | 0xFF -> Some 0 (* end of transmission *)
      | _    -> None in
    match length with
    | Some x -> Ok (x + (if has_crc then 2 else 0), has_crc, code, rest)
    | None -> Error (Bad_msg_code code)

  let check_length (len, has_crc, code, rest') =
    if len > 512 - sizeof_common_header then Error (Bad_length len) else
      let body, rest = Cstruct.split rest' len in
      Ok (has_crc, code, body, rest)

  let check_next_prefix ((code, _, rest) as x) =
    if Cstruct.len rest < sizeof_common_header then Ok x
    else (match check_prefix rest with
          | Ok _    -> Ok x
          | Error _ -> Error (No_prefix_after_msg code))

  let check_crc (code,body,rest) =
    let b = Cstruct.create 2 |>
              (fun b -> Cstruct.LE.set_uint16 b 0 code; b) in
    let body, crc' = Cstruct.(split body (len body - 2)) in
    let iter =
      Cstruct.iter (fun _ -> Some 2)
        (fun buf -> Cstruct.LE.get_uint16 buf 0)
        (Cstruct.append b body) in
    let crc = (Cstruct.fold (fun acc el -> el + acc) iter 0) land 0xFFFF in
    let crc' = Cstruct.LE.get_uint16 crc' 0 in
    if crc <> crc' then Error (Bad_crc (code, crc, crc'))
    else Ok (code, body, rest)

  let get_msg buf =
    try
      Result.(
      check_prefix buf
      >>= check_msg_code
      >>= check_length
      >>= (fun (has_crc, code, body, rest) ->
        if has_crc then check_crc (code, body, rest)
        else check_next_prefix (code, body, rest)))
    with
    | Invalid_argument _ -> Error (Insufficient_payload buf)
    | e -> Error (Unknown_err (Printexc.to_string e))

  let parse_simple_msg = fun (code, body, parts) ->
    try
      begin match code lsr 8 with
      | x when x = Get_board_info.rsp_code ->
         Logs.debug (fun m -> m "deserializer - got board info");
         `R (`Board_info body)
      | x when x = Get_board_mode.rsp_code ->
         Logs.debug (fun m -> m "deserializer - got board mode");
         `R (`Board_mode body)
      | x when x = Status.msg_code ->
         Logs.debug (fun m -> m "deserializer - got status");
         `E (`Status (Status.parse body))
      | x when x = Ts_errors.msg_code ->
         Logs.debug (fun m -> m "deserializer - got ts errors");
         `E (`Ts_errors (Ts_errors.parse body))
      | x when x = T2mi_errors.msg_code ->
         Logs.debug (fun m -> m "deserializer - got t2mi errors");
         `E (`T2mi_errors (T2mi_errors.parse body))
      | x when x = TS_streams.msg_code ->
         Logs.debug (fun m -> m "deserializer - got streams");
         `E (`Streams_event (TS_streams.parse body))
      | 0xFD ->
         Logs.debug (fun m -> m "deserializer - got end of errors");
         `E `End_of_errors
      | 0xFF ->
         Logs.debug (fun m -> m "deserializer - got end of transmission");
         `E `End_of_transmission (* exit from receive loop *)
      | 0x09 ->
         let code_ext = get_complex_rsp_header_code_ext body in
         let long = code_ext land 0x2000 <> 0 in
         let parity = if code_ext land 0x1000 <> 0 then 1 else 0 in
         let first = code_ext land 0x8000 <> 0 in
         let code = code_ext land 0x0FFF in
         let req_id = get_complex_rsp_header_request_id body in
         let data' =
           if long then Cstruct.shift body sizeof_complex_rsp_header_ext
           else Cstruct.shift body sizeof_complex_rsp_header in
         let data, _ = Cstruct.(split data' (len data' - parity)) in
         let param =
           Int32.mul 2l
             (if long then get_complex_rsp_header_ext_param body
              else Int32.of_int @@ get_complex_rsp_header_param body)
           |> fun x -> Int32.sub x (Int32.of_int parity) in
         Logs.debug (fun m ->
             m "deserializer - got complex message part \
                (first: %B, code: 0x%X, parity: %d, req_id: %d, \
                length: %d, param: %ld)"
               first code parity req_id (Cstruct.len data) param);
         let part = { data; first; param } in
         `P (List.Assoc.update (code, req_id) ~eq:(Pair.equal (=) (=))
               ~f:(function
                 | Some x -> Some (part :: x)
                 | None   -> Some [ part ])
               parts)
      | _ -> Logs.debug (fun m ->
                 m "deserializer - unknown simple message code: 0x%x" code);
             `N
      end
    with e ->
      Logs.warn (fun m ->
          m "deserializer - failure while parsing simple message: %s"
          @@ Printexc.to_string e);
      `N

  let parse_complex_msg = fun ((code, r_id), (msg:Cstruct.t)) ->
    try
      let data = (r_id, msg) in
      (match code with
       | x when x = Get_board_errors.rsp_code ->
          Logs.debug (fun m -> m "deserializer - got board errors");
          `ER (`Board_errors data)
       | x when x = Get_section.rsp_code ->
          Logs.debug (fun m -> m "deserializer - got section");
          `R (`Section data)
       | x when x = Get_t2mi_frame_seq.rsp_code ->
          Logs.debug (fun m -> m "deserializer - got t2mi frame sequence");
          `R (`T2mi_frame_seq data)
       | x when x = Get_jitter.rsp_code ->
          Logs.debug (fun m -> m "deserializer - got jitter");
          `ER (`Jitter (r_id, (get_jitter_req_ptr msg), msg))
       | x when x = Get_ts_structs.rsp_code ->
          Logs.debug (fun m -> m "deserializer - got ts structs");
          `ER (`Struct (r_id, (get_ts_structs_version msg), msg))
       | x when x = Get_bitrates.rsp_code ->
          Logs.debug (fun m -> m "deserializer - got bitrates");
          `ER (`Bitrates (r_id, (get_bitrates_version msg), msg))
       | x when x = Get_t2mi_info.rsp_code ->
          Logs.debug (fun m -> m "deserializer - got t2mi info");
          `ER (`T2mi_info (r_id,
                           (get_t2mi_info_version msg),
                           (get_t2mi_info_stream_id msg),
                           msg))
       | _ -> Logs.debug (fun m ->
                  m "deserializer - unknown complex message code: 0x%x" code); `N)
    with e ->
      Logs.warn (fun m ->
          m "deserializer - failure while parsing complex message: %s"
            (Printexc.to_string e)); `N

  let try_compose_parts ((id, gp) as x) =
    let open Int32 in
    let gp = List.sort (fun x y ->
                 if x.first then (-1)
                 else if y.first then 1
                 else compare x.param y.param) gp in
    let first, rest = List.hd_tl gp in
    try
      let acc =
        List.fold_left (fun acc x ->
            if equal x.param (of_int (Cstruct.len acc))
            then Cstruct.append acc x.data
            else failwith "Incorrect part offset")
          first.data rest
      in
      if equal first.param (of_int (Cstruct.len acc))
      then `F (id, acc) else `P x
    with e ->
      Logs.warn (fun m ->
          m "deserializer - failure while composing complex message parts: %s"
          @@ Printexc.to_string e); `N

  let deserialize parts buf =
    let rec f events event_rsps rsps parts b =
      if Cstruct.len b < sizeof_common_header
      then List.(rev events, rev event_rsps, rev rsps, rev parts, b)
      else
        match get_msg b with
        | Ok (code, body, rest) ->
           begin match parse_simple_msg (code, body, parts) with
           | `E x -> f (x :: events) event_rsps rsps parts rest
           | `ER x -> f events (x :: event_rsps) rsps parts rest
           | `R x -> f events event_rsps (x :: rsps) parts rest
           | `P x -> f events event_rsps rsps x rest
           | `N -> f events event_rsps rsps parts rest
           end
        | Error e ->
           begin match e with
           | Insufficient_payload x ->
              List.(rev events, rev event_rsps, rev rsps, rev parts, x)
           | e ->
              Logs.warn (fun m -> m "deserializer - composer error: %s"
                                  @@ err_to_string e);
              f events event_rsps rsps parts (Cstruct.shift b 1)
           end in
    let ev, ev_rsps, rsps, parts, res = f [] [] [] parts buf in
    let parts =
      List.filter (fun (_, x) ->
          let first_msgs = List.find_all (fun x -> x.first) x in
          match first_msgs with
          | [_] -> true
          | _   -> false) parts in
    let ev_rsps, rsps, parts =
      List.fold_left (fun ((e, r, p) as acc) x ->
          match try_compose_parts x with
          | `F x ->
             begin match parse_complex_msg x with
             | `ER x -> x :: e, r, p
             | `R x -> e, x :: r, p
             | `N -> acc
             end
          | `P x -> e, r, x :: p
          | `N -> e, r, p)
        (ev_rsps, rsps, []) parts
    in
    ev, ev_rsps, rsps, parts, if Cstruct.len res > 0 then Some res else None

end
