open Board_niitv_tsan_types
open Application_types

module List = Boards.Util.List

let ( % ) f g x = f (g x)

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
  }

let of_general_block (msg : Cstruct.t) : Ts_info.t * int =
  let bdy, rest = Cstruct.split msg Message.sizeof_general_struct_block in
  let string_len = Message.get_general_struct_block_string_len bdy in
  let nw_pid' = Message.get_general_struct_block_network_pid bdy in
  let strings, _ = Cstruct.split rest (string_len * 2) in
  let nw_name, bq_name = Cstruct.split strings string_len in
  let nw_name = match Text_decoder.decode nw_name with
    | Error _ -> ""
    | Ok s -> s in
  let bouquet_name = match Text_decoder.decode bq_name with
    | Error _ -> ""
    | Ok s -> s in
  { services_num = Message.get_general_struct_block_services_num bdy
  ; nw_pid = nw_pid' land 0x1FFF
  ; complete = (nw_pid' land 0x4000) <> 0
  ; ts_id = Message.get_general_struct_block_ts_id bdy
  ; nw_id = Message.get_general_struct_block_nw_id bdy
  ; orig_nw_id = Message.get_general_struct_block_orig_nw_id bdy
  ; nw_name
  ; bouquet_name
  }, string_len

let of_pids_block (msg : Cstruct.t) : Pid.t list =
  let open Pid in
  let iter = Cstruct.iter (fun _ -> Some 2)
      (fun buf -> Cstruct.LE.get_uint16 buf 0) msg in
  Cstruct.fold (fun acc el ->
      let pid = el land 0x1FFF in
      let description =
        { has_pts = (el land 0x8000) <> 0
        ; has_pcr = false (* Filled in later *)
        ; scrambled = (el land 0x4000) <> 0
        ; typ = Private (* Filled in later *)
        ; service_id = None (* Filled in later *)
        ; service_name = None
        ; present = (el land 0x2000) <> 0 } in
      (pid, description) :: acc) iter []
  |> List.sort (fun (a, _) (b, _) -> Pervasives.compare a b)

let of_service_block (string_len : int)
    (msg : Cstruct.t) : Service.t =
  let bdy, rest = Cstruct.split msg Message.sizeof_services_struct_block in
  let flags = Message.get_services_struct_block_flags bdy in
  let strings, _ = Cstruct.split rest (string_len * 2) in
  let sn, pn = Cstruct.split strings string_len in
  let id = Message.get_services_struct_block_id bdy in
  let name = match Text_decoder.decode sn with
    | Error _ -> Printf.sprintf "Service %d" id
    | Ok s -> s in
  let provider_name = match Text_decoder.decode pn with
    | Error _ -> ""
    | Ok s -> s in
  id, { name
      ; provider_name
      ; pmt_pid = Message.get_services_struct_block_pmt_pid bdy
      ; pcr_pid = Message.get_services_struct_block_pcr_pid bdy
      ; has_pmt = (flags land 0x8000) <> 0
      ; has_sdt = (flags land 0x4000) <> 0
      ; dscr = (flags land 0x2000) <> 0
      ; dscr_list = (flags land 0x1000) <> 0
      ; eit_schedule = (flags land 0x0080) <> 0
      ; eit_pf = (flags land 0x0040) <> 0
      ; free_ca_mode = (flags land 0x0020) <> 0
      ; running_status = flags land 0x0007
      ; service_type = Message.get_services_struct_block_service_type bdy
      ; service_type_list = Message.get_services_struct_block_service_type_list bdy
      ; elements = [] (* Filled in later *)
      }

let of_es_block (msg : Cstruct.t) : Service.element list =
  let iter =
    Cstruct.iter
      (fun _ -> Some 4)
      (fun buf -> buf)
      msg in
  List.rev
  @@ Cstruct.fold (fun acc x ->
      let pid = (Message.get_es_struct_block_pid x) land 0x1FFF in
      let info =
        Mpeg_ts.Pid.Type.PES
          { stream_type = Message.get_es_struct_block_es_type x
          ; stream_id = Message.get_es_struct_block_es_stream_id x } in
      (pid, info) :: acc) iter []

let of_ecm_block (msg : Cstruct.t) : Service.element list =
  let iter = Cstruct.iter (fun _ -> Some 4) (fun buf -> buf) msg in
  List.rev
  @@ Cstruct.fold (fun acc x ->
      let pid = (Message.get_ecm_struct_block_pid x) land 0x1FFF in
      let ca_sys_id = Message.get_ecm_struct_block_ca_system_id x in
      let info = Mpeg_ts.Pid.Type.ECM { ca_sys_id } in
      (pid, info) :: acc) iter []

let of_emm_block (msg : Cstruct.t) : Service.element list =
  of_ecm_block msg

let of_table_block (msg : Cstruct.t) : SI_PSI_table.t =
  let open SI_PSI_table in
  let bdy, rest = Cstruct.split msg Message.sizeof_table_struct_block in
  let iter = Cstruct.iter (fun _ -> Some 2)
      (fun buf -> Cstruct.LE.get_uint16 buf 0) rest in
  let sections =
    Cstruct.fold (fun acc x ->
        let section = List.length acc in
        (* let analyzed = (x land 0x8000) > 0 in *)
        let length = (x land 0x0FFF) in
        ({ section; length } :: acc)) iter []
    |> List.filter (fun ({ length; _ }) -> length > 0)
    |> List.rev in
  let pid' = Message.get_table_struct_block_pid msg in
  let table_id = Message.get_table_struct_block_table_id bdy in
  let table_id_ext = Message.get_table_struct_block_table_id_ext bdy in
  let id_ext_1 = Message.get_table_struct_block_id_ext_1 bdy in
  let id_ext_2 = Message.get_table_struct_block_id_ext_2 bdy in
  let eit_segment_lsn = Message.get_table_struct_block_eit_segment_lsn bdy in
  let eit_last_table_id = Message.get_table_struct_block_eit_last_table_id bdy in
  let version = Message.get_table_struct_block_version bdy in
  let pid = pid' land 0x1FFF in
  let last_section = Message.get_table_struct_block_lsn bdy in
  let section_syntax = (pid' land 0x8000) > 0 in
  let (id : id) =
    { table_id
    ; table_id_ext
    ; id_ext_1
    ; id_ext_2
    } in
  let (info : info) =
    { version
    ; pid
    ; service_id = None (* Filled in later *)
    ; service_name = None
    ; last_section
    ; section_syntax
    ; eit_segment_lsn
    ; eit_last_table_id
    ; sections
    } in
  id, info

let split_into_blocks (msg : Cstruct.t) : acc =
  let rec aux service_flag msg acc = match Cstruct.len msg with
    | 0 -> acc
    | _ ->
      let hdr, data = Cstruct.split msg Message.sizeof_struct_block_header in
      let len = Message.get_struct_block_header_length hdr in
      let block, rest = Cstruct.split data len in
      let acc, flag = match Message.get_struct_block_header_code hdr with
        | 0x2000 -> { acc with general = block }, false
        | 0x2100 -> { acc with pids = Cstruct.append acc.pids block }, false
        | 0x2200 ->
          let service = { service_acc_empty with info = block } in
          { acc with services = service :: acc.services }, true
        | 0x2201 ->
          begin match service_flag, acc.services with
            | false, _ | _, [] ->
              failwith "no service block before es block"
            | _, ({ es; _ } as hd) :: tl ->
              let es = Cstruct.append es block in
              { acc with services = { hd with es } :: tl }, true
          end
        | 0x2202 ->
          begin match service_flag, acc.services with
            | false, _ | _, [] ->
              failwith "no service block before ecm block"
            | _, ({ ecm; _ } as hd) :: tl ->
              let ecm = Cstruct.append ecm block in
              { acc with services = { hd with ecm } :: tl }, true
          end
        | 0x2300 ->
          { acc with emm = Cstruct.append acc.emm block }, false
        | 0x2400 -> { acc with tables = block :: acc.tables }, false
        | x ->
          let s = Printf.sprintf "unknown structure block: 0x%X" x in
          failwith s in
      aux flag rest acc
  in
  aux false msg acc_empty

let parse_service (slen : int)
    (acc : service_acc) : Service.t * (Service.element list) =
  let es = of_es_block acc.es in
  let ecm = of_ecm_block acc.ecm in
  let elements = List.map fst @@ es @ ecm in
  let sid, sinfo = of_service_block slen acc.info in
  (sid, { sinfo with elements }), (es @ ecm)

let update_if_null (pid, info) : Pid.t option =
  if pid = 0x1FFF
  then Some (pid, { info with typ = Null }) else None

let find_in_elements
    ((pid, _) : Pid.t)
    (elements : Service.element list) : Mpeg_ts.Pid.Type.t option =
  let pid = List.find_opt (fun (pid', _) -> pid' = pid) elements in
  match pid with
  | None -> None
  | Some (_, info) -> Some info

let update_if_in_services (elements : (Service.t * (Service.element list)) list)
    ((pid, info) : Pid.t) : (Pid.t) option =
  let result =
    List.find_map (fun (((sid, sinfo), elts) : Service.t * Service.element list) ->
        match find_in_elements (pid, info) elts with
        | None -> None
        | Some t -> Some (sid, sinfo.name, sinfo.pcr_pid, t)) elements in
  match result with
  | None -> None
  | Some (id, name, pcr_pid, typ) ->
    let has_pcr = pid = pcr_pid in
    Some (pid, { info with service_id = Some id
                         ; service_name = Some name
                         ; typ; has_pcr })

let update_if_in_emm (emm : Service.element list)
    (pid, info) : Pid.t option =
  match find_in_elements (pid, info) emm with
  | None -> None
  | Some typ -> Some (pid, { info with typ })

let update_if_in_tables (tables : SI_PSI_table.t list)
    ((pid, info) : Pid.t) : Pid.t option =
  let open SI_PSI_table in
  List.find_all (fun ((_, info) : t) ->
      info.pid = pid) tables
  |> (function
      | [] -> None
      | [(id, info)] ->
        let open Mpeg_ts.Pid.Type in
        begin match Mpeg_ts.table_of_int id.table_id with
          | `PMT -> Some (info.service_id, info.service_name, SEC [id.table_id])
          | _ -> Some (None, None, SEC [id.table_id])
        end
      | l ->
        let ids =
          List.map (fun ((id, _) : t) -> id.table_id) l
          |> List.sort_uniq Pervasives.compare in
        Some (None, None, SEC ids))
  |> function
  | None -> None
  | Some (id, name, typ) ->
    Some (pid, { info with service_id = id
                         ; service_name = name
                         ; typ })

let update_pid (elements : (Service.t * Service.element list) list)
    (tables : SI_PSI_table.t list)
    (emm : Service.element list)
    (pid : Pid.t) : Pid.t =
  let ( >>= ) x f = match x with Some x -> Some x | None -> f pid in
  update_if_null pid
  >>= update_if_in_services elements
  >>= update_if_in_emm emm
  >>= update_if_in_tables tables
  |> function None -> pid | Some x -> x

let update_table (services : Service.t list)
    ((id, info) : SI_PSI_table.t) : SI_PSI_table.t =
  let result =
    match Mpeg_ts.table_of_int id.table_id with
    | `EIT (`Actual, _) ->
      let service = List.find_opt (fun (sid, _) ->
          sid = id.table_id_ext) services in
      (match service with
       | None -> None
       | Some (id, info) -> Some (id, info.name))
    | `PMT ->
      let service = List.find_opt (fun ((sid, sinfo) : Service.t) ->
          sinfo.has_pmt && sinfo.pmt_pid = info.pid) services in
      (match service with
       | None -> None
       | Some (id, info) -> Some (id, info.name))
    | _ -> None in
  match result with
  | None -> id, info
  | Some (sid, name) ->
    id, { info with service_id = Some sid
                  ; service_name = Some name }

let parse_blocks (msg : Cstruct.t) =
  let acc = split_into_blocks msg in
  let info, slen = of_general_block acc.general in
  let elements =
    List.map (parse_service slen) acc.services in
  let services =
    List.split elements
    |> fst
    |> List.sort (fun (a, _) (b, _) -> compare a b) in
  let tables =
    List.rev_map (update_table services % of_table_block)
      acc.tables in
  let emm = of_emm_block acc.emm in
  let pids = List.map (update_pid elements tables emm)
    @@ of_pids_block acc.pids in
  { Request. info; services; tables; pids; time = Time.epoch }

let of_ts_struct msg =
  let header, rest = Cstruct.split msg Message.sizeof_ts_struct in
  let length = (Int32.to_int @@ Message.get_ts_struct_length header) in
  let body, rest = Cstruct.split rest length in
  let structure = parse_blocks body in
  let stream =
    Stream.Multi_TS_ID.of_int32_pure
    @@ Message.get_ts_struct_stream_id header in
  (stream, structure), rest

let parse stream (msg : Cstruct.t) =
  try
    let hdr, bdy' = Cstruct.split msg Message.sizeof_ts_structs in
    let count = Message.get_ts_structs_count hdr in
    (* stream id list *)
    let bdy = snd @@ Cstruct.split bdy' (count * 4) in
    (* FIXME stuffing bytes when requesting for a single stream *)
    let parse = match stream with
      | `All ->
        let rec aux = fun acc buf ->
          match Cstruct.len buf with
          | 0 -> List.rev acc
          | _ ->
            let x, rest = of_ts_struct buf in
            aux (x :: acc) rest in
        aux []
      | `Single _ -> fun buf -> let s, _ = of_ts_struct buf in [s] in
    Ok (if count > 0 then parse bdy else [])
  with Invalid_argument _ -> Error Request.Invalid_payload
