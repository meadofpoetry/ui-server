open Containers
open Board_msg_formats
open Board_qos_types
open Common
open Boards.Pools

type sender = Cstruct.t -> unit Lwt.t

let request_id = ref (-1)

let get_request_id () =
  incr request_id;
  !request_id

module Make(Logs : Logs.LOG) = struct

  let to_common_header ~code () =
    let hdr = Cstruct.create sizeof_common_header in
    set_common_header_prefix hdr prefix;
    set_common_header_msg_code hdr code;
    hdr

  let to_complex_req_header ?(client_id = 0) ?(request_id = 0)
        ~code ~length () =
    let hdr = to_common_header ~code () in
    let complex_hdr = Cstruct.create sizeof_complex_req_header in
    set_complex_req_header_client_id complex_hdr client_id;
    set_complex_req_header_length complex_hdr length;
    set_complex_req_header_request_id complex_hdr request_id;
    Cstruct.append hdr complex_hdr

  let to_simple_req ~code ~body () =
    let hdr = to_common_header ~code () in
    Cstruct.append hdr body

  let to_complex_req ?client_id ?request_id ~code ~body () =
    let length = (Cstruct.len body / 2) + 1 in
    let hdr = to_complex_req_header ?client_id ?request_id ~code ~length () in
    Cstruct.append hdr body

  module Set_board_init = struct
    let code = 0x0089

    let serialize (src : init) : Cstruct.t =
      let body = Cstruct.create sizeof_req_set_init in
      set_req_set_init_input_src_id body src.input;
      set_req_set_init_t2mi_src_id body src.t2mi;
      to_simple_req ~code ~body ()
  end

  module Set_board_mode = struct
    let code = 0x0082

    let serialize ((input : input), (mode : t2mi_mode_raw)) : Cstruct.t =
      let { pid; enabled; stream; t2mi_stream_id } = mode in
      let body = Cstruct.create sizeof_board_mode in
      let pid = (t2mi_stream_id lsl 13) lor (pid land 0x1FFF) in
      input_to_int input
      |> (lor) (if enabled then 4 else 0)
      |> (lor) 8 (* disable board storage by default *)
      |> set_board_mode_mode body;
      set_board_mode_t2mi_pid body pid;
      set_board_mode_stream_id body @@ Stream.Multi_TS_ID.to_int32_pure stream;
      to_simple_req ~code ~body ()

  end

  module Set_jitter_mode = struct
    let code = 0x0112

    let serialize (mode : jitter_mode option) : Cstruct.t =
      let pid, stream = match mode with
        | None -> 0, 0l
        | Some m -> m.pid, Stream.Multi_TS_ID.to_int32_pure m.stream in
      let body = Cstruct.create sizeof_req_set_jitter_mode in
      set_req_set_jitter_mode_stream_id body stream;
      set_req_set_jitter_mode_pid body pid;
      to_complex_req ~code ~body ()
  end

  module Get_board_info = struct
    let code = 0x0080

    let serialize () : Cstruct.t =
      to_common_header ~code ()
  end

  module Get_board_mode = struct
    let code = 0x0081

    let serialize () : Cstruct.t =
      to_common_header ~code ()
  end

  module Get_board_errors = struct
    let code = 0x0110

    let serialize (request_id : int) : Cstruct.t =
      let body = Cstruct.create 0 in
      to_complex_req ~request_id ~code ~body ()
  end

  module Get_section = struct
    let code = 0x0302

    let serialize ({ request_id; params } : section_req) : Cstruct.t =
      let body = Cstruct.create sizeof_req_get_section in
      let id = Stream.Multi_TS_ID.to_int32_pure params.stream_id in
      set_req_get_section_stream_id body id;
      set_req_get_section_table_id body params.table_id;
      Option.iter (set_req_get_section_section body) params.section;
      Option.iter (set_req_get_section_table_id_ext body) params.table_id_ext;
      Option.iter (set_req_get_section_id_ext_1 body) params.id_ext_1;
      Option.iter (set_req_get_section_id_ext_2 body) params.id_ext_2;
      to_complex_req ~request_id ~code ~body ()
  end

  module Get_t2mi_frame_seq = struct
    let code = 0x0306

    let serialize ({ request_id; params } : t2mi_frame_seq_req) : Cstruct.t =
      let body = Cstruct.create sizeof_req_get_t2mi_frame_seq in
      set_req_get_t2mi_frame_seq_time body params.seconds;
      to_complex_req ~request_id ~code ~body ()
  end

  module Get_jitter = struct
    let code = 0x0307

    let serialize ({ request_id; pointer } : jitter_req) : Cstruct.t =
      let body = Cstruct.create sizeof_req_get_jitter in
      set_req_get_jitter_ptr body pointer;
      to_complex_req ~request_id ~code ~body ()
  end

  module Get_ts_struct = struct
    let code = 0x0309

    let serialize ({ stream; request_id } : ts_struct_req) : Cstruct.t =
      let id = match stream with
        | `All -> 0xFFFF_FFFFl
        | `Single x -> Stream.Multi_TS_ID.to_int32_pure x in
      let body = Cstruct.create sizeof_req_get_ts_struct in
      let () = set_req_get_ts_struct_stream_id body id in
      to_complex_req ~request_id ~code ~body ()
  end

  module Get_bitrate = struct
    let code = 0x030A

    let serialize (request_id : int) : Cstruct.t =
      let body = Cstruct.create 0 in
      to_complex_req ~request_id ~code ~body ()
  end

  module Get_t2mi_info = struct
    let code = 0x030B

    let serialize ({ request_id; stream_id; _ } : t2mi_info_req) : Cstruct.t =
      let body = Cstruct.create sizeof_req_get_t2mi_info in
      set_req_get_t2mi_info_stream_id body stream_id;
      to_complex_req ~request_id ~code ~body ()
  end

  (** Senders *)

  let probe_req_to_string = function
    | Get_board_errors _ -> "Board errors"
    | Get_jitter _ -> "Jitter"
    | Get_ts_struct _ -> "TS structure"
    | Get_bitrates _ -> "TS bitrates"
    | Get_t2mi_info _ -> "T2-MI info"

  let probe_rsp_to_string = function
    | Board_errors _ -> "Board errors"
    | Bitrate _ -> "TS bitrates"
    | Struct _ -> "TS structures"
    | T2mi_info _ -> "T2-MI info"
    | Jitter _ -> "Jitter"

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_board_info ->
        Logs.debug (fun m -> m "requesting board info");
        Get_board_info.serialize ()
     | Get_board_mode ->
        Logs.debug (fun m -> m "requesting board mode");
        Get_board_mode.serialize ()
     | Get_t2mi_frame_seq x ->
        Logs.debug (fun m -> m "requesting t2mi frame sequence: %s"
                             @@ show_t2mi_frame_seq_req x);
        Get_t2mi_frame_seq.serialize x
     | Get_section x ->
        Logs.debug (fun m -> m "requesting section: %s" @@ show_section_req x);
        Get_section.serialize x)
    |> sender

  let send_event (type a) sender (msg : a probe_request) : unit Lwt.t =
    (match msg with
     | Get_board_errors id ->
        Logs.debug (fun m -> m "requesting board errors");
        Get_board_errors.serialize id
     | Get_jitter req ->
        Logs.debug (fun m -> m "requesting jitter: %s"
                             @@ show_jitter_req req);
        Get_jitter.serialize req
     | Get_ts_struct req ->
        Logs.debug (fun m -> m "requesting ts structs: %s"
                             @@ show_ts_struct_req req);
        Get_ts_struct.serialize req
     | Get_bitrates req ->
        Logs.debug (fun m -> m "requesting bitrates");
        Get_bitrate.serialize req
     | Get_t2mi_info req ->
        Logs.debug (fun m -> m "requesting t2mi info: %s"
                             @@ show_t2mi_info_req req);
        Get_t2mi_info.serialize req)
    |> sender

  let send_instant (type a) sender (msg : a instant_request) : unit Lwt.t =
    (match msg with
     | Reset ->
        Logs.debug (fun m -> m "requesting reset");
        to_complex_req ~code:0x0111 ~body:Cstruct.empty ()
     | Set_board_init x ->
        Logs.debug (fun m -> m "requesting board init");
        Set_board_init.serialize x
     | Set_board_mode (i, m) ->
        Logs.debug (fun m -> m "requesting board mode setup");
        Set_board_mode.serialize (i, m)
     | Set_jitter_mode x ->
        Logs.debug (fun m -> m "requesting set jitter mode setup");
        Set_jitter_mode.serialize x)
    |> sender

  let enqueue (type a) is_response state msgs (sender : sender)
        (msg : a request) (timeout : int) (exn : exn option) : a Lwt.t =
    (* no instant msgs *)
    match React.S.value state with
    | `No_response | `Init ->
       Lwt.fail (Failure "board is not responding")
    | `Fine ->
       let t, w = Lwt.wait () in
       let pred = function
         | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
         | l -> Option.(is_response msg l >|= Lwt.wakeup w) in
       let send = fun () -> send_msg sender msg in
       msgs := Await_queue.append !msgs { send; pred; timeout; exn };
       t

  let enqueue_instant (type a) state msgs sender
        (msg : a instant_request) : unit Lwt.t =
    let open Lwt.Infix in
    match React.S.value state with
    | `No_response | `Init ->
       Lwt.fail (Failure "board is not responding")
    | `Fine ->
       let t, w = Lwt.wait () in
       let send () =
         send_instant sender msg
         >>= Fun.(Lwt.return % Lwt.wakeup w) in
       let pred _ = Some () in
       msgs := Queue.append !msgs { send; pred; timeout = 0; exn = None };
       t

end
